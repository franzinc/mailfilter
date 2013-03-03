
(in-package :user)

(eval-when (compile load eval)
  (require :aserve)
  (use-package :net.aserve.client)
  (require :shell)
  (use-package :excl.shell)
  (require :mime)
  (use-package :net.post-office))

;; variables that may be redefined by the user
(defparameter *incorporating-mail* nil)	; running from incfilter
(defparameter *mail-status* nil)	; running from mailstatus
(defparameter *body-lines-to-read* 0)
(defparameter *track-dispatches* nil)
(defparameter *http-timeout* 15)
;; inbox-regexp is used in mailstatus
(defparameter *inbox-regexp* "^inbox-(.*)") 
;; used by mailstatus
(defparameter *mailstatus-inbox-folder-order* nil)
;; used by incfilter.   
(defparameter *logfilename* "~/incfilter.log") 
;; Debugging option.  If set, it should be a string representing the 
;; path to an existing directory where files will be saved
(defparameter *save-spools* nil)
;; If true, log more information into *logfilename* during normal
;; operation.
(defparameter *verbose-logging* nil)

(defstruct msginfo
  num
  stream 
  user
  headers
  bodylines
  to
  cc
  recips
  froms
  subject
  bhid
  dispatched
  dispatched-to
  actions
  class
  references)

(defmacro precompile-re (re)
  `(load-time-value (compile-re ,re)))

(defmacro defclassification ((infovar) &body body)
  `(defun classify-message (,infovar)
     (flet ((from-one-of (checklist)
	      (one-of-addrs-is-in-checklist-p (msginfo-froms minfo)
					      checklist))
	    (cc-one-of (checklist)
	      (one-of-addrs-is-in-checklist-p (msginfo-cc minfo)
					      checklist))
	    (to-one-of (checklist &optional recips)
	      (one-of-addrs-is-in-checklist-p
	       (or recips (msginfo-recips minfo))
	       checklist))
	    (to-one-of-domain (domain-regexp checklist &optional recips)
	      (one-of-addrs-is-in-checklist-p
	       (or recips (msginfo-recips minfo))
	       checklist
	       :domain domain-regexp))
	    (add-header (name value)
	      (setf (msginfo-headers minfo)
		(nconc (msginfo-headers minfo) 
		       (list (cons name value))))))
       (declare (ignorable #'from-one-of #'to-one-of #'cc-one-of
			   #'to-one-of-domain #'add-header))
       ;; putting these into a macrolet allows the the regexps to be
       ;; compiled at the time the .mailfilter.cl is compiled.
       (macrolet
	   ((subject-match (regexp)
	      `(and (msginfo-subject minfo)
		    (match-re ,regexp (msginfo-subject minfo))))
	    (class-match (regexp)
	      `(and (msginfo-class minfo)
		    (match-re ,regexp (msginfo-class minfo)))))
	 (block nil ,@body)))))
     
(defun get-mhpath (homedir)
  (let (line)
    (if (null homedir)
	(error "Environment variable HOME not set."))
    (with-open-file (f (concatenate 'string homedir "/.mh_profile"))
      (while (setf line (read-line f nil nil))
	(multiple-value-bind (found whole path)
	    (match-re "^Path:\\s*(.*)$" line)
	  (declare (ignore whole))
	  (when found
	    (return-from get-mhpath 
	      (concatenate 'string homedir "/" path))))))
    (error "Couldn't find Path: in ~/.mh_profile")))

;; By default, leaves the file position unchanged.. this is so
;; people can safely use this in their .mailfilter.cl files.
(defun copy-message-to-stream (source dest headers &key (save-pos t)
							just-body)
  (let (line)
    (if save-pos
	(push-spool-stream-mark source))
    
    ;; skips the headers (and the blank line that follows them)
    (scan-message-headers source 0)
    
    (when (and dest (not just-body))
      ;; Write out replacement headers.
      (dolist (h headers)
	(if (eq (cdr h) :from-leader)
	    (write-line (car h) dest)
	  (format dest "~A: ~A~%" (car h) (cdr h))))
      (terpri dest)) ;; blank line 
    
    ;; read the body now
    (while (stringp (setf line (next-message-line source)))
      (if dest
	  (write-line line dest)))
    (if save-pos
	(pop-spool-stream-mark source))))

(defun skip-message (stream)
  (copy-message-to-stream stream nil nil :save-pos nil))

(defmacro get-header (header headers &key null-string)
  (let ((res (gensym)))
    `(let ((,res (cdr (assoc ,header ,headers :test #'equalp))))
       (if ,res
	   ,res
	 (if ,null-string
	     ""
	   nil)))))

(defun get-headers (header headers &aux (res '()))
  (dolist (h headers)
    (when (equalp header (car h))
      (push (cdr h) res)))
  (when res (nreverse res)))

;; returns:  (values headers bodylines envelope-sender)))
(defun scan-message-headers (stream maxbodylines)
  (let ((firstline t)
	line headers envelope-sender bodylines eom)
    (loop
      (setf line (next-message-line stream :first firstline))
      (when (eof-or-eom-p line)
	(setf eom t)
	(return)) ;; break from loop
      (cond
       ((string= line "") ;; end of headers
	(return)) ;; break from loop
       ;; Check for message that begins with "From "
       ((and firstline (=~ (precompile-re "^From\\s+(\\S+)") line))
	(setf envelope-sender $1)
	;; hack since we need the "From " leader when writing the
	;; message out to another file
	(push (cons line :from-leader) headers))
       ((excl::whitespace-char-p (schar line 0)) ;; continuation header
	(setf (cdr (first headers))
	  (format nil "~A~%~A" (cdr (first headers)) line)))
       (t
	(multiple-value-bind (found whole name data)
	    (match-re "^(\\S+)\\s*:\\s*(.*)$" line)
	  (declare (ignore whole))
	  (if* found
	     then
		  (push (cons name data) headers)
	     else
		  ;; something strange..  cancel header processing
		  (when (> maxbodylines 0)
		    (push line bodylines)
		    (decf maxbodylines))
		  (return)))))
      (setf firstline nil))

    ;; outside of loop.. collect some body lines.
    (when (not eom)
      (while (and (> maxbodylines 0)
		  (stringp (setf line (next-message-line stream))))
	(push line bodylines)
	(decf maxbodylines)))

    (values (nreverse headers) (nreverse bodylines) envelope-sender)))
    
    

;; returns values:
;;  headers alist
;;  list of body lines
;;  list of strings: senders (envelope sender and address in From:)
;;  list of strings: recipients from To: header
;;  list of strings: recipients from Cc: header
;;  list of strings: recipients from Resent-To or Resent-CC

(defun scan-message (stream &key (maxbodylines 0))
  (with-spool-excursion (stream)
    (multiple-value-bind (headers bodylines envelope-sender)
	(scan-message-headers stream maxbodylines)
      (let (froms tmpheader)
	(if (setf tmpheader (get-header "From" headers))
	    (setf froms (get-addr-list tmpheader)))
	
	(if envelope-sender
	    (pushnew envelope-sender froms :test #'equalp))
	
	(if (setf tmpheader (get-header "Return-Path" headers))
	    (pushnew tmpheader froms :test #'equalp))
	
	(if (setf tmpheader (get-header "Return-Path" headers))
	    (pushnew tmpheader froms :test #'equalp))
	
	(values headers 
		bodylines
		froms
		(get-header-recips headers "To")
		(get-header-recips headers "Cc")
		(append (get-header-recips headers "Resent-To")
			(get-header-recips headers "Resent-Cc")))))))


(defun get-header-recips (headers what &aux (res '()))
  (dolist (header headers res)
    (when (equalp (car header) what)
      (setq res (nconc res (get-addr-list (unfold-header (cdr header))))))))

(defun unfold-header (h)
  (setf h (remove #\newline h)))
  

;; In all of these functions, 'addr' is an email address
;; found in the message.  'check-against' is the address
;; provided by the mailfilter user.

;; Two modes:
;;   :domain nil
;;    check-against may be unqualified or qualified.
;;    if qualified, a full match is done.  If unqualified,
;;    a userpart match is done.

;;   :domain specified
;;    check-against must be a userpart only.  If 'addr' is
;;    qualified, the domain part must be equal to 'domain' and
;;    userparts must match.  If unqualified, userpart matching is done.

;; check-against is a regular expression.

;; below, "exact" means full address regexp match, rather than
;; just matching on a piece of the address.

;; This is to avoid repeated regular expression compiles.
(defparameter *re-cache* (make-hash-table :test #'equal))

(defun match (addr check-against)
  (let ((re (gethash check-against *re-cache*)))
    (if* (null re)
       then (setf re (compile-re (concatenate 'string "^" check-against "$")
				 :case-fold t :return nil))
	    (setf (gethash check-against *re-cache*) re))
    (match-re re addr :case-fold t :return nil)))
    
(defun address-matches-p (addr check-against &key domain)
  (let ((atpos-addr (position #\@ addr))
	(atpos-check (position #\@ check-against)))
    (if* domain
       then (cond 
	     (atpos-check
	      (error "~A has a domain part.  Expected just a user part" 
		     check-against))
	     ((null atpos-addr)
	      ;; addr is unqualified, so do exact matching
	      (match addr check-against))
	     (t
	      ;; addr is qualified, so we qualify check-against and
	      ;; do an exact match
	      (match addr (concatenate 'string check-against "@" domain))))
       else (cond
	     ;; do exact match if check-against is qualified.. 
	     ;;  or if addr is unqualified (and, implicitly, check-against
	     ;;   is unqualified)
	     ((or atpos-check (null atpos-addr))
	      (match addr check-against))
	     (t
	      ;; check-against is unqualified but addr isn't.. just check
	      ;; the userpart
	      (match (subseq addr 0 atpos-addr) check-against))))))
	     

(defun one-of-addrs-is-in-checklist-p (addrs check-list &key domain)
  (if (not (listp check-list))
      (setf check-list (list check-list)))
  (dolist (addr addrs)
    (if (member addr check-list
		:test #'(lambda (a c)
			  (address-matches-p a c :domain domain)))
	(return t))))

(defun collect-bh-id (headers)
  (block nil
    (let ((subj (get-header "Subject" headers))
	  (id (get-header "Bh-Id" headers)))
      (when id
	;; Always trust the Bh-Id
	(return id))
      (when subj
	;; Only trust a few types of things in the subject
	(multiple-value-bind (found whole id)
	    (match-re "\\b((?:spr|bug|rfe|bhrfe|bhbug)\\d+)\\b" subj)
	  (declare (ignore whole))
	  (if found
	      id))))))

(defun dispatched-to (reportid)
  (mp:with-timeout (*http-timeout* (error "bh.franz.com unresponsive")) 
    (multiple-value-bind (res code)
	(do-http-request 
	    (format nil 
		    "http://bh.franz.com/getinfo?reportid=~a"
		    (net.aserve:uriencode-string reportid)))
      (when (and (eql code 200)
		 (stringp res)
		 (string/= "" res))
	;; Don't use destructuring-bind here since /getinfo is documented
	;; to possible return more entries in the list in the future.
	(let ((info (read-from-string res)))
	  (values (first info)		; dispatched-to
		  (second info)		; open actions list 
		  (third info)))))))	; references information 
	

(defun clean-msgid (msgid)
  (string-trim '(#\newline #\return #\space #\tab) msgid))

(defun bh-appended-p (reportid msgid)
  (mp:with-timeout (*http-timeout* (error "bh.franz.com unresponsive")) 
    (let ((res 
	   (do-http-request 
	       (format nil 
		       "http://bh.franz.com/appended-p?reportid=~a&msgid=~a"
		       (net.aserve:uriencode-string reportid)
		       (net.aserve:uriencode-string (clean-msgid msgid))))))
      (string= res "t"))))


(defvar *config-file* nil)

(defun load-user-config (homedir &key (compile t) if-changed)
  (let* ((clfile
	  (or *config-file*
	      (concatenate 'string homedir "/" ".mailfilter.cl")))
	 (faslfile (merge-pathnames #p(:type "fasl") clfile)))
    (tagbody
     retry-config-load
      (when (probe-file clfile)
	(when (or (null if-changed)
		  (and compile (not (probe-file faslfile)))
		  (and compile (> (file-write-date clfile)
				  (file-write-date faslfile)))
		  (not compile))
;;;;(format t "~&;loading config~%")
	  (if* compile
	     then (compile-file-if-needed clfile :verbose nil :print nil)
		  (handler-case (load faslfile :verbose nil)
		    (excl::file-incompatible-fasl-error ()
		      (delete-file faslfile)
		      (go retry-config-load)))
	     else (load clfile :verbose nil))
	  ;; non-nil means we did something.
	  t)))))

(defmacro with-each-message ((spoolstream classificationvar minfovar user
			      &key (initial-msgnum 0))
			     &body body)
  (let ((msgnum (gensym))
	(spoolstreamvar (gensym))
	(uservar (gensym))
	(headers (gensym))
	(bodylines (gensym))
	(froms (gensym))
	(dispatched-to (gensym))
	(actions (gensym))
	(references (gensym))
	(to (gensym))
	(cc (gensym))
	(resent-tos (gensym))
	)
    `(let ((,spoolstreamvar ,spoolstream)
	   (,uservar ,user)
	   (,msgnum (1- ,initial-msgnum)))
       (loop
	 (incf ,msgnum)
	 (multiple-value-bind (,headers ,bodylines ,froms ,to ,cc ,resent-tos)
	     (scan-message ,spoolstreamvar :maxbodylines *body-lines-to-read*)
	   (if (null ,headers) ;; end of spool
	       (return)) 
	   (let ((,minfovar 
		  (make-msginfo 
		   :user ,uservar
		   :stream ,spoolstreamvar
		   :num ,msgnum
		   :headers ,headers :bodylines ,bodylines
		   :to ,to
		   :cc ,cc
		   :recips (append ,to ,cc ,resent-tos)
		   :froms ,froms
		   :subject (let ((sub (get-header "Subject" ,headers)))
			      (when sub
				(let ((decoded-sub
				       (ignore-errors
					(decode-header-text sub))))
				  (if* decoded-sub
				     thenret
				     else sub))))
		   :bhid (collect-bh-id ,headers)
		   :class (get-header "Class" ,headers))))
	     
	     (when (and *track-dispatches* (msginfo-bhid ,minfovar))
	       (multiple-value-bind (,dispatched-to ,actions ,references)
		   (dispatched-to (msginfo-bhid ,minfovar))
		 (setf (msginfo-dispatched-to ,minfovar) ,dispatched-to)
		 (setf (msginfo-actions ,minfovar) ,actions)
		 (setf (msginfo-dispatched ,minfovar)
		   (and ,dispatched-to
			(string= ,uservar ,dispatched-to)))
		 (setf (msginfo-references ,minfovar) ,references)))

	     #+ignore
	     (let ((,classificationvar 
		    (if (fboundp 'classify-message)
			(handler-bind
			    ((error
			      (lambda (c)
				(with-standard-io-syntax
				  (let ((*print-readably* nil)
					(*print-miser-width* 40)
					(*print-pretty* t)
					(top-level:*zoom-print-circle* t)
					(top-level:*zoom-print-level* nil)
					(top-level:*zoom-print-length* nil)
					(*standard-output* *terminal-io*))
				    (format t "Error: ~a~%" c)
				    (top-level:do-command "zoom"
				      :from-read-eval-print-loop nil
				      :count t :all t))))))
			  (funcall 'classify-message ,minfovar))
		      "+inbox")))
	       ,@body)

	     (let ((,classificationvar 
		    (if (fboundp 'classify-message)
			(funcall 'classify-message ,minfovar)
		      "+inbox")))
	       ,@body)))))))

(defmacro with-single-message ((spoolstream classificationvar minfovar user
				&key msgnum)
			       &body body)
  `(with-each-message (,spoolstream ,classificationvar ,minfovar ,user
				    :initial-msgnum ,msgnum)
     ,@body
     (file-position ,spoolstream (file-length ,spoolstream))))


(defmacro with-tmp-dir ((dir) &body body)
  (let ((dirsym (gensym)))
    `(let ((,dirsym ,dir))
       (unwind-protect
	   (progn
	     (make-directory ,dirsym #o700)
	     ,@body)
	 ;; cleanup
	 (delete-directory-and-files ,dirsym :if-does-not-exist :ignore)))))

;; refs is a list of conses where the car is the report id and the cdr
;; is a list of (dispatched-to open-action-user1 open-action-user2 ...)
(defun one-of-references-is-my-spr (refs user)
  (dolist (ref refs)
    (let ((reportid (car ref)))
      (when (and (match-re "^spr" reportid)
		 (member user (cdr ref) :test #'string=))
	(return reportid)))))

(defun message-is-my-spr-p (minfo &key check-for-spr-references)
  (let ((bhid (msginfo-bhid minfo))
	(user (msginfo-user minfo)))
    (when (and bhid (not (match-re "^spr\\d+$" bhid :return nil)))
      (return-from message-is-my-spr-p nil))
    (or
     (and bhid
	  (msginfo-dispatched minfo))
     
     (and (one-of-addrs-is-in-checklist-p (msginfo-froms minfo) "handler")
	  (one-of-addrs-is-in-checklist-p (msginfo-recips minfo) user)
	  (msginfo-subject minfo)
	  (match-re "^sprs\\.\\.\\." (msginfo-subject minfo) :return nil))
     
     (and (msginfo-actions minfo)
	  (member user (msginfo-actions minfo) :test #'string=))
     
     (and check-for-spr-references
	  (one-of-references-is-my-spr (msginfo-references minfo) user)))))

(defun make-timestamped-filename (dir basename)
  (format nil "~a/~a.~a" 
	  dir
	  basename
	  (locale-format-time
	   nil (get-universal-time) nil nil nil "%Y-%m-%d-%H:%M:%S")))
