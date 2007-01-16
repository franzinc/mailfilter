(in-package :user)

(eval-when (compile load eval)
  (require :aserve)
  (use-package :net.aserve.client)
  (require :shell)
  (use-package :excl.shell))

;; variables that may be redefined by the user
(defparameter *incorporating-mail* nil)
(defparameter *body-lines-to-read* 0)
(defparameter *track-dispatches* nil)
(defparameter *http-timeout* 15)
;; inbox-regexp is used in mailstatus
(defparameter *inbox-regexp* "^inbox-(.*)") 
;; used by mailstatus
(defparameter *mailstatus-inbox-folder-order* nil)
;; used by incfilter.   
(defparameter *logfilename* "~/incfilter.log") 

(defstruct msginfo
  num
  stream 
  user
  headers
  bodylines
  tos
  froms
  subject
  bhid
  dispatched
  dispatched-to
  class)
  
(defmacro defclassification ((infovar) &body body)
  `(defun classify-message (,infovar)
     (flet ((from-one-of (checklist)
	      (one-of-addrs-is-in-checklist-p (msginfo-froms minfo)
					      checklist))
	    (to-one-of (checklist)
	      (one-of-addrs-is-in-checklist-p (msginfo-tos minfo)
					      checklist))
	    (to-one-of-domain (domain-regexp checklist)
	      (one-of-addrs-is-in-checklist-p (msginfo-tos minfo)
					      checklist
					      :domain domain-regexp))
	    (add-header (name value)
	      (setf (msginfo-headers minfo)
		(nconc (msginfo-headers minfo) 
		       (list (cons name value))))))
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
       ((and firstline (=~ "^From\\s+(\\S+)" line))
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
;;  list of strings: recipients (To: and Cc: headers)
(defun scan-message (stream &key (maxbodylines 0))
  (with-spool-excursion (stream)
    (multiple-value-bind (headers bodylines envelope-sender)
	(scan-message-headers stream maxbodylines)
      (let (froms tmpheader)
	(setf tmpheader (get-header "From" headers))
	(if tmpheader
	    (setf froms (get-addr-list tmpheader)))
	
	(if envelope-sender
	    (pushnew envelope-sender froms :test #'equalp))
	
	(setf tmpheader (get-header "Return-Path" headers))
	(if tmpheader
	    (pushnew tmpheader froms :test #'equalp))
	
	(values headers 
		bodylines
		froms
		(get-header-recips headers))))))


(defun get-header-recips (headers)
  (let (res)
    (dolist (header headers)
      (if (or (equalp (car header) "To")
	      (equalp (car header) "Cc"))
	  (setf res (nconc 
		     res
		     (get-addr-list (unfold-header (cdr header)))))))
    res))

(defun unfold-header (h)
  (setf h (delete #\newline h)))
  

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

(defmacro match (addr check-against &key case-fold)
  `(match-re (concatenate 'string "^" ,check-against "$") ,addr
	     :case-fold ,case-fold))

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
	      (match addr check-against :case-fold t))
	     (t
	      ;; addr is qualified, so we qualify check-against and
	      ;; do an exact match
	      (match addr (concatenate 'string check-against "@" domain)
		     :case-fold t)))
       else (cond
	     ;; do exact match if check-against is qualified.. 
	     ;;  or if addr is unqualified (and, implicitly, check-against
	     ;;   is unqualified)
	     ((or atpos-check (null atpos-addr))
	      (match addr check-against :case-fold t))
	     (t
	      ;; check-against is unqualified but addr isn't.. just check
	      ;; the userpart
	      (match (subseq addr 0 atpos-addr) check-against
		     :case-fold t))))))
	     

(defun one-of-addrs-is-in-checklist-p (addrs check-list &key domain)
  (if (not (listp check-list))
      (setf check-list (list check-list)))
  (dolist (addr addrs)
    (if (member addr check-list
		:test #'(lambda (a c)
			  (address-matches-p a c :domain domain)))
	(return t))))
					

;; Since there is no \b (word boundary) regexp thingy.
(defmacro =~word (word-regexp string)
  (if (not (stringp word-regexp))
      (error "match-word: word-regexp should be a string"))
  (let* ((stringvar (gensym))
	 (nonwordchar "[^a-zA-Z0-9_]")
	 (r1 (concatenate 'string "^" word-regexp "$"))
	 (r2 (concatenate 'string "^" word-regexp nonwordchar))
	 (r3 (concatenate 'string nonwordchar word-regexp "$"))
	 (r4 (concatenate 'string nonwordchar word-regexp nonwordchar)))
    `(block nil
       (let ((,stringvar ,string))
	 (if (not (stringp ,string))
	     (return nil))
	 (if (=~ ,r1 ,stringvar)
	     (return t))
	 (if (=~ ,r2 ,stringvar)
	     (return t))
	 (if (=~ ,r3 ,stringvar)
	     (return t))
	 (if (=~ ,r4 ,stringvar)
	     (return t))
	 nil))))

(defun collect-bh-id (headers)
  (block nil
    (let ((thing (or (get-header "Bh-Id" headers)
		     (get-header "Subject" headers))))
      (when thing
	(if (=~word "(spr[0-9]+)" thing)
	    (return $1))
	(if (=~word "(bug[0-9]+)" thing)
	    (return $1))
	(if (=~word "(rfe[0-9]+)" thing)
	    (return $1))))))

  
#+ignore ;; not needed anymore
(defun dispatched-to-p (reportid user)
  (mp:with-timeout (*http-timeout* (error "bh.franz.com unresponsive")) 
    (let ((res 
	   (do-http-request 
	       (format
		nil 
		"http://bh.franz.com/dispatched-to-p?reportid=~a&user=~a"
		(net.aserve:uriencode-string reportid)
		(net.aserve:uriencode-string user)))))
      (string= res "t"))))

(defun dispatched-to (reportid)
  (mp:with-timeout (*http-timeout* (error "bh.franz.com unresponsive")) 
    (multiple-value-bind (res code)
	(do-http-request 
	    (format nil 
		    "http://bh.franz.com/dispatched-to?reportid=~a"
		    (net.aserve:uriencode-string reportid)))
      (if* (and (eql code 200) (stringp res))
	 then res
	 else nil))))

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

(defun load-user-config (homedir &key nocompile)
  (let* ((clfile
	  (or *config-file*
	      (concatenate 'string homedir "/" ".mailfilter.cl")))
	 (faslfile (merge-pathnames #p(:type "fasl") clfile)))
    (catch :retry-config-load
      (when (probe-file clfile)
	(unless nocompile
	  (compile-file-if-needed clfile :verbose nil :print nil))
	(if* nocompile
	   then (load clfile :verbose nil)
	   else (handler-case (load faslfile :verbose nil)
		  (excl::file-incompatible-fasl-error ()
		    (delete-file faslfile)
		    (throw :retry-config-load nil))))))))

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
	(tos (gensym)))
    `(let ((,spoolstreamvar ,spoolstream)
	   (,uservar ,user)
	   (,msgnum (1- ,initial-msgnum)))
       (loop
	 (incf ,msgnum)
	 (multiple-value-bind (,headers ,bodylines ,froms ,tos)
	     (scan-message ,spoolstreamvar :maxbodylines *body-lines-to-read*)
	   (if (null ,headers) ;; end of spool
	       (return)) 
	   (let ((,minfovar 
		  (make-msginfo 
		   :user ,uservar
		   :stream ,spoolstreamvar
		   :num ,msgnum
		   :headers ,headers :bodylines ,bodylines
		   :tos ,tos :froms ,froms
		   :subject (get-header "Subject" ,headers)
		   :bhid (collect-bh-id ,headers)
		   :class (get-header "Class" ,headers))))
	     
	     (when (and *track-dispatches* (msginfo-bhid ,minfovar))
	       (let ((,dispatched-to (dispatched-to (msginfo-bhid ,minfovar))))
		 (setf (msginfo-dispatched-to ,minfovar) ,dispatched-to)
		 (setf (msginfo-dispatched ,minfovar)
		   (and ,dispatched-to
			(string= ,uservar ,dispatched-to)))))
	     
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

(defun message-is-my-spr-p (minfo)
  (let ((bhid (msginfo-bhid minfo))
	(user (msginfo-user minfo)))
    (or
     (and bhid 
	  (=~ "^spr[0-9]+$" bhid)
	  (msginfo-dispatched minfo))
     
     (and (one-of-addrs-is-in-checklist-p (msginfo-froms minfo) "handler")
	  (one-of-addrs-is-in-checklist-p (msginfo-tos minfo) user)
	  (msginfo-subject minfo)
	  (=~ "^sprs\\.\\.\\." (msginfo-subject minfo))))))
