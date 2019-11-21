
(in-package :user)

(defun main (&rest args)
  (setq *load-print* nil)
  (setq *load-verbose* nil)
  ;; setup the command line args tables
  (setf *command-line-options*
	(make-hash-table :test #'equal))

  (let* ((user (getenv "USER"))
	 (home (getenv "HOME"))
	 (*mail-status* t)
	 (spoolfile (guess-spool-filename user))
	 (dotlock t)
	 (prgname (pop args))
	 (interval 30) ;; seconds
	 (boxes (make-hash-table :test #'equal))
	 (usage (format nil "usage: ~A [options]" prgname))
	 show-time once debug long show-unread print-separators
	 cleanup-conversations)
    (if (null user)
	(error "Environment variable USER is not set"))
    (if (null home)
	(error "Environment variable HOME is not set"))
    (if (null spoolfile)
	(error "Couldn't determine your spool filename"))

    (register-c-l-o "-c"
		    "Defaults to '~/.mailfilter.cl'."
		    (lambda ()
		      (setf *config-file* (pop args))
		      (or (probe-file *config-file*)
			  (error "Config file ~a does not exist." 
				 *config-file*)))
		    "CONFIG_FILE")

    (register-c-l-o "-t"
		    (format nil "defaults to '~A' seconds." interval)
		    (lambda ()
		      (if (null args)
			  (error "~A: missing argument to -t" prgname))
		      (setf interval (parse-integer (pop args))))
		    "INTERVAL")

    (register-c-l-o "-T"
		    "enable showing the spool check time."
		    (lambda ()
		      (setf show-time t)))

    (register-c-l-o "-o"
		    "run once and exit."
		    (lambda ()
		      (setf once t)))

    (register-c-l-o "-d"
		    "enable debugging."
		    (lambda ()
		      (setf debug t)))

    (register-c-l-o "-l"
		    "enable long output (implies -o)"
		    (lambda ()
		      (setf long t)
		      (call-command-line-option-action "-o")))
    
    (register-c-l-o "-L"
		    "enable scanning long output (implies -o)"
		    (lambda (&aux temp)
		      (when (null args)
			(error "mailstatus: missing argument to -L"))
		      (setf long (pop args))
		      (if* (string= "all" long)
			 then (setq long 999)
		       elseif (null (setq temp
				      (parse-integer long :junk-allowed nil)))
			 then (error "Couldn't parse -L number: ~a." long)
			 else (setq long temp))
		      (call-command-line-option-action "-o")))
    
    (register-c-l-o "-cleanup"
		    "clean out old conversations"
		    (lambda ()
		      (setq cleanup-conversations t)))        
    
    (register-c-l-o "-u"
		    "enable showing of unread mail."
		    (lambda ()
		      (setf show-unread t)))

    (register-c-l-o "-p"
		    "enable printing of separators."
		    (lambda ()
		      (setf print-separators #\,)))

    (register-c-l-o "-file"
		    (format nil "defaults to '~A'" spoolfile)
		    (lambda ()
		      (when (null args)
			(error "mailstatus: missing argument to -file"))
		      (setf spoolfile (pop args))
		      (setf dotlock nil))
		    "SPOOLFILE")

    (register-c-l-o "-h"
		    "Print this help menu and exit."
		    (lambda ()
		      (display-help usage)
		      (error "Exiting..")))

    (register-c-l-o "-help"
		    "The same as -h"
		    "-h")

    (register-c-l-o "-version"
		    "Prints the version and exits."
		    (lambda ()
		      (error "~A: version ~A"
			     prgname
			     *mailfilter-version*)))

    (while args
      (call-command-line-option-action (pop args)
	(lambda (a)
	  (display-help usage)
	  (error "~A: Unexpected command line argument: ~A" 
		 prgname
		 a))))

    (load-user-config home)
    
    ;; bleh
    (setf *default-pathname-defaults* (pathname (chdir (get-mhpath home))))

    (labels
	((doit (&aux configuration-changed (total 0)
		     inboxes saw-unknown-marker)
	   (when cleanup-conversations
	     (remove-old-conversations)
	     (return-from doit))
	   
	   (loop ;; interval loop
	     (with-output-to-string (output)
	       (when show-time (output-time output))
	
	       ;; the 'boxes' hash table holds entries in the following form:
	       ;; +folder ==> (old new modification-time)
	       ;; (old is not used for +inbox)

	       (get-main-inbox-information long spoolfile user boxes dotlock
					   configuration-changed)
	
	       (let* ((info (gethash "+inbox" boxes))
		      (oinbox (first info))
		      (ninbox (second info))
		      (uinbox (count-unread-messages "inbox/.mh_sequences")))
		 (if* long
		    then (incf total (+ ninbox oinbox))
			 (print-long-summary output "inbox"
					     print-separators
					     ninbox uinbox oinbox
					     :scan (if (numberp long) long))
		  elseif (> ninbox 0)
		    then (format output " ~D+" ninbox)))
	
	       ;; inbox ==> (shortname longname fullname)
	       (multiple-value-setq (inboxes saw-unknown-marker)
		 (make-list-of-inboxes))
	       
	       (dolist (inbox inboxes)
		 (cond
		  ((symbolp inbox)
		   (when long
		     (if* (eq :newline inbox)
			then (format output "~%")
			else (error "bad inbox: ~s." inbox))))
		  ((and (consp inbox) (symbolp (car inbox)))
		   (when long
		     (if* (or (eq :header (first inbox))
			      (and (eq :header-if-unknown (first inbox))
				   saw-unknown-marker))
			then (format output "; ~a~%" (second inbox))
			else (error "bad inbox: ~s." inbox))))
		  (t
		   (multiple-value-bind (old new unread important)
		       (get-other-inbox-information inbox boxes
						    configuration-changed)
		     (when (> (+ old new unread) 0)
		       (if* long
			  then (incf total (+ old new))
			       (print-long-summary output (second inbox)
						   print-separators
						   new unread old
						   :important important
						   :scan (if (numberp long)
							     long))
			  else (print-short-summary output (first inbox)
						    show-unread new unread
						    old)))))))
	       
	       (when (and long (> total 0))
		 (format output "~%; TOTAL: ~s~%" total))

	       (let ((string (get-output-stream-string output)))
		 (when (string= "" string)
		   ;; make sure there is some output if there is no mail
		   ;; and no inboxes... this ensures that the previous
		   ;; status line will be cleared and gives an indication
		   ;; that mailstatus did not crash.  :)
		   (setq string "<no mail>"))
		 (write-string string)
		 (finish-output)))

	     (when once (return))
	     
	     (sleep interval)
	     
	     ;; In case it changed
	     (setq configuration-changed
	       (load-user-config home :if-changed t)))))
      (if* debug
	 then (handler-bind
		  ((error
		    (lambda (e)
		      (with-standard-io-syntax
			(let ((*print-readably* nil)
			      (*print-miser-width* 40)
			      (*print-pretty* t)
			      (tpl:*zoom-print-circle* t)
			      (tpl:*zoom-print-level* nil)
			      (tpl:*zoom-print-length* nil))
			  (ignore-errors ;prevent recursion
			   (format *terminal-io* "~
~@<An unhandled error condition has been signalled:~3I ~a~I~:@>~%~%"
				   e))
			  (ignore-errors ;prevent recursion
			   (tpl:do-command "zoom"
			     :from-read-eval-print-loop nil
			     :count t :all t)))))))
		(doit))
	 else (doit)))))

(defun print-short-summary (s name show-unread new unread old)
  (cond ((and (> new 0) (> old 0))
	 (format s " ~D>~A:~D~@[[~a]~]" new name old
		 (when (and show-unread (> unread 0)) unread)))
	((> new 0) (format s " ~D>~A" new name))
	((> old 0)
	 (format s " ~A:~D~@[[~a]~]" name old
		 (when (and show-unread (> unread 0)) unread)))))

(defun print-long-summary (s name print-separators new unread old 
			   &key important scan)
  (if* scan
     then (multiple-value-bind (stdout stderr code)
	      (excl.osi:command-output
	       (format nil "scan +~a~@[~* -reverse~] -width 132 last:~a"
		       name
		       (/= scan 999)
		       scan)
	       :whole t)
	    (format s "~%+~a~%" name)
	    (if* (and (/= 0 code)
		      (match-re "scan: no messages in " stderr :return nil))
	       thenret ;; skip
	     elseif stdout
	       then (princ stdout s)
	       else (error "stdout=~s stderr=~s code=~s" stdout stderr
			   code)))
     else (format s "+~30a~@[~c~]~10a~@[~c~]~13a~@[~c~]~10a~a~%"
		  name
		  print-separators
		  (if (> new 0) (format nil "~3d new" new) "")
		  print-separators
		  (if (> unread 0)
		      (format nil "~4d unread" unread)
		    "")
		  print-separators
		  (if (> old 0) (format nil "~4d old" old) "")
		  (if important "*****" ""))))

(defun ensure-box (box boxes)
  (if (null (gethash box boxes))
      (zero-box box boxes)))

(defun zero-box (box boxes)
  (setf (gethash box boxes) (list 0 0 0)))

(defun reset-new-count-for-boxes (boxes)
  (maphash #'(lambda (key value)
	       (declare (ignore key))
	       (setf (second value) 0))
	   boxes))

(defun make-list-of-inboxes ()
  ;; make a list of these items, one for each inbox
  ;;  (shortname longname mh-e-name)
  ;; where shortname is name without "inbox-"
  ;;   and longname  is the given name
  ;;   and mh-e-name is the logname prefixed with "+"
  ;; and return that list sorted by the specification in
  ;; *mailstatus-inbox-folder-order*.
  (let (inboxes)
    (dolist (path (directory "."))
      (let ((longname (enough-namestring path)))
	(multiple-value-bind (found whole shortname)
	    (match-re *inbox-regexp* longname)
	  (declare (ignore whole))
	  (when found
	    (push (list shortname longname
			(concatenate 'string "+" longname))
		  inboxes)))))
    (if* *mailstatus-inbox-folder-order*
       then (sort-inboxes inboxes)
       else ;; user doesn't care, use directory order
	    inboxes)))

(defun sort-inboxes (inboxes)
  ;; return a sorted list.  The 2nd value is non-nil if the :unknown marker
  ;; was seen.
  (let ((unknown-placeholder-seen
	 ;; non-nil if :unknown seen in *mailstatus-inbox-folder-order*
	 nil)
	(before-unknown-inboxes
	 ;; the inboxes seen before :unknown
	 '())
	(after-unknown-inboxes
	 ;; the inboxes seen after :unknown
	 '())
	this)

    (dolist (template *mailstatus-inbox-folder-order*)
      (if* (eq :unknown template)
	 then (setq unknown-placeholder-seen t)
	      (setq this nil)
       elseif (consp template)
	 then (setq this template)
       elseif (or (eq :newline template)
		  (consp template))
	 then (setq this template)
       elseif (setq this (find template inboxes :key #'first :test #'string=))
	 then (setq inboxes
		;; remove this one from inboxes so at the end it's just the
		;; unknown ones.
		(delete template inboxes :key #'first :test #'string=))
	 else (setq this nil))
      (when this
	(if* unknown-placeholder-seen
	   then (push this after-unknown-inboxes)
	   else (push this before-unknown-inboxes))))
    
    (when inboxes
      (setq inboxes (sort inboxes #'string< :key #'first)))

    (if* before-unknown-inboxes
       then (setq before-unknown-inboxes (nreverse before-unknown-inboxes))
    
	    (if* after-unknown-inboxes
	       then (setq after-unknown-inboxes
		      (nreverse after-unknown-inboxes))
		    
		    (values (nconc before-unknown-inboxes
				   inboxes
				   after-unknown-inboxes)
			    ;; indicate there was an :unknown marker seen
			    t)
	     elseif inboxes
	       then ;; No :unknown marker, so put the unknown ones at the
		    ;; top, like before :unknown existed.
		    (nconc inboxes before-unknown-inboxes)
	       else before-unknown-inboxes)
     elseif inboxes
       then inboxes)))


(defun output-time (stream)
  (multiple-value-bind (sec min hour)
      (get-decoded-time)
    (declare (ignore sec))
    (if (= hour 0)
	(setf hour 12))
    (if (> hour 12)
	(decf hour 12))
    (format stream " ~D:~2,'0d" hour min)))

;; Gets the new count for other inboxes as a side effect

(defun get-main-inbox-information (long spoolfile user boxes dotlock
				   ignore-cache)
  (when ignore-cache (clrhash boxes))
  
  (ensure-box "+inbox" boxes)
  
  (with-spool-file (f spoolfile :dotlock dotlock)
    (when (eq f :no-spool)
      (zero-box "+inbox" boxes)
      (reset-new-count-for-boxes boxes))

    (let ((new-incoming
	   (and (streamp f) 
		 (or ignore-cache
		     (> (file-write-date f)
			(third (gethash "+inbox" boxes)))))))
      
      (when (or long new-incoming)
	(setf (first (gethash "+inbox" boxes))
	  (count-if (lambda (p)
		      (match-re "/[0-9]+$" (enough-namestring p)))
		    (directory "inbox/"))))
    
      (when new-incoming
	(reset-new-count-for-boxes boxes)
      
	(setf (third (gethash "+inbox" boxes)) (file-write-date f))
      
	(with-each-message (f box minfo user)
	  (ensure-box box boxes)
	  (incf (second (gethash box boxes)))
	  (skip-message f))))))
	

;; returns oldcount and newcount.
(defun get-other-inbox-information (inbox boxes ignore-cache)
  (when ignore-cache (clrhash boxes))
  (let (dir old new unread important)
    (destructuring-bind (shortname longname fullname) inbox
      (declare (ignore shortname))
      
      (setq dir (concatenate 'string longname "/"))
      
      (ensure-box fullname boxes)
      
      (setf new (second (gethash fullname boxes)))
      
      (when (or ignore-cache
		(> (file-write-date longname)
		   (third (gethash fullname boxes))))
	(setf (third (gethash fullname boxes)) 
	  (file-write-date longname))
	
	(setf (first (gethash fullname boxes))
	  (count-if (lambda (p)
		      (match-re "/[0-9]+$" (enough-namestring p)))
		    (directory dir))))
      
      (setq unread
	(count-unread-messages (merge-pathnames ".mh_sequences" dir)))
      
      (setq important
	(important-sequence-exists-p (merge-pathnames ".mh_sequences" dir)))
      
      (setf old (first (gethash fullname boxes))))
    (values old new unread important)))

(defun count-unread-messages (mh-seq &aux (unread 0))
  (dolist (unseen (retrieve-sequence "unseen" mh-seq) unread)
    (incf unread (count-range unseen))))

(defun important-sequence-exists-p (mh-seq)
  (when (retrieve-sequence "important" mh-seq)
    t))

(defun retrieve-sequence (name file)
  (when (probe-file file)
    (with-open-file (s file :direction :input)
      (let (line)
	(loop
	  (setq line (read-line s nil s))
	  (when (eq s line) (return))
	  (multiple-value-bind (found ignore1 seq)
	      (match-re (format nil "^~a: (.*)$" name) line)
	    (declare (ignore ignore1))
	    (when found
	      (return (delimited-string-to-list seq #\space)))))))))

(defun count-range (string)
  (multiple-value-bind (found ignore1 start ignore2 end)
      (match-re "^([0-9]+)(-([0-9]+))?$" string)
    (declare (ignore ignore1 ignore2))
    (when found
      (setq start (parse-integer start))
      (if* end
	 then (setq end (parse-integer end))
	      (1+ (- end start))
	 else 1))))

;;; For Gnu Emacs.
;;; Local Variables: ***
;;; eval: (put 'call-command-line-option-action 'fi:common-lisp-indent-hook 1) ***
;;; End: ***
