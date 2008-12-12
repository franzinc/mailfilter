;; $Id: mailstatus.cl,v 1.14 2008/12/12 06:22:30 layer Exp $

(in-package :user)

(defvar *long* nil)

(defun main (&rest args)
  (let* ((user (getenv "USER"))
	 (home (getenv "HOME"))
	 (spoolfile (guess-spool-filename user))
	 (dotlock t)
	 (prgname (pop args))
	 (interval 30) ;; seconds
	 (boxes (make-hash-table :test #'equal))
	 show-time once debug long show-unread print-separators)
    (if (null user)
	(error "Environment variable USER is not set"))
    (if (null home)
	(error "Environment variable HOME is not set"))
    (if (null spoolfile)
	(error "Couldn't determine your spool filename"))
    
    (while args
      (cond 
       ((string= (first args) "-c")
	(pop args)
	(setq *config-file* (first args))
	(or (probe-file *config-file*)
	    (error "Config file ~a does not exist." *config-file*))
	(pop args))
       ((string= (first args) "-t")
	(pop args)
	(if (null args)
	    (error "~A: missing argument to -t" prgname))
	(setf interval (parse-integer (pop args))))
       ((string= (first args) "-T")
	(pop args)
	(setf show-time t))
       ((string= (first args) "-o")
	(pop args)
	(setf once t))
       ((string= (first args) "-d")
	(pop args)
	(setf debug t))
       ((string= (first args) "-l")
	(pop args)
	(setf long t)
	(setf *long* t)
	(setf once t))
       ((string= (first args) "-u")
	(pop args)
	(setf show-unread t))
       ((string= (first args) "-p")
	(pop args)
	(setf print-separators #\,))
       ((string= (first args) "-file")
	(pop args)
	(when (null args)
	  (error "mailstatus: missing argument to -file"))
	(setf spoolfile (pop args))
	(setf dotlock nil))
       (t (error "~A: Unexpected command line argument: ~A" (first args)))))

    (load-user-config home)
    
    ;; bleh
    (setf *default-pathname-defaults* (pathname (chdir (get-mhpath home))))

    (labels
	((doit (&aux configuration-changed)
	   (loop ;; interval loop
	     (with-output-to-string (output)
	       (when show-time (output-time output))
	
	       ;; the 'boxes' hash table holds entries in the following form:
	       ;; +folder ==> (old new modification-time)
	       ;; (old is not used for +inbox)

	       (get-main-inbox-information spoolfile user boxes dotlock
					   configuration-changed)
	
	       (let* ((info (gethash "+inbox" boxes))
		      (oinbox (first info))
		      (ninbox (second info))
		      (uinbox (count-unread-messages "inbox/.mh_sequences")))
		 (if* long
		    then (print-long-summary output "inbox"
					     print-separators
					     ninbox uinbox oinbox)
		  elseif (> ninbox 0)
		    then (format output " ~D+" ninbox)))
	
	       ;; inbox ==> (shortname longname fullname)
	       (dolist (inbox (make-list-of-inboxes))
		 (multiple-value-bind (old new unread)
		     (get-other-inbox-information inbox boxes
						  configuration-changed)
		   (when (> (+ old new unread) 0)
		     (if* long
			then (print-long-summary output (second inbox)
						 print-separators
						 new unread old)
			else (print-short-summary output (first inbox)
						  show-unread new unread
						  old)))))

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

(defun print-long-summary (s name print-separators new unread old)
  (format s "+~15a~@[~c~]~10a~@[~c~]~13a~@[~c~]~10a~%"
	  name
	  print-separators
	  (if (> new 0) (format nil "~3d new" new) "")
	  print-separators
	  (if (> unread 0)
	      (format nil "~4d unread" unread)
	    "")
	  print-separators
	  (if (> old 0) (format nil "~4d old" old) "")))

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

;; make a list of
;;  (shortname longname fullname) lists
(defun make-list-of-inboxes ()
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
    (sort-inboxes inboxes)))

(defun sort-inboxes (inboxes)
  (let (sorted entry sort-order)
    (setq sort-order (if* *long*
			then (or *mailstatus-long-format*
				 *mailstatus-inbox-folder-order*)
			else *mailstatus-inbox-folder-order*))
    (setq inboxes (sort inboxes #'string< :key #'first))
    (dolist (inbox sort-order)
      (when (setf entry (find inbox inboxes :key #'first :test #'string=))
	(push entry sorted)
	(setf inboxes 
	  (delete inbox inboxes :key #'first :test #'string=))))
    (nconc (nreverse sorted) inboxes)))

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

(defun get-main-inbox-information (spoolfile user boxes dotlock
				   ignore-cache)
  (when ignore-cache (clrhash boxes))
  
  (ensure-box "+inbox" boxes)
  
  (with-spool-file (f spoolfile :dotlock dotlock)
    (when (eq f :no-spool)
      (zero-box "+inbox" boxes)
      (reset-new-count-for-boxes boxes))
    
    (when (and (streamp f) 
	       (or ignore-cache
		   (> (file-write-date f)
		      (third (gethash "+inbox" boxes)))))
      (reset-new-count-for-boxes boxes)
      
      (setf (first (gethash "+inbox" boxes))
	  (count-if (lambda (p)
		      (match-re "/[0-9]+$" (enough-namestring p)))
		    (directory "inbox/")))
      
      (setf (third (gethash "+inbox" boxes)) (file-write-date f))
      
      (with-each-message (f box minfo user)
	(ensure-box box boxes)
	(incf (second (gethash box boxes)))
	(skip-message f)))))
	

;; returns oldcount and newcount.
(defun get-other-inbox-information (inbox boxes ignore-cache)
  (when ignore-cache (clrhash boxes))
  (let (dir old new unread)
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
      
      (setf old (first (gethash fullname boxes))))
    (values old new unread)))

(defun count-unread-messages (mh-seq &aux (unread 0))
  (dolist (unseen (retrieve-unseen-sequence mh-seq) unread)
    (incf unread (count-range unseen))))

(defun retrieve-unseen-sequence (file)
  (when (probe-file file)
    (with-open-file (s file :direction :input)
      (let (line)
	(loop
	  (setq line (read-line s nil s))
	  (when (eq s line) (return))
	  (multiple-value-bind (found ignore1 seq)
	      (match-re "^unseen: (.*)$" line)
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
      

		    
