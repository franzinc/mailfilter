;; $Id: mailstatus.cl,v 1.4 2004/11/30 21:31:53 layer Exp $

(in-package :user)

(defun main (&rest args)
  (let* ((user (getenv "USER"))
	 (home (getenv "HOME"))
	 (spoolfile (guess-spool-filename user))
	 (prgname (pop args))
	 (interval 30) ;; seconds
	 (boxes (make-hash-table :test #'equal))
	 show-time once debug)
    (if (null user)
	(error "Environment variable USER is not set"))
    (if (null home)
	(error "Environment variable HOME is not set"))
    (if (null spoolfile)
	(error "Couldn't determine your spool filename"))
    
    (while args
      (cond 
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
       (t
	(error "~A: Unexpected command line argument: ~A" (first args)))))

    (load-user-config home :nocompile debug)
    
    ;; bleh
    (setf *default-pathname-defaults* (pathname (chdir (get-mhpath home))))

    (flet
	((doit ()
	   (loop ;; interval loop
	     (with-output-to-string (output)
	       (when show-time (output-time output))
	
	       ;; the 'boxes' hash table holds entries in the following form:
	       ;; +folder ==> (old new modification-time)
	       ;; (old is not used for +inbox)
	
	       (get-main-inbox-information spoolfile user boxes)
	
	       (let ((ninbox (second (gethash "+inbox" boxes))))
		 (if (> ninbox 0)
		     (format output " ~D+" ninbox)))
	
	       ;; inbox ==> (shortname longname fullname)
	       (dolist (inbox (make-list-of-inboxes))
		 (multiple-value-bind (old new)
		     (get-other-inbox-information inbox boxes)
		   (let ((shortname (first inbox)))
		     (cond 
		      ((and (> new 0) (> old 0))
		       (format output " ~D>~A:~D" new shortname old))
		      ((> new 0)
		       (format output " ~D>~A" new shortname))
		      ((> old 0)
		       (format output " ~A:~D" shortname old))))))
	
	       (write-string (get-output-stream-string output))
	       (finish-output))

	     (if once
		 (return))
      
	     (sleep interval))))
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
  (let (sorted entry)
    (dolist (inbox *mailstatus-inbox-folder-order*)
      (when (setf entry (find inbox inboxes :key #'first :test #'string=))
	(push entry sorted)
	(setf inboxes 
	  (delete inbox inboxes :key #'first :test #'string=))))

    (nconc (nreverse sorted)  (sort inboxes #'string< :key #'first))))

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

(defun get-main-inbox-information (spoolfile user boxes)
  (ensure-box "+inbox" boxes)
  
  (with-spool-file (f spoolfile :dotlock t)
    (when (eq f :no-spool)
      (zero-box "+inbox" boxes)
      (reset-new-count-for-boxes boxes))
    
    (when (and (streamp f) 
	       (> (file-write-date f)
		  (third (gethash "+inbox" boxes))))
      (reset-new-count-for-boxes boxes)
      
      (setf (third (gethash "+inbox" boxes)) (file-write-date f))
      
      (with-each-message (f box minfo user)
	(ensure-box box boxes)
	(incf (second (gethash box boxes)))
	(skip-message f)))))
	

;; returns oldcount and newcount.
(defun get-other-inbox-information (inbox boxes)
  (let (old new)
    (multiple-value-bind (shortname longname fullname)
	(values-list inbox)
      (declare (ignore shortname))
      
      (ensure-box fullname boxes)
      
      (setf new (second (gethash fullname boxes)))
      
      (when (> (file-write-date longname)
	       (third (gethash fullname boxes)))
	(setf (third (gethash fullname boxes)) 
	  (file-write-date longname))
	
	(setf (first (gethash fullname boxes))
	  (count-if (lambda (p)
		      (match-re "/[0-9]+$" (enough-namestring p)))
		    (directory (concatenate 'string longname "/")))))
      
      (setf old (first (gethash fullname boxes))))
    (values old new)))
