;; $Id: incfilter.cl,v 1.18 2007/12/01 02:35:22 layer Exp $

(in-package :user)

(eval-when (compile load eval)
  (require :osi)
  (use-package :excl.osi)
  (require :locale))

(defun main (&rest args)
  (let* ((user (getenv "USER"))
	 (home (getenv "HOME"))
	 (spoolfile (guess-spool-filename user))
	 (dotlock t)
	 (truncate :unset)
	 (prgname (pop args))
	 (*incorporating-mail* t)
	 inc-args
	 debug verbose)
    (declare (ignore prgname))
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
       ((string= (first args) "-d")
	(if debug
	    (incf debug)
	  (setf debug 1))
	(pop args))
       ((string= (first args) "-v")
	(pop args)
	(if verbose
	    (incf verbose)
	  (setf verbose 1)))
       ((string= (first args) "-notruncate")
	(setf truncate nil)
	(pop args))
       ((string= (first args) "-truncate")
	(setf truncate t)
	(pop args))
       ((string= (first args) "-file")
	(pop args)
	(if (null args)
	    (error "inc: missing argument to -file"))
	(setf spoolfile (pop args))
	(setf dotlock nil)
	(if (eq truncate :unset)
	    (setf truncate nil)))
       (t (push (pop args) inc-args))))
    
    (load-user-config home)
    
    (setf inc-args (nreverse inc-args))
    
    (if (eq truncate :unset)
	(setf truncate t))

    (flet
	((doit ()
	   (with-open-file (logfile (if debug "/dev/null" *logfilename*)
			    :direction :output
			    :if-exists :always-append
			    :if-does-not-exist :always-append)
	     (with-spool-file (f spoolfile :dotlock dotlock)
	       (when (eq f :no-spool)
		 (write-line "inc: no mail to incorporate" excl::*stderr*)
		 (finish-output excl::*stderr*) ;; yeesh
		 (return-from main))

	       (let ((tmpdir (make-temp-dir-name home)))
		 (with-tmp-dir (tmpdir)
		   (with-each-message (f folder minfo user)
		     (when (or debug verbose)
		       (format t "~D " (msginfo-num minfo))
		       (let ((s (get-header "Subject"
					    (msginfo-headers minfo))))
			 (if s
			     (format t "(~A) " s)))
		       (format t "--> ~A~%" folder))
	      
		     (let ((tmpfile (concatenate 'string tmpdir "/" folder)))
		       (with-open-file (out tmpfile
					:direction :output
					:external-format :latin1
					:if-does-not-exist :create
					:if-exists :append)
			 (copy-message-to-stream f out (msginfo-headers minfo) 
						 :save-pos nil)))
	      
		     (logentry logfile minfo folder))
	    
		   ;; Outside of with-each message.
	  
		   (if (and debug (> debug 1))
		       (break "You may now inspect ~A" tmpdir))
	  
		   (let ((folders (get-folders-used-list tmpdir)))
		     (dolist (folder folders)
		       (when (not (string= folder "+inbox"))
			 (let ((cmdvec 
				(make-inc-cmdvec folder tmpdir inc-args
						 :silent (not verbose))))

			   (if (or debug verbose)
			       (debugcmd cmdvec))
		  
			   (when (not debug)
			     (if (/= 0 (run-shell-command cmdvec :wait t))
				 (error "inc ~A exited w/ non-zero status"
					folder))))))

		     (when (not (member "+inbox" folders :test #'string=))
		       (when (not debug)
			 (if truncate
			     (os-truncate f 0))
			 (write-line "inc: no mail to incorporate"
				     excl::*stderr*)
			 (finish-output excl::*stderr*)) ;; yeesh
		       (return-from main))

		     (let ((cmdvec 
			    (make-inc-cmdvec "+inbox" tmpdir inc-args
					     :truncate t)))

		       (if debug
			   (debugcmd cmdvec))
	      
		       (when (not debug)
			 (if (/= 0 (run-shell-command cmdvec :wait t))
			     (error "inc +inbox exited w/ non-zero status"))
		
			 (if* (not truncate)
			    then
				 (format t "~A not zero'd~%" spoolfile)
			    else
				 (os-truncate f 0)))))))))))
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
~@<inc: An unhandled error condition has been signalled:~3I ~a~I~:@>~%~%"
				   e))
			  (ignore-errors ;prevent recursion
			   (tpl:do-command "zoom"
			     :from-read-eval-print-loop nil
			     :count t :all t)))))))
		(doit))
	 else (handler-case (doit)
		(error (c) (format t "inc: ~a~&" c)))))))

(defun make-temp-dir-name (homedir)
  (format nil "~A/incfilter.tmp.~D.~D"
	  homedir (getpid) (get-universal-time)))

(defun get-folders-used-list (tmpdir)
  (let ((btmpdir (concatenate 'string tmpdir "/")))
    (mapcar #'(lambda (x) (enough-namestring x btmpdir))
	    (directory btmpdir))))
  
(defun debugcmd (vec)
  (format t "debug: ~A~%"
	  (list-to-delimited-string 
	   (cdr (coerce vec 'list))
	   #\space)))

(defun make-inc-cmdvec (folder tmpdir inc-args &key silent truncate)
  (let ((list (list "inc" "inc" folder 
		    "-file" (concatenate 'string tmpdir "/" folder))))
    (if silent
	(setf list (nconc list (list "-silent"))))
    (if truncate
	(setf list (nconc list (list "-truncate"))))
  
    (coerce (nconc list inc-args) 'vector)))

(defun logentry (stream minfo folder)
  (format stream "~A:~A:~A:~A:~A~%"
	  (ctime) 
	  (list-to-delimited-string (msginfo-froms minfo) #\,)
	  (get-header "Message-Id" (msginfo-headers minfo) :null-string t)
	  folder
	  (msginfo-num minfo))
  (finish-output stream))
