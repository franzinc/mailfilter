;; $Id: incfilter.cl,v 1.20 2009/02/22 22:24:06 elliott Exp $

(in-package :user)

(eval-when (compile load eval)
  (require :osi)
  (use-package :excl.osi)
  (require :locale))

(defun main (&rest args)
  (setf *command-line-options*
	(make-hash-table :test #'equal))
  (let* ((user (getenv "USER"))
	 (home (getenv "HOME"))
	 (spoolfile (guess-spool-filename user))
	 (inputfile spoolfile)
	 (dotlock t)
	 (truncate :unset)
	 (prgname (pop args))
	 (usage (format nil "usage: ~A [options] folders" prgname))
	 (*incorporating-mail* t)
	 inc-args
	 debug verbose)
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
			  (error "Config file ~A does not exist."
				 *config-file*)))
		    "CONFIG_FILE")
    
    (register-c-l-o "-d"
		    "increase the debugging level."
		    (lambda ()
		      (if debug 
			  (incf debug)
			  (setf debug 1))))

    (register-c-l-o "-v"
		    "increase the verbosity level."
		    (lambda ()
		      (if verbose
			  (incf verbose)
			  (setf verbose 1))))

    (register-c-l-o "-notruncate"
		    "disable truncating INPUTFILE"
		    (lambda ()
		      (setf truncate nil)))

    (register-c-l-o "-truncate"
		    "enable truncating INPUTFILE (default)"
		    (lambda ()
		      (setf truncate t)))

    (register-c-l-o "-file"
		    (format nil "defaults to '~A'" inputfile)
		    (lambda ()
		      (if (null args)
			  (error "inc: missing argument to -file"))
		      (setf inputfile (pop args))
		      (setf dotlock nil)
		      (if (eq truncate :unset)
			  (setf truncate nil)))
		    "INPUTFILE")

    (register-c-l-o "-h"
		    "Print this help menu and exit."
		    (lambda ()
		      (display-help usage)
		      (error "Exiting..")))

    (register-c-l-o "-help"
		    "The same as -h"
		    "-h")

    (while args
      (call-command-line-option-action (pop args)
				       (lambda (arg)
					 (push arg inc-args))))
    
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
	     (if *verbose-logging*
		 (logmsg logfile "incfilter started."))
	     (with-spool-file (f inputfile :dotlock dotlock)
	       (when (eq f :no-spool)
		 (if *verbose-logging*
		     (logmsg logfile "No mail to incorporate. Terminating."))
		 (write-line "inc: no mail to incorporate" excl::*stderr*)
		 (finish-output excl::*stderr*) ;; yeesh
		 (return-from main))

	       (when (and *save-spools* (not debug) (eq inputfile spoolfile))
		 (let ((copyfilename (make-timestamped-filename *save-spools* "spool")))
		   (logmsg logfile "Making copy of ~a to ~a" 
			   inputfile copyfilename)
		   (ensure-directories-exist *save-spools*)
		   (sys:copy-file inputfile copyfilename)))
	       
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
			   (if *verbose-logging*
			       (debugcmd cmdvec logfile))

			   (when (not debug)
			     (if (/= 0 (run-shell-command cmdvec :wait t))
				 (error "inc ~A exited w/ non-zero status"
					folder))))))
		     
		     (when (not (member "+inbox" folders :test #'string=))
		       (when (not debug)
			 (if truncate
			     (os-truncate f 0))
			 (if *verbose-logging*
			     (logmsg logfile "incfilter terminating."))
			 (write-line "inc: no mail to incorporate"
				     excl::*stderr*)
			 (finish-output excl::*stderr*)) ;; yeesh
		       (return-from main))

		     (let ((cmdvec 
			    (make-inc-cmdvec "+inbox" tmpdir inc-args
					     :truncate t)))

		       (if debug
			   (debugcmd cmdvec))
		       (if *verbose-logging*
			   (debugcmd cmdvec logfile))
	      
		       (when (not debug)
			 (if (/= 0 (run-shell-command cmdvec :wait t))
			     (error "inc +inbox exited w/ non-zero status"))
		
			 (if* (not truncate)
			    then (format t "~A not zero'd~%" inputfile)
			    else (os-truncate f 0))
			 (if *verbose-logging*
			     (logmsg logfile "incfilter terminating.")))))))))))
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
  
(defun debugcmd (vec &optional (stream t))
  (format stream "debug: ~A~%"
	  (list-to-delimited-string 
	   (cdr (coerce vec 'list))
	   #\space))
  (finish-output stream))

(defun make-inc-cmdvec (folder tmpdir inc-args &key silent truncate)
  (let ((list (list "inc" "inc" folder 
		    "-file" (concatenate 'string tmpdir "/" folder))))
    (if silent
	(setf list (nconc list (list "-silent"))))
    (if truncate
	(setf list (nconc list (list "-truncate"))))
    (if *verbose-logging*
  	(setf list 
	  (nconc 
	   list 
	   (list "-audit" 
		 (namestring (translate-logical-pathname *logfilename*))))))
    (coerce (nconc list inc-args) 'vector)))

(defun logentry (stream minfo folder)
  (format stream "~A:~A:~A:~A:~A~%"
	  (ctime) 
	  (list-to-delimited-string (msginfo-froms minfo) #\,)
	  (get-header "Message-Id" (msginfo-headers minfo) :null-string t)
	  folder
	  (msginfo-num minfo))
  (finish-output stream))

(defun logmsg (stream format &rest args)
  (write-string (excl.osi:ctime) stream)
  (write-string ": " stream)
  (apply #'format stream format args)
  (terpri stream)
  (finish-output stream))
