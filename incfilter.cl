(in-package :user)

(eval-when (compile load eval)
  (require :osi)
  (use-package :excl.osi))

(defun main (&rest args)
  (let* ((user (getenv "USER"))
	 (home (getenv "HOME"))
	 (spoolfile (guess-spool-filename user))
	 (dotlock t)
	 (truncate :unset)
	 (prgname (pop args))
	 (*incorporating-mail* t)
	 debug)
    (declare (ignore prgname))
    (if (null user)
	(error "Environment variable USER is not set"))
    (if (null home)
	(error "Environment variable HOME is not set"))
    (if (null spoolfile)
	(error "Couldn't determine your spool filename"))
    
    (load-user-config home)
    
    (while args
      (cond 
       ((string= (first args) "-d")
	(setf debug t)
	(pop args))
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
       (t
	(error "incfilter: Don't know how to cope with option ~A" 
	       (first args)))))
    
    (if (eq truncate :unset)
	(setf truncate t))
    
    ;;(format t "will operate on file ~A~%" spoolfile)
    ;;(format t "truncate: ~A~%" truncate)
    ;;(format t "dotlocking: ~A~%" dotlock)
    
    (with-spool-file (f spoolfile :dotlock dotlock)
      (when (eq f :no-spool)
	(write-line "inc: no mail to incorporate" excl::*stderr*)
	(finish-output excl::*stderr*) ;; yeesh
	(return-from main))

      (let ((tmpdir (make-temp-dir-name home)))
	(with-tmp-dir (tmpdir)
	  (with-each-message (f folder minfo user)
	    (when debug
	      (format t "~D " (msginfo-num minfo))
	      (let ((s (get-header "Subject" (msginfo-headers minfo))))
		(if s
		    (format t "(~A) " s)))
	      (format t "--> ~A~%" folder))
	    
	    (let ((tmpfile (concatenate 'string tmpdir "/" folder)))
	      (with-open-file (out tmpfile
			       :direction :output
			       :if-does-not-exist :create
			       :if-exists :append)
		(copy-message-to-stream f out (msginfo-headers minfo) 
					:save-pos nil))))
	  
	  ;;(break "You may now inspect ~A%" tmpdir)
	  
	  (let ((folders (get-folders-used-list tmpdir)))
	    (dolist (folder folders)
	      (when (not (string= folder "+inbox"))
		(let ((cmdvec (vector "inc" "inc" folder "-file"
				      (concatenate 'string tmpdir "/" folder)
				      "-silent")))
		  (if debug
		      (debugcmd cmdvec))
		  
		  (when (not debug)
		    (if (/= 0 (run-shell-command cmdvec :wait t))
			(error "inc ~A exited w/ non-zero status" folder))))))
	    
	    (when (not (member "+inbox" folders :test #'string=))
	      (write-line "inc: no mail to incorporate" excl::*stderr*)
	      (finish-output excl::*stderr*) ;; yeesh
	      (return-from main))
	    
	    (let ((cmdvec
		   ;; having -truncate avoids a message about
		   ;; the file not being zero'd.
		   (vector "inc" "inc" "+inbox" "-file"
			   (concatenate 'string tmpdir "/+inbox")
			   "-truncate")))
	      (if debug
		  (debugcmd cmdvec))
	      
	      (when (not debug)
		(if (/= 0 (run-shell-command cmdvec :wait t))
		    (error "inc +folder exited w/ non-zero status"))
		
		(if* (not truncate)
		   then
			(format t "~A not zero'd~%" spoolfile)
		   else
			(os-truncate f 0))))))))))
		

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
		    
			
	
	


