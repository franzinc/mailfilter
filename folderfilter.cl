;; $Id: folderfilter.cl,v 1.8 2009/02/22 22:47:32 elliott Exp $

(in-package :user)

(eval-when (compile load eval)
  (require :osi)
  (use-package :excl.osi))

(defun main (&rest args)
  (setq *load-print* nil)
  (setq *load-verbose* nil)
  (setf *command-line-options*
	(make-hash-table :test #'equal))
  (let* ((user (getenv "USER"))
	 (home (getenv "HOME"))
	 (prgname (pop args))
	 (usage (format nil "usage: ~A [options] folder" prgname))
	 folder
	 debug)
    (if (null user)
	(error "Environment variable USER is not set"))
    (if (null home)
	(error "Environment variable HOME is not set"))

    (register-c-l-o "-c"
		    "Defaults to '~/.mailfilter.cl'"
		    (lambda ()
		      (setq *config-file* (first args))
		      (or (probe-file *config-file*)
			  (error "Config file ~A does not exist." 
				 *config-file*))))

    (register-c-l-o "-d"
		    "enable debugging"
		    (lambda ()
		      (setf debug t)))

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
      (call-command-line-option-action 
       (pop args)
       ;; default action
       (lambda (a)
	 (if (match-re "^\\+" a)
	     (if folder
		 (error "~A: Already specified a folder: ~A" 
			prgname 
			folder)
		 (setf folder a))
	     (error "~A: Invalid argument: ~A" 
		    prgname 
		    a)))))
    
    (load-user-config home)
    
    (when (null folder)
      (display-help usage)
      (error "Exiting.."))
    
    (let ((folderdir (concat (get-mhpath home) "/" (subseq folder 1) "/"))
	  (moves (make-hash-table :test #'equal)))
      (dolist (path (directory folderdir))
	(multiple-value-bind (matched msgnum)
	    (match-re "^\\d+$" (enough-namestring path folderdir))
	  (when matched
	    (with-spool-file (f path)
	      (when (not (eq f :no-spool)) ;; don't try to process empty files
		(with-single-message (f newfolder minfo user 
					:msgnum (parse-integer msgnum))
		  ;;(format t "msg ~A -> ~A~%" msgnum newfolder)
		  (if (string/= newfolder folder)
		      (push msgnum (gethash newfolder moves)))))))))
      
      (maphash 
       #'(lambda (newfold msgnums)
	   (if* debug
	      then
		   (setf msgnums (mapcar #'parse-integer msgnums))
		   (setf msgnums (sort msgnums #'<))
		   (format t "would do: refile ~A -src ~A ~A~%"
			   (list-to-delimited-string msgnums #\space)
			   folder newfold)
	      else
		   (let ((cmdvec
			  (coerce
			   (append '("refile" "refile") msgnums 
				   (list "-src" folder newfold))
			   'vector)))
		     (if (/= 0 (run-shell-command cmdvec :wait t))
			 (error "refile exited w/ non-zero status")))))
       moves))))
