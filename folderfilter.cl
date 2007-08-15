;; $Id: folderfilter.cl,v 1.6 2007/08/15 18:08:37 dancy Exp $

(in-package :user)

(eval-when (compile load eval)
  (require :osi)
  (use-package :excl.osi))

(defun main (&rest args)
  (let* ((user (getenv "USER"))
	 (home (getenv "HOME"))
	 (prgname (pop args))
	 folder
	 debug)
    (if (null user)
	(error "Environment variable USER is not set"))
    (if (null home)
	(error "Environment variable HOME is not set"))
    
    (while args
      (cond 
       ((string= (first args) "-c")
	(pop args)
	(setq *config-file* (first args))
	(or (probe-file *config-file*)
	    (error "Config file ~a does not exist." *config-file*))
	(pop args))
       ((string= (first args) "-d")
	(setf debug t)
	(pop args))
       ((match-re "^\\+" (first args))
	(if folder
	    (error "~A: Already specified a folder: ~A" prgname folder))
	(setf folder (pop args)))
       (t (error "~A: Invalid argument: ~A" prgname (first args)))))
    
    (load-user-config home)
    
    (if (null folder)
	(error "Usage: ~A [-d] folder" prgname))
    
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
