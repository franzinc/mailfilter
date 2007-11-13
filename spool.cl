(in-package :user)

(eval-when (compile load eval)
  (require :osi)
  (use-package :excl.osi))


(defun guess-spool-filename (user)
  (let ((spooldir (guess-spool-directory)))
    (if (null spooldir)
	nil
      (concatenate 'string spooldir "/" user))))

(defun guess-spool-directory ()
  (dolist (dir '("/var/mail" "/var/spool/mail" "/usr/spool/mail"))
    (if (probe-file dir)
	(return-from guess-spool-directory dir)))
  nil)

(defclass spool-stream (file-simple-stream)
  ((marks :accessor spool-stream-marks :initform nil)))
  


;; lock/unlock a mail spool
;; Always uses posix locking.
;; Uses dotlocking as well if possible.
;; Tries to honor dotlocks even if we can't create them (due to spool
;; permissions).  This isn't particularly great if the other application
;; doesn't use posix locks as well.

;; Returns :no-spool if spool isn't there or is empty
;; normally returns two values.  
;;  1) the spool stream
;;  2) the name of the dotlockfile, if one was created.
(defun open-spool-stream (filename &key dotlock)
  (block nil
    (if (not (probe-file filename))
	(return :no-spool))
    (let ((dotlockfile (if dotlock (attempt-dotlock filename) nil))
	  (stream (make-instance 'spool-stream
		    :filename filename 
		    :external-format :latin1
		    :direction :io
		    :if-exists :append)))
      ;;(format t "Waiting for posix lock.~%")
      (lock-stream stream :wait t)
      ;;(format t "Got posix lock.~%")
      (when (= (file-length stream) 0)
	(unlock-stream stream)
	(close stream)
	(if dotlockfile
	    (delete-file dotlockfile))
	(return :no-spool))
      (file-position stream 0) ;; rewind
      ;;(format t "non-empty spool opened.~%")
      (values stream dotlockfile))))

;; returns the name of the lockfile if we acquired a dotlock.
;; returns nil if dotlocking is not allowed by regular users
;;  (which is the case on RedHat linux)
(defun attempt-dotlock (filename)
  (let ((lockfile (concatenate 'string filename ".lock"))
	f)
    ;;(format t "Trying to lock ~A~%" lockfile)
    (tagbody
     retry
      (handler-case
	  (setf f (os-open lockfile (logior *o-excl* *o-creat* *o-wronly*) 0))
	(syscall-error (e)
	  (let ((errno (syscall-error-errno e)))
	    (cond 
	     ;; lockfile exists.  Wait for it to go away
	     ((= errno *eexist*)
	      ;;(format t "Waiting for ~A to disappear.~%" lockfile)
	      (sleep 1)
	      (go retry))
	     ((= errno *eacces*)
	      ;;(format t "Dotlocking not allowed by regular users on this system.~%")
	      (return-from attempt-dotlock nil))
	     (t ;; something unexpected..  resignal the error
	      (error e)))))))
    ;; successful dotlock created
    (close f)
    ;;(format t "Locked ~A~%" lockfile)
    lockfile))

(defmacro with-spool-file ((streamvar filename &key dotlock) &body body)
  (let ((lockfilevar (gensym)))
    `(multiple-value-bind (,streamvar ,lockfilevar)
	 (open-spool-stream ,filename :dotlock ,dotlock)
       (unwind-protect
	   (progn 
	     ;;(if (eq ,streamvar :no-spool) (format t "spool file is empty.~%"))
	     ,@body)
	 ;; cleanup forms
	 (when (streamp ,streamvar)
	   ;;(format t "closing spool file.~%")
	   (close ,streamvar))
	 (when ,lockfilevar
	   ;;(format t "unlocking ~A~%" ,lockfilevar)
	   (delete-file ,lockfilevar))))))

(defun push-spool-stream-mark (stream)
  (car (push (file-position stream) (spool-stream-marks stream))))


(defun pop-spool-stream-mark (stream &key ignore)
  (let ((mark (pop (spool-stream-marks stream))))
    (if (null mark)
	(error "pop-spool-stream-mark: No marks set!"))
    (unless ignore
      (file-position stream mark))))


(defmacro with-spool-excursion ((stream) &body body)
  (let ((streamvar (gensym)))
    `(let* ((,streamvar ,stream))
       (push-spool-stream-mark ,streamvar)
       (unwind-protect 
	   (progn ,@body)
	 ;; cleanup
	 (pop-spool-stream-mark ,streamvar)))))

;; returns nil on EOF
;; returns :eom on end of message
(defun next-message-line (stream &key first)
  (block nil
    (push-spool-stream-mark stream)
    (let ((line (read-line stream nil nil)))
      (when (null line)
	(pop-spool-stream-mark stream :ignore t)
	(return nil)) ;; end of file.
      
      (when (prefix-of-p "From " line)
	(if* first
	   then
		(pop-spool-stream-mark stream :ignore t)
		(return line)
	   else
		(pop-spool-stream-mark stream) ;; unread
		(return :eom)))
      (pop-spool-stream-mark stream :ignore t)
      line)))


(defmacro eof-or-eom-p (line)
  (let ((linevar (gensym)))
    `(let ((,linevar ,line))
       (or (null ,linevar) (eq ,linevar :eom)))))

(defun prefix-of-p (prefix string)
  (declare (optimize (speed 3) (safety 0)))
  (let ((len-prefix (length prefix))
	(len-string (length string)))
    (and (>= len-string len-prefix)
	 (string= prefix string :end2 len-prefix))))

