(in-package :user)

(eval-when (compile load eval)
  (defparameter *file-list* '("lex" "emailaddr" "parse" "spool" "subs"))
  
  (defun do-compile ()
    (let ((excl::*break-on-warnings* t))
      (dolist (file *file-list*)
	(compile-file-if-needed (concatenate 'string file ".cl"))
	(load (concatenate 'string file ".fasl")))))
  
  (do-compile))

(defun build (prg)
  (let ((*file-list* (append *file-list* (list prg))))
    (do-compile)
    (generate-executable 
     prg
     (append
      (mapcar #'(lambda (f) (concatenate 'string f ".fasl")) *file-list*)
      (list :trace))
     :include-compiler t
     :runtime :dynamic)))






