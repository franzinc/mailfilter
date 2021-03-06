(in-package :user)

(eval-when (compile load eval)
  ;; Make sure regexp2 is ready early so that the match-re compiler
  ;; macro is active.
  (require :regexp2) 
  
  (require :smtp) 
  (require :acldns) 
  
  (defparameter *file-list* '("version" "lex" "emailaddr" "parse" "spool"
			      "subs" "command-line-options"))
  
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
      (list :acldns :smtp :regexp2 :shell :trace))
     :include-compiler t
     :runtime-bundle t
     :runtime :dynamic)))
