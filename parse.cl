(in-package :user)

;; front end to emailaddr stuff.

(defun get-addr-list (input)
  (block nil
    (let ((tokens (emailaddr-lex input))
	  recips)
      (loop
	;; skip and leading whitespace, comments, and commas
	(while (and tokens
		    (or (whitespace-token-p (first tokens))
			(comment-token-p (first tokens))
			(comma-token-p (first tokens))))
	  (pop tokens))
	
	(if (null tokens) 
	    (return))
	
	(multiple-value-bind (new-recips remainder)
	    (get-next-address tokens)
	  (setf tokens remainder)
	  (setf recips (nconc recips new-recips))))
      recips)))

;; Returns a list of strings (since a group may have more than one mailbox)
(defun get-next-address (tokens)
  (multiple-value-bind (parsed remainder)
      (parse-address tokens)
    (if* (or (null parsed) (not (at-end-of-recip-p remainder)))
       then
	    ;; couldn't parse
	    (multiple-value-bind (toks remainder)
		(collect-tokens-up-to-comma tokens)
	      (declare (ignore toks))
	      (values nil remainder))
       else
	    (let ((mblist (if (groupspec-p (second parsed))
			      (second 
			       (groupspec-mailbox-list (second parsed)))
			    (list (second parsed)))))
	      (values 
	       (mapcar #'make-short-string-from-mailbox mblist)
	       remainder)))))

(defun make-short-string-from-mailbox (mb)
  (let ((addr (mailbox-to-emailaddr mb)))
    (if (and (null (emailaddr-user addr)) (null (emailaddr-domain addr)))
	"<>"
      (if (null (emailaddr-domain addr))
	  (emailaddr-user addr)
	(format nil "~A@~A" (emailaddr-user addr) (emailaddr-domain addr))))))
						   

;; we're really at a proper ending if, after skipping whitespace and comments,
;; we see no more tokens.. or a comma
(defun at-end-of-recip-p (tokens)
  (setf tokens (skip-cfws tokens))
  (or (null tokens) (comma-token-p (first tokens))))

(defun make-string-from-tokens-up-to-comma (tokens)
  (multiple-value-bind (tokens remainder)
      (collect-tokens-up-to-comma tokens)
    (values (tokens-to-string tokens) remainder)))

(defun collect-tokens-up-to-comma (tokens)
  (let (res)
    (while (and tokens (not (comma-token-p (first tokens))))
      (push (pop tokens) res))
    (values (nreverse res) tokens)))


