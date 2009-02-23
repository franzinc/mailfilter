;; $Id: command-line-options.cl,v 1.1 2009/02/23 01:19:46 elliott Exp $

(defvar *command-line-options*
  ()
  "A table of actions to use for a given command line option.")

(defstruct command-line-option ()
  ;; Documentation to display.
  (description "undocumented feature." :type string)
  ;; The name of an argument this option accepts
  (argument nil :type (or null string))
  ;; An action that'll be funcalled when the option is specified.
  ;;   Value can be a function that is funcalled or a string,
  ;;   which acts as an alias to another option.
  (action "-h" :type (or function string)))

(defun display-help (program-usage-line)
  "Displays the program usage line, then dynamically lists the command
line options."
  (format t program-usage-line)
  (format t "~2&options:~%")
  (loop for arg being the hash-keys in 
	*command-line-options*
	using (hash-value option)
	do (format t 
		   "~2T~A~@[ ~A~]~20T~A~%" 
		   arg 
		   (command-line-option-argument option)
		   (command-line-option-description option))))

(defun register-c-l-o (arg description function &optional argument)
  "For use in adding command line arguments.  The motivation behind
it is to make it easy to add new command line arguments and
have them documented."
  (let ((clo (make-command-line-option :description description
				       :argument    argument
				       :action      function)))
    (setf (gethash arg *command-line-options*) clo)))

(defun call-command-line-option-action 
    (arg &optional default-action)
  "Performs the action associated with a given command line.  If no action is
found for the given option, the default-action will be called."
  (let ((option (gethash arg *command-line-options*)))
    (if option
	(let ((action (command-line-option-action option)))
	  (cond ((stringp action) (call-command-line-option-action action))
		((functionp action) (funcall action))
		(t (error "Scripting error.. unknown action for '~A'" arg))))
	(funcall default-action arg))))