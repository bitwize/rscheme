
(define-class <xterm-edit-input-port> (<basic-edit-input-port>)
  (history type: <list> init-value: '()))

(define-method provide-more-input ((self <xterm-edit-input-port>))
  (let ((line (xterm-readline 
	       (if (use-secondary? self)
		   (secondary-prompt self)
		   (primary-prompt self))
	       (underlying-input-port self)
	       (underlying-output-port self)
	       (make <readline-state>
		     completions: (completions self)
		     history: (history self)))))
    (if (string? line)
	(if (string=? line "")
	    "\n"
	    (begin
	      (set-history! self (cons line (history self)))
	      (string-append line "\n")))
	line)))
