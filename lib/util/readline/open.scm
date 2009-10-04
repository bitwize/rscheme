
;;; a NOP if it gets opened again

(define-method open-edit-port-from ((self <xterm-edit-input-port>)
                                    (peer <output-port>)
                                    (errs <output-port>))
  (values self peer errs))

(define-method open-edit-port-from ((self <mbox-input-port>)
				    (peer <output-port>)
				    (errs <output-port>))
  (if (member (getenv "TERM") '("xterm" "vt100"))
      (begin
	;(init-for-readline 0)
	(let ((nvt (open-output-nvt peer)))
	  (values (make <xterm-edit-input-port>
			underlying-input-port: self
			underlying-output-port: nvt)
		  nvt
		  (open-output-nvt errs))))
      (next-method)))

