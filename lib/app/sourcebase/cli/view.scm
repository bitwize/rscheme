(define (handle-ls-request args req inp out (u <user>))
  (let ((state (make <vsh-state>
		     current-filespace: (get-fspace-arg req)
		     current-path: (get-pwd-arg req)
		     variables: *standard-vsh-variables*
		     user: u
		     group: (world-group *application*)
		     current-local-top: (string->dir "/tmp"))))
    (print state)
    (fluid-let ((*vsh-state* state))
      (client-print-message
       out
       (with-output-to-string
	  (lambda ()
	    (apply ls args)))))))
