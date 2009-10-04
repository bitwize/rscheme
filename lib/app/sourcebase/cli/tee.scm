
(define-class <client-tee> (<output-port>)
  console-out
  client-out
  output-buffer)

(define (check-tee-flush (self <client-tee>))
  (if (pair? (buffer-overflows (output-buffer self)))
      (flush-output-port self)))

(define-method write-string ((self <client-tee>) (str <string>))
  (write-string (console-out self) str)
  (write-string (output-buffer self) str)
  (check-tee-flush self))

(define-method output-port-write-char ((self <client-tee>) ch)
  (output-port-write-char (console-out self) ch)
  (output-port-write-char (output-buffer self) ch)
  (check-tee-flush self))

(define-method flush-output-port ((self <client-tee>))
  (client-print-message 
   (client-out self)
   (close-output-port (output-buffer self)))
  (set-output-buffer! self (open-output-string))
  (flush-output-port (console-out self)))

(define-method close-output-port ((self <client-tee>))
  (flush-output-port self))

  
(define (open-client-tee out)
  (make <client-tee>
	console-out: (current-output-port)
	client-out: out
	output-buffer: (open-output-string)))
