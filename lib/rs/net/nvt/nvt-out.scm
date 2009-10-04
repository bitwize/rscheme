;;;
;;;  network virtual terminal support (ie, telnet)
;;;
;;;  (also applicable to terminals in "raw" mode)

;;; so far, all we support is turning '\n' into '\r\n'
;;; and output line-buffering

(define-class <nvt-output> (<output-port>)
  (underlying-port type: <output-port>))

(define (open-output-nvt on-port)
  (make <nvt-output>
	underlying-port: on-port))

(define-method output-port-write-char ((self <nvt-output>) ch)
  (if (eq? ch #\newline)
      (begin
	(write-string (underlying-port self) "\r\n")
	(flush-output-port (underlying-port self)))
      (output-port-write-char (underlying-port self) ch)))

(define-method write-string ((self <nvt-output>) (str <string>))
  (let loop ((i 0))
    (let ((nl (string-search str #\newline i)))
      (if nl
	  (begin
	    (writev (underlying-port self) (vector (substring str i nl)
						   "\r\n"))
	    (loop (+ nl 1)))
	  (if (eq? i 0)
	      (write-string (underlying-port self) str)
	      (begin
		(write-string (underlying-port self) (substring str i))
		(flush-output-port (underlying-port self))))))))

(define-method flush-output-port ((self <nvt-output>))
  (flush-output-port (underlying-port self)))
