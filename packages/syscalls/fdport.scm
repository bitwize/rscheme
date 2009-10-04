;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;      OUTPUT PORTS
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-class <fd-output-port> (<output-port>)
  file-descriptor)

(define-method output-port-write-char ((self <fd-output-port>) 
				       (ch <ascii-char>))
  (fd-write (file-descriptor self)
	    (string ch)
	    0
	    1))

(define-method write-string ((self <fd-output-port>) (str <string>))
  (fd-write (file-descriptor self)
	    str
	    0
	    (string-length str)))


(define-method close-output-port ((self <fd-output-port>))
  ;; we aren't considered to "own" the file-descriptor,
  ;; since we didn't open it
  0)

(define-method flush-output-port ((self <fd-output-port>))
  ;; we don't buffer, either
  0)

(define (open-output-fd fd)
  (make <fd-output-port>
	file-descriptor: fd))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;      INPUT PORTS
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-class <fd-input-port> (<buffered-input-port>)
  file-descriptor
  (buffer type: <string> init-value: ""))

(define-method initialize ((self <fd-input-port>))
  (set-buffer! self (make-string 1000)))

(define-method provide-more-input ((self <fd-input-port>))
  (let ((n (fd-read (file-descriptor self)
		    (buffer self)
		    0
		    (string-length (buffer self)))))
    (if (eq? n 0)
	#f
	(substring (buffer self) 0 n))))

(define (open-input-fd fd)
  (make <fd-input-port>
	file-descriptor: fd))
