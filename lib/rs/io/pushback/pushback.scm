
;;;
;;;  Note:  This could be moved into the base system, and
;;;         the input-port-pushback method defined effeciently
;;;         for several kinds of input ports
;;;

(define-class <pushback-input-port> (<buffered-input-port>)
  (underlying-input-port type: <input-port>))

(define-method name ((self <pushback-input-port>))
  (name (underlying-input-port self)))

(define (open-input-pushback-port (source <input-port>))
  (make <pushback-input-port>
        underlying-input-port: source))

(define-method provide-more-input ((self <pushback-input-port>))
  (let ((m (input-port-read-max (underlying-input-port self) 8192)))
    (if (string? m)
        m
        #f)))

(define-method more-input-ready? ((self <pushback-input-port>))
  (input-port-char-ready? (underlying-input-port self)))

(define-method input-port-pushback ((self <pushback-input-port>)
                                    (data <string>))
  (let ((b (buffered-input-buffer self))
        (k (buffered-input-posn self)))
    (set-buffered-input-buffer!
     self
     (case k
       ((0)
        (string-append data b))
       (else
        (set-buffered-input-posn! self 0)
        (string-append data (substring b k)))))
    (values)))

