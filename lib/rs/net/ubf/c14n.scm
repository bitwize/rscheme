

(define-method write-ubf ((self <integer>) (port <output-port>))
  (write-string port (number->string self)))

(define-method write-ubf ((self <string>) (port <output-port>))
  (output-port-write-char port #\")
  (write-string-escaped port self #\")
  (output-port-write-char port #\"))

(define-method write-ubf ((self <symbol>) (port <output-port>))
  (output-port-write-char port #\')
  (write-string-escaped port (symbol->string self) #\')
  (output-port-write-char port #\'))

(define-method write-ubf ((self <pair>) (port <output-port>))
  (write-ubf (cdr self) port)
  (output-port-write-char port #\space)
  (write-ubf (car self) port)
  (output-port-write-char port #\&))

(define-method write-ubf ((self <byte-vector>) (port <output-port>))
  (write-string port (number->string (bvec-length self)))
  (output-port-write-char port #\~)
  (write-string port (bvec->string self))
  (output-port-write-char port #\~))

(define-method write-ubf ((self <empty-list>) (port <output-port>))
  (output-port-write-char port #\#))

(define-method write-ubf ((self <vector>) (port <output-port>))
  (output-port-write-char port #\{)
  (let loop ((i 0))
    (if (< i (vector-length self))
        (begin
          (if (> i 0)
              (output-port-write-char port #\space))
          (write-ubf (vector-ref self i) port)
          (loop (+ i 1)))
        (output-port-write-char port #\}))))

(define (object->ubf u #optional port)
  (let ((p (or port (open-output-string))))
    ;;
    (write-ubf u p)
    (if port
        (values)
        (get-output-string p))))
