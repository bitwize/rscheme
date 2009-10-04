
(define-class <nvt-input> (<input-port>)
  (underlying-port type: <input-port>))

(define (open-input-nvt on)
  (make <nvt-input>
        underlying-port: on))

(define-method input-port-read-char ((self <nvt-input>))
  (let ((ch (input-port-read-char (underlying-port self))))
    (if (eq? ch #\cr)
        (input-port-read-char self)
        ch)))

(define-method input-port-peek-char ((self <nvt-input>))
  (let ((ch (input-port-peek-char (underlying-port self))))
    (if (eq? ch #\cr)
        (begin
          (input-port-read-char (underlying-port self))
          (input-port-peek-char self))
        ch)))
        


(define-method input-port-char-ready? ((self <nvt-input>))
  (input-port-char-ready? (underlying-port self)))
