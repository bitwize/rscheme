
(define-class <text-input-port> (<input-port>)
  underlying-input-port
  owner
  (input-line type: <fixnum> init-value: 1)
  (input-sol type: <fixnum> init-value: 0)
  (input-offset type: <fixnum> init-value: 0))

(define-method close-input-port ((self <text-input-port>))
  ;; return number of characters read
  (input-offset self))
  
(define-method input-port-read-char ((self <text-input-port>))
  (let ((ch (input-port-read-char (underlying-input-port self))))
    (if (eof-object? ch)
	ch
	(begin
	  (set-input-offset! self (add1 (input-offset self)))
	  (if (eq? ch #\newline)
	      (begin
		(set-input-line! self (+ (input-line self) 1))
		(set-input-sol! self (input-offset self))))
	  ch))))

(define-method input-port-peek-char ((self <text-input-port>))
  (input-port-peek-char (underlying-input-port self)))

(define-method location ((self <text-input-port>))
  (make <text-location>
	owner: (owner self)
	input-line: (input-line self)
	input-col: (+ 1 (- (input-offset self) (input-sol self)))
	input-offset: (input-offset self)))

(define-method previous-location ((self <text-input-port>))
  (location+ (location self) -1))

;;

(define (make-text-input-port (p <input-port>) . extra)
  (make <text-input-port>
	owner: (if (pair? extra)
		   (car extra)
		   #f)
	underlying-input-port: p))


