(define-class <text-location> (<object>)
  owner
  (input-line type: <fixnum>)
  (input-offset type: <fixnum>)
  (input-col type: <fixnum>))

(define-method location-line-number ((self <text-location>))
  (input-line self))

(define-method location-column-number ((self <text-location>))
  (input-col self))

(define-method location-port-owner ((self <text-location>))
  (owner self))

(define-class <text-extent> (<text-location>)
  (text-length type: <fixnum>))

(define-method to-string ((self <text-location>))
  (display-object self #f))

(define-method display-object ((self <text-location>) port)
  (let ((o (owner self)))
    (if o
        (format port "~a:~d:~d" o (input-line self) (input-col self))
        (format port "~d:~d" (input-line self) (input-col self)))))

(define-method display-object ((self <text-extent>) port)
  (next-method)
  (format port " (~d chars)" (text-length self)))

(define-method write-object ((self <text-location>) port)
  (format port "#[<text-location> ~a]" self))

(define-method write-object ((self <text-extent>) port)
  (format port "#[<text-extent> ~a]" self))

;;; doesn't work across newlines

(define (location+ (self <text-location>) (delta <fixnum>))
  (make <text-location>
        owner: (owner self)
        input-line: (input-line self)
        input-col: (+ (input-col self) delta)
        input-offset: (+ (input-offset self) delta)))

(define (location- (a <text-location>) (b <text-location>))
  (if (eq? (owner a) (owner b))
      (values
       (- (input-offset a) (input-offset b))
       (make <text-extent>
             owner: (owner b)
             input-line: (input-line b)
             input-offset: (input-offset b)
             input-col: (input-col b)
             text-length: (- (input-offset a) (input-offset b))))
      (error "location-: incommensurate text locations: ~s ~s" a b)))

(define-method start-location ((self <text-location>))
  self)

(define-method start-location ((self <text-extent>))
  (make <text-location>
        owner: (owner self)
        input-line: (input-line self)
        input-offset: (input-offset self)
        input-col: (input-col self)))
        
