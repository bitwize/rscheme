(define-style-type <stroke> color linewidth)

(define-syntax define-stroke-style 
  (syntax-form (name () . settings)
    (define-style name <stroke> () . settings))
  (syntax-form (name (basis) . settings)
    (define-style name <stroke> (basis) . settings)))

(define (init-stylesheet)
  (let ((s (make-stylesheet)))
    (call-with-stylesheet
     s
     (lambda ()
       (define-style $default-stroke <stroke> () color: 'black linewidth: 1)
       (define-stroke-style red-stroke ($default-stroke) color: 'red)))
    s))

(define (q)
  (let ((s (init-stylesheet)))
    (call-with-stylesheet
     s
     (lambda ()
       (get-all-style-attributes 'red-stroke)))))


