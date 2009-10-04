(define-syntax make-area
   (syntax-form ()
      (make-allocation-area (object-allocation-area *application*)))
   (syntax-form (in)
      (make-allocation-area (object-allocation-area in))))

(define-syntax area-of
   (syntax-form ()
      (object-allocation-area *application*))
   (syntax-form (item)
      (object-allocation-area item)))
