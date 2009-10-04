(define-class <tiff-type> (<object>)
  (type-id type: <fixnum>)
  (name type: <symbol>)
  (element-size type: <fixnum>)
  (reader-index type: <fixnum>))

(define (type-id->type id)
  (and (< id (vector-length *tiff-types*))
       (vector-ref *tiff-types* id)))

(define (make-tiff-type id nm elem rx)
  (let ((t (make <tiff-type>
		 type-id: id
		 name: nm
		 element-size: elem
		 reader-index: rx)))
    (vector-set! *tiff-types* id t)
    t))

(define-method to-string ((self <tiff-type>))
  (symbol->string (name self)))

(define *tiff-types* (make-vector 8))

(make-tiff-type 1 'byte 1 2)
(make-tiff-type 2 'ascii 1 2)
(make-tiff-type 3 'short 2 0)
(make-tiff-type 4 'long 4 1)
(make-tiff-type 5 'rational 8 3)

