

(define (string-upcase str)
  (list->string
   (map char-upcase (string->list str))))

(define-method properties ((self <template>))
  (function-descr self))

(define-method set-properties! ((self <template>) val)
  (set-function-descr! self val))

#|
(define-method name ((self <template>))
  (let ((l (get-property self 'location #f)))
    (if l
	(format #f "~j:~a:~a" (template-place self) (cadr l) (car l))
	(format #f "~j" (template-place self)))))

(define-method write-object ((self <template>) port)
  (format port "#[<template> ~a ~s ~s ~s]" 
	  (to-string self)
	  (gvec-ref self 0)
	  (gvec-ref self 1)
	  (gvec-ref self 2)))
|#