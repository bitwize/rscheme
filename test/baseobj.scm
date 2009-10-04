#|------------------------------------------------------------*-Scheme-*--|
 | File:    test/baseobj.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.3
 | File mod date:    1997-11-29 23:10:42
 | System build:     v0.7.3.4-b7u, 2007-05-30
 |
 `------------------------------------------------------------------------|#

;;
;; test the basic object system
;;

(define-class <foo> (<object>)
  x
  y)

(define-method write-object ((self <foo>) port)
  (format port "<~s,~s>" (x self) (y self)))

(define-method display-object ((self <foo>) port)
  (format port "[~s,~s]" (x self) (y self)))


(define-class <blech> (<foo>)
  (color init-value: 'clear))

(define-method display-object ((self <blech>) port)
  (next-method)
  (format port "(~a)" (color self)))

(define p (make <blech> x: 13 y: 50))
(define q (make <foo> x: 50 y: 13))

;;

(define-method bleen ((self <object>) b)
  3)

(define-method bleen ((self <foo>) b)
  (+ (x self) 
     (x b)
     (next-method)))


(define-method bleen ((self <blech>) b)
  (list (y self) 
	(y b)
	(next-method)))

;;

(define (display->string item)
  (with-output-to-string
    (lambda ()
      (display item))))

(test-section
 (object-system)
 ;;
 (test-section
  (write-and-display)
  ;;
  (compare-using
   string=?
   ;;
   (check "<33,100>"
	  (object->bounded-string 50 (make <foo> y: 100 x: 33)))
   (check "<12,..."
	   (object->bounded-string 4 (make <foo> y: 100 x: 12)))
   (check "[50,13]" (display->string q))
   (check "[13,50](clear)" (display->string p))))
 ;;
 (test-section
  (method-dispatch)
  ;;
  (check 66 (bleen q p))
  (check '(50 13 66) (bleen p q))))

  
