#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/compiler/errors.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.9
 | File mod date:    2003-11-05 19:32:59
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  compiler
 |
 | Purpose:          compile-time error reporting library
 `------------------------------------------------------------------------|#

;; *source-point* is a fluid var used
;; to describe the user's view of where the current
;; expression being compiled is.
;;  for files, it is a 3-vector
;;        [0] => 'file
;;        [1] => source <file-name>
;;        [2] => line number, or #f
;;        [3] => top-level expr, or #f
;;  for repl, it is a 2-vector
;;        [0] => 'repl
;;        [1] => prompt
;;        [2] => top-level expr
  
(define-fluid *source-point*)

(define-class <compile-error> (<condition>) :abstract
  error-location
  error-description
  error-args)

;;
;; concrete classes...
;;

(define-class <warning> (<compile-error>))

(define-class <syntax-error> (<compile-error>))
  
(define-class <semantic-error> (<compile-error>))

(define (compile-error-class (ce <compile-error>))
  (let ((c (object-class ce)))
    (if (eq? c <syntax-error>)
	"syntax error"
	(if (eq? c <warning>)
	    "warning"
	    "semantic error"))))

(define-method display-object ((self <compile-error>) port)
  (let ((srcpt (vector-ref (error-location self) 0))
	(srcexpr (vector-ref (error-location self) 1)))
    (if srcpt 
	(case (vector-ref srcpt 0)
	  ((file)
	   (if (vector-ref srcpt 2)
	       (format port "~a:~d: " 
		       (vector-ref srcpt 1)
		       (vector-ref srcpt 2))
	       (format port "~a: " (vector-ref srcpt 1)))
	   (if (and (not srcexpr) (vector-ref srcpt 3))
	       (set! srcexpr (vector-ref srcpt 3))))
	  ((repl)
	   (if (not srcexpr)
	       (set! srcexpr (vector-ref srcpt 2))))))
    (if srcexpr
	(format port "~a in form: ~#*50s\n>> " 
		(compile-error-class self)
		srcexpr)
	(format port "~a: " (compile-error-class self)))
    (apply format 
	   port
	   (error-description self)
	   (error-args self))
    (newline port)))

(define (error/syntax fmt . args)
  (error (make <syntax-error>
	       error-location: (vector (fluid-ref *source-point* #f) #f)
	       error-description: fmt
	       error-args: args)))


(define (error/syntax* form fmt . args)
  (error (make <syntax-error>
	       error-location: (vector (fluid-ref *source-point* #f) form)
	       error-description: fmt
	       error-args: args)))

(define (error/semantic fmt . args)
  (error (make <semantic-error>
	       error-location: (vector (fluid-ref *source-point* #f) #f)
	       error-description: fmt
	       error-args: args)))

(define (error/semantic* form fmt . args)
  (error (make <semantic-error>
	       error-location: (vector (fluid-ref *source-point* #f) form)
	       error-description: fmt
	       error-args: args)))

(define (error/internal . args)
  (error "Internal error: ~a" (apply format #f args)))

(define (warning fmt . args)
  (display (make <warning>
		 error-location: (vector (fluid-ref *source-point* #f) #f)
		 error-description: fmt
		 error-args: args))
  (values))

(define (current-source-point)
  (fluid-ref *source-point*))

(define (current-location-table)
  (let ((p (fluid-ref *source-point* #f)))
    (if (vector? p)
        (case (vector-ref p 0)
          ((file)
           (if (>= (vector-length p) 5)
               (vector-ref p 4)
               #f))
          (else
           #f))
        #f)))

(define (current-location-lookup source)
  (let ((t (current-location-table)))
    (and t (table-lookup t source))))

(define (append-source-property props dyn-envt)
  (let ((p (compile-point-file-and-line dyn-envt)))
    (if p
        (append props (list (cons 'source p)))
        props)))
