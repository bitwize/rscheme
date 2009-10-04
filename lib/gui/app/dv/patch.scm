#| fixed in b20...

(set-heap-type! <bignum> 0)


;;;
;;; arrange for `rs.lang' to export per-import *self*, eval, and load
;;;

(define-module-extend mlink ()
  (let ((m (get-module 'rs.lang)))
    (set-usage-hooks!
     m
     (list 
      (lambda (tle)
	(with-module compiler
	  (with-module repl
	    (begin
	      (define (make-eval-proc tle)
		(lambda 'eval (s-expr #optional (envt default: tle))
		   (eval-in-envt s-expr envt)))
	      (bind! tle (make <top-level-var>
			   name: '*self*
			   value: tle))
	      (bind! tle (make <top-level-var>
			   name: 'eval
			   value: (make-eval-proc tle)))
	      (bind! tle (make-load-bdg tle))))))))))

;;;

(define-module-extend rs.sys.tables ()
  (&module (export table-for-each table?)))

(define-module-extend rs.lang ()
  ;;
  (&module 
   (import rs.sys.generic-math)
   (export + - * /)
   (export binary+ binary- binary* binary/)
   (export quotient remainder modulo)
   (export numerator denominator floor ceiling truncate round)
   (export exp log sin cos tan asin acos atan)
   (export sqrt expt)
   (export magnitude angle))
  ;
  (&module (export write-string <char>))
  ;
  (&module (import usual-inlines)) ;; For `exported-value'
  (&module (export exported-value)))
|#

(define-module-extend rs.lang ()
  ;
  (&module (import usual-inlines)) ;; For `exported-value'
  (&module (export write-string exported-value)))

;; dv convenience
(with-module graphics.afm
  (get-afm "Times-Roman")
  (get-afm "Helvetica-Bold")
  (get-afm "Times-Italic"))
