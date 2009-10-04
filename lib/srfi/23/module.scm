(define-module srfi.23.%setup ()
  (&module (import precore)
           (export simple-condition-msg 
                   simple-condition-args)))

(define-module srfi.23 ()
  (&module
   (implements SRFI-23 srfi-23)
   (import usual-inlines
           rs.sys.undefine
           srfi.23.%setup))
  ;;
  (undefine error)
  ;;
  (define-class <srfi-23-error> (<simple-error>))
  ;;
  ;;  the reporting we do here is more friendly
  ;;  to code that is used to SRFI-23
  ;;
  (define-method display-object ((self <srfi-23-error>) port)
    (format port "Error: ~a" (simple-condition-msg self))
    (for-each
     (lambda (arg)
       (format port " ~s" arg))
     (simple-condition-args self))
    (format port "\n"))
  ;;
  (define (error reason . args)
    (signal (make <srfi-23-error>
                  simple-condition-msg: reason
                  simple-condition-args: args)))
  ;;
  (&module (export error)))
