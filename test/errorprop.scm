;;; test built-in handling

(define-class <oops> (<condition>)
  (oopsy type: <string>))

(define-method display-object ((self <oops>) port)
  (format port "Oops! ~a\n" (oopsy self)))

(define (oops (str <string>))
  (signal (make <oops>
		oopsy: str)))

;;;

(define (bar y)
  (if (number? y)
      (+ y 10)
      (if (symbol? y)
	  (oops y)
	  (oops "not-a-number"))))

(define (foo a b)
  (+ (bar a) (bar b)))

(define (t1)
  (handler-case
   (list (foo 1 2)
	 (foo 1 "rats"))
   ((<condition> condition: o)
    (format #t "<error>~a</error>\n" o)
    (print o)
    #f)))
   
;;; test type_check_error

(define (t2)
  (handler-case
   (list (foo 1 2)
	 (foo 1 'x))
   ((<condition> condition: o)
    (format #t "<error>~a</error>\n" o)
    (print o)
    #f)))

;;; test lists

(define (t3)
  (handler-case
   (length '(1 2 3 . x))
   ((<condition> condition: o)
    (format #t "<error>~a</error>\n" o)
    (print o)
    #f)))

;;; test generation in glue

(define (t4)
  (handler-case
   (with-module syscalls (time->string "foo"))
   ((<condition> condition: o)
    (format #t "<error>~a</error>\n" o)
    (print o)
    #f)))

(if-implements Test-Suite-Driver
 (test-section basic-function
   (check #f (t1))
   (check #f (t2))
   (check #f (t3))
   (check #f (t4))))
