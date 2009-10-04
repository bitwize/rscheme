
(define-class <getopt-error> (<condition>) :abstract)

(define-class <getopt-unknown-option-error> (<getopt-error>)
  given)

(define-class <getopt-excess-value> (<getopt-error>)
  given)

(define-class <getopt-missing-value> (<getopt-error>)
  given)
  

(define-method display-object ((self <getopt-unknown-option-error>) port)
  (format port "unknown option: ~s\n" (given self)))


;;; TODO
;;;      -m"foo the bar" as well as -m "foo the bar"
;;;      --bob=sally as well as --bob sally
;;; if that's consistent with getopt(3), that is

(define-class <opt-spec> (<object>)
  ;;
  ;;  `options' is an alist mapping strings (for long options)
  ;;  or chars (for short options) to the option name
  ;;  (usually a symbol) and the option argument spec
  ;;
  ;; an option spec is (option-name . arg-spec)
  ;;
  ;; an arg-spec is () => no arguments
  ;;             or (1) => required argument
  ;;             or (?) => optional argument
  ;;
  ;; for example,
  ;;    ((#\v verbose)          ; no arguments
  ;;     (#\f file 1))          ; one required argument
  ;;
  (options type: <list>))

(define-class <arg-list> (<object>)
  arguments
  ;; options that are still pending (?)
  (queued-options init-value: '())
  (options init-value: '())
  (non-options init-value: '()))

(define-method getopt ((self <list>) (spec <opt-spec>))
  (getopt (make <arg-list>
                arguments: self
                queued-options: '()
                options: '()
                non-options: '())
	  spec))

(define-method getopt ((self <arg-list>) (spec <opt-spec>))
  (let ((a (arguments self)))
    (cond
     ((pair? (queued-options self))
      (process-option (car (queued-options self))
		      spec
		      a
		      (cdr (queued-options self))
		      (options self)
		      (non-options self)
                      #f))
     ((null? a)
      (values #f self))
     ((and (> (string-length (car a)) 1)
	   (char=? (string-ref (car a) 0) #\-)
	   (not (char=? (string-ref (car a) 1) #\-)))
      (getopt (make <arg-list>
                    arguments: (cdr a)
                    queued-options: (cdr (string->list (car a)))
                    options: (options self)
                    non-options: (non-options self))
	      spec))
     ((and (> (string-length (car a)) 2)
	   (char=? (string-ref (car a) 0) #\-)
	   (char=? (string-ref (car a) 1) #\-))
      (let ((q (string-search (car a) #\=)))
        (if q
            (process-option (substring (car a) 2 q)
                            spec
                            (cdr a)
                            '()
                            (options self)
                            (non-options self)
                            (substring (car a) (+ q 1)))
            (process-option (substring (car a) 2)
                            spec
                            (cdr a)
                            '()
                            (options self)
                            (non-options self)
                            #f))))
     (else
      (process-non-option (car a)
			  spec
			  (cdr a)
			  (options self)
			  (non-options self))))))

(define (process-non-option non-option
			    spec
			    rest-of-args
			    current-options
			    current-non-options)
  (getopt (make <arg-list>
                arguments: rest-of-args
                queued-options: '()
                options: current-options
                non-options: (cons non-option current-non-options))
	  spec))

(define (process-option given-option
			spec
			rest-of-args
			rest-queued
			current-options
			current-non-options
                        inline-value)
  (let ((x (assoc given-option (options spec))))
    (if x
	(let ((opt-name (cadr x)))
	  (cond
	   ((equal? (cddr x) '())
            (if inline-value
                (signal (make <getopt-excess-value>
                              given: given-option))
                (values opt-name
                        (make <arg-list>
                              arguments: rest-of-args
                              queued-options: rest-queued
                              options: (cons (list opt-name) current-options)
                              non-options: current-non-options))))
	   ((equal? (cddr x) '(1))
            (if inline-value
                (values opt-name
                        (make <arg-list>
                              arguments: rest-of-args
                              queued-options: rest-queued
                              options: (cons (list opt-name inline-value)
                                             current-options)
                              non-options: current-non-options)
                        inline-value)
                (if (null? rest-of-args)
                    (signal (make <getopt-missing-value>
                                  given: given-option))
                    (values opt-name
                            (make <arg-list>
                                  arguments: (cdr rest-of-args)
                                  queued-options: rest-queued
                                  options: (cons 
                                            (list opt-name (car rest-of-args))
                                            current-options)
                                  non-options: current-non-options)
                            (car rest-of-args)))))
           ((equal? (cddr x) '(?))
            (error "getopt: bad argument spec(1): ~s" x))
           (else
            (error "getopt: bad option spec(2): ~s" x))))
	(signal (make <getopt-unknown-option-error>
                      given: given-option)))))

#|
(define *tspec*
  (make <opt-spec>
    options: '((#\v verbose)
	       (#\f file 1)
	       (#\z compress)
	       (#\R recursive)
	       ("preserve" preserve))))

(define (test . args)
  (let loop ((i (map to-string args)))
    (bind ((opt i #rest args (getopt i *tspec*)))
      (if opt
	  (begin
	    (format #t "~s => ~s\n" opt args)
	    (loop i))
	  (print i)))))
|#

;;;
;;;  Helper proc
;;;

(define (for-each-opt (args <list>) 
                      (opt <opt-spec>)
                      proc)
  (let loop ((a (make <arg-list>
                      arguments: args)))
    (bind ((opt next #rest args (getopt a opt)))
      (if opt
          (begin
            (apply proc opt args)
            (loop next))
          (non-options next)))))
