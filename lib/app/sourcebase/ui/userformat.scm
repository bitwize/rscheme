;;
;;  processing a user format
;;
;;  a "user format" is the specification of a function which
;;  takes as input a record and renders its desired representation
;;  onto the current output port
;;
;;  there is only one form supported so far:  (raw field...)
;;  which renders a |-seperated line consisting of the given fields,
;;  followed by a newline

;;
;;  takes a user format descriptor and returns a procedure of
;;  one argument which implements the formatting operation
;;

(define-method raw-format-write ((self <object>))
  (display self))

(define-method raw-format-write ((self <time>))
  (display (time->string self "%Y-%m-%d %H:%M:%S")))

(define (make-xml-generator grabs fields)
  (with-module util.xml
    (lambda (item)
      (write-sxml
       `(record
         ,@(map (lambda (f g)
                  (let ((x (g item)))
                    (cond
                     ((string? x)
                      `(,f ,x))
                     ((instance? x <time>)
                      `(,f (@ (unix ,(to-string 
                                      (format #f "~.0f"
                                              (time->epoch-seconds x)))))
                           ,(time->string x "%Y-%m-%d %H:%M:%S")))
                     (else
                      `(,f
                        ,(with-output-to-string
                           (lambda ()
                             (raw-format-write x))))))))
                fields
                grabs)))
      (newline))))

(define (make-raw-generator grabs)
  (lambda (item)
    (let loop ((g grabs)
               (not-first? #f))
      (if (null? g)
          (newline)
          (begin
            (if not-first?
                (write-char #\|))
            (raw-format-write ((car g) item))
            (loop (cdr g) #t))))))

(define (compile-user-format target-class fmt)
  (let ((slots (user-format-accessors target-class)))
    (cond
     ((and (list? fmt)
	   (pair? fmt)
	   (memq (car fmt) '(raw xml)))
      (let ((grabs (map (lambda (x)
			  (let ((g (assq x slots)))
			    (if g
				(cdr g)
				(error "~s: field not defined for ~s" 
				       x target-class))))
			(cdr fmt))))
        (case (car fmt)
          ((raw) (make-raw-generator grabs))
          ((xml) (make-xml-generator grabs (cdr fmt))))))
     (else
      (error "~s: unrecognized format" fmt)))))

(define-method user-format-accessors ((self <<class>>))
  (append
   (map (lambda (gf)
	  (cons (generic-function-name gf)
		(lambda (rec)
		  (gf rec))))
	(select (curry gf-has-method-on-class? self)
		(user-format-gfs)))
   (map (lambda (sd)
	 (cons (name sd)
	       (lambda (rec)
		 (slot-value sd rec))))
       (slot-descriptors self))))

;; Adding a GF to the list returned here will allow a user to
;; invoke it using a raw format request, e.g., this is how
;;    sb --report snapshot-members --format '(raw cr)'
;; works

(define (user-format-gfs)
  (list state cr title snapshot requestor open-time))

(define (gf-has-method-on-class? class gf)
  (any? (lambda ((meth <method>))
	  (subclass? class (car (function-specializers meth))))
	(generic-function-methods gf)))

;;;
;;;  build the appropriate rendition proc, using
;;;  `default-proc' if no `--format' is specified
;;;
;;;  valid formats look like:
;;;
;;;      --format field ...
;;;      --format "(raw field ...)"

(define (rendition-proc target-class req default-proc)
  (if (assq 'xml req)
      render-xml-as-text
      (if (assq 'format req)
          (let ((f (cdr (assq 'format req))))
            (if (> (length f) 1)
                (compile-user-format target-class 
                                     (cons 'raw 
                                           (map read-str f)))
                (if (null? f)
                    (service-error 497 "missing `--format' arguments")
                    (let ((f (read-str (car f))))
                      (if (symbol? f)
                          (compile-user-format target-class (list 'raw f))
                          (compile-user-format target-class f))))))
          default-proc)))

(define (render-xml-as-text item)
  (format #t "<?xml version=\"1.0\"?>\n")
  (format #t "<?xml-stylesheet type=\"text/xsl\" href=\"changereq.xsl\" ?>\n")
  (newline)
  (print-xml (render-xml item) 0))

(define (print-xml n i)
  (if (pair? n)
      (let ((t (make-string (* 2 i) #\space)))
        (if (any? pair? (cdr n))
            (begin
              (format #t "~a<~a>\n" t (car n))
              (for-each (rcurry print-xml (+ i 1)) (cdr n))
              (format #t "~a</~a>\n" t (car n)))
            (begin
              (format #t "~a<~a>" t (car n))
              (for-each (rcurry print-xml (+ i 1)) (cdr n))
              (format #t "</~a>\n" (car n)))))
      (display-escaped-xml n)))

(define *xml-special* (reg-expr->proc '(or #\< #\& #\>)))

(define-method display-escaped-xml ((content <object>))
  (display content))

(define-method display-escaped-xml ((content <string>))
  (bind ((s e (*xml-special* content)))
    (if s
        (begin
          (display (substring content 0 s))
          (case (string-ref content s)
            ((#\<) 
             (display "&lt;"))
            ((#\>) 
             (display "&gt;"))
            ((#\&)
             (display "&amp;")))
          (display-escaped-xml (substring content e)))
        (display content))))
