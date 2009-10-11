
(define-class <<homogeneous-numeric-vector>> (<<standard-class>>)
  (homogeneous-type-primtype init-value: #f)
  (homogeneous-type-signed? init-value: #f)
  (homogeneous-type-bytes/element init-value: #f)
  (homogeneous-type-tag init-value: #f)
  (homogeneous-type-constructor init-value: #f))

(define-class <homogeneous-numeric-vector> (<sequence>) :abstract)

(define-method initial-state ((self <homogeneous-numeric-vector>))
  0)

;;;

#|
(define-method initialize ((self <<homogeneous-numeric-vector>>))
  ...)
|#

;;;

(define *hnvec-tag-table* (make-string-ci-table))

;;

(define-syntax (list->homogeneous-vector list
					 bytes/elem
					 signed?
					 class
					 setter)
  (let ((b (bvec-alloc class (* bytes/elem (length list)))))
    (let loop (((i <fixnum>) 0)
	       (l list))
      (if (null? l)
	  b
	  (begin
	    (setter b i (car l))
	    (loop (fixnum+ i bytes/elem) (cdr l)))))))
  
(define-macro (define-homogeneous-numeric-vector-type 
		tag
		primtype 
		b/e sgn get set)
  (let ((class-name (symbol-append "<" tag "-vector>"))
	(TAGvector? (symbol-append tag "vector?"))
	(TAGvector (symbol-append tag "vector"))
	(make-TAGvector (symbol-append "make-" tag "vector"))
	(TAGvector-length (symbol-append tag "vector-length"))
	(TAGvector-ref (symbol-append tag "vector-ref"))
	(TAGvector-set! (symbol-append tag "vector-set!"))
	(TAGvector->list (symbol-append tag "vector->list"))
	(list->TAGvector (symbol-append "list->" tag "vector")))
    ;;
    `(begin
       (define-class ,class-name (<homogeneous-numeric-vector>) 
	 metaclass: <<homogeneous-numeric-vector>>
	 :bvec)
       (table-insert! *hnvec-tag-table* ,(symbol->string tag) ,class-name)
       ;;
       (define (,TAGvector? thing)
	 (instance? thing ,class-name))
       (define (,TAGvector . args)
	 (,list->TAGvector args))
       (define (,make-TAGvector len . fill)
	 (let ((b (bvec-alloc ,class-name (* ,b/e len)))
	       (real-fill (if (null? fill) 0 (car fill))))
	   (let loop (((i <fixnum>) 0))
	     (if (>= i len)
		 b
		 (begin
		   (,set b (* i ,b/e) real-fill)
		   (loop (+ i 1)))))))
	   
       (define (,TAGvector-length (self ,class-name))
	 (quotient (bvec-length self) ,b/e))
       (define (,TAGvector-ref (self ,class-name) (index <fixnum>))
	 (,get self (* index ,b/e)))
       (define (,TAGvector-set! (self ,class-name) (index <fixnum>) val)
	 (,set self (* index ,b/e) val)
	 (values))
       (define (,TAGvector->list (self ,class-name))
	 ;; quickie
	 (map (lambda (i)
		(,TAGvector-ref self i))
	      (range (,TAGvector-length self))))
       (define (,list->TAGvector lst)
	 (list->homogeneous-vector lst ,b/e ,sgn ,class-name ,set))
       ;;
       ;; RScheme iteration protocol
       ;;
       (define-method final-state ((self ,class-name))
	 (let (((n <fixnum>) (bvec-length self)))
	   (if (eq? n 0)
	       #f
	       (fixnum- n ,b/e))))
       ;;
       (define-method next-state ((self ,class-name) (state <fixnum>))
	 (let (((n <fixnum>) (fixnum+ state ,b/e)))
	   (if (fixnum<? n (bvec-length self))
	       n
	       #f)))

       (define-method current-element ((self ,class-name) (state <fixnum>))
	 (,get self state))
       ;;
       (define-method size ((self ,class-name))
	 (quotient (bvec-length self) ,b/e))
       ;;
       (define-method element ((self ,class-name) 
			       (key <fixnum>) 
			       #key (default default: '#unbound))
	 (let ((x (fixnum* key ,b/e)))
	   (if (and (fixnum>=? x 0)
		    (fixnum<? x (bvec-length self)))
	       (,get self x)
	       (if (eq? default '#unbound)
		   (signal (make <no-such-key>
			     collection: self
			     key: key))
		   default))))
       ;;
       (define-method set-element! ((self <vector>)
				    (key <fixnum>)
				    (value <object>))
	 (let ((x (fixnum* key ,b/e)))
	   (if (and (fixnum>=? x 0)
		    (fixnum<? x (bvec-length self)))
	       (begin
		 (,set self x value)
		 (values))
	       (signal (make <no-such-key>
			 collection: self
			 key: key)))))
       ;;
       (set-homogeneous-type-primtype! ,class-name ',primtype)
       (set-homogeneous-type-signed?! ,class-name ,sgn)
       (set-homogeneous-type-bytes/element! ,class-name ,b/e)
       (set-homogeneous-type-tag! ,class-name ,(symbol->string tag))
       ;;
       (set-homogeneous-type-constructor! ,class-name ,list->TAGvector)
       ;;
       (&module
	(export ,class-name  ;; RScheme extension to SRFI-4
		,TAGvector?
		,TAGvector
		,make-TAGvector
		,TAGvector-length
		,TAGvector-ref
		,TAGvector-set!
		,TAGvector->list
		,list->TAGvector))
       )))
