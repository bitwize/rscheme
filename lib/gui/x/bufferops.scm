
;;;  coerce a valid RScheme number into an X integer (ie, a <fixnum>)
;;;  (now that we have bignums, its easy for the client to send us
;;;  them -- esp. since (object-class (numerator 1/3)) ==> #{<bignum>})

(define-syntax (xint q)
  (let ((temp q))
    (if (fixnum? temp)
	temp
	(+ 0 (inexact->exact temp))))) ;; chop floats and demote bignums

;; support for formatting and parsing X protocol frames

(define (unit-length kwd)
  (cadr (assq kwd '((u1: 1)
                    (u2: 2)
                    (u4: 4)
                    (s1: 1)
                    (s2: 2)
                    (s4: 4)))))

(define-syntax (xbo-read-u1 b x) (bvec-read-unsigned-8 b x))
(define-syntax (xbo-read-u2 b x) (bvec-read-unsigned-16 b x))
(define-syntax (xbo-read-u4 b x) (bvec-read-signed-32 b x))

(define-syntax (xbo-read-s1 b x) (bvec-read-signed-8 b x))
(define-syntax (xbo-read-s2 b x) (bvec-read-signed-16 b x))
(define-syntax (xbo-read-s4 b x) (bvec-read-signed-32 b x))

(define-macro (unpack* expr offset . descr)
  (let loop ((d descr)
	     (grabs '())
	     (i 0))
    (if (null? d)
	`(values ,@(reverse grabs))
	(if (fixnum? (car d))
	    (loop (cdr d)
		  grabs
		  (+ i (car d)))
	    (loop (cdr d)
		  (cons `(,(name-for-unit-reader (car d)) 
			  ,expr 
			  ,(if (eq? offset 0)
			       i
			       `(+ ,offset ,i)))
			grabs)
		  (+ i (unit-length (car d))))))))

(define-macro (unpack expr . descr)
  `(unpack* ,expr 0 ,@descr))

(define-macro (unpack-from-input-string inp-str . descr)
  (let loop ((d descr)
	     (grabs '())
	     (i 0))
    (if (null? d)
	`(let* (((%port <string-input-port>) ,inp-str)
		((%str <string>) (buffered-input-buffer %port))
		((%base <fixnum>) (buffered-input-posn %port)))
	   (set-buffered-input-posn! %port (fixnum+ %base ,i))
	   (values ,@(reverse grabs)))
	(if (fixnum? (car d))
	    (loop (cdr d)
		  grabs
		  (+ i (car d)))
	    (loop (cdr d)
		  (cons `(,(name-for-unit-reader (car d)) 
			  %str 
			  (fixnum+ %base ,i))
			grabs)
		  (+ i (unit-length (car d))))))))

(define (name-for-unit-reader kwd)
  (cadr (assq kwd
	      '((u1: xbo-read-u1)
		(u2: xbo-read-u2)
		(u4: xbo-read-s4)
		(s1: xbo-read-s1)
		(s2: xbo-read-s2)
		(s4: xbo-read-s4)))))

(define-macro (with-unpacked expr descr . body)
  (let loop ((vars '())
	     (modes '())
	     (d descr))
    (if (null? d)
	`(bind ((,@(reverse vars) (unpack ,expr ,@(reverse modes))))
	    ,@body)
	(if (eq? (cadr d) '-)
	    (loop vars (cons (unit-length (car d)) modes) (cddr d))
	    (loop (cons (cadr d) vars) (cons (car d) modes) (cddr d))))))

(define-macro (with-unpacked-from-input-string str descr . body)
  (let loop ((vars '())
	     (modes '())
	     (d descr))
    (if (null? d)
	`(bind ((,@(reverse vars) (unpack-from-input-string
				   ,str
				   ,@(reverse modes))))
	    ,@body)
	(if (eq? (cadr d) '-)
	    (loop vars (cons (unit-length (car d)) modes) (cddr d))
	    (loop (cons (cadr d) vars) (cons (car d) modes) (cddr d))))))

(define (xbo-endianess)
  (case (xbo-read-u2 "0z" 0)
   ((#x307a) 'big-endian)
   ((#x7a30) 'little-endian)
   (else (error "unknown endianess"))))

(define-syntax (second-value expr)
  (bind ((first second expr)) 
    second))

(define-syntax (pad4str n)
  (second-value (pad4 n)))
  
(define (pad4 (n <fixnum>))
  (let (((rem <fixnum>) (bitwise-and n #b11)))
    (if (eq? rem 0)
	(values 0 "")
	(values (fixnum- 4 rem)
	        (vector-ref '#("" "\0\0\0" "\0\0" "\0") rem)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-syntax (xbo-write-u1 b x v) (bvec-write-unsigned-8 b x (xint v)))
(define-syntax (xbo-write-u2 b x v) (bvec-write-unsigned-16 b x (xint v)))
(define-syntax (xbo-write-u4 b x v) (bvec-write-signed-32 b x (xint v)))

(define-syntax (xbo-write-s1 b x v) (bvec-write-signed-8 b x (xint v)))
(define-syntax (xbo-write-s2 b x v) (bvec-write-signed-16 b x (xint v)))
(define-syntax (xbo-write-s4 b x v) (bvec-write-signed-32 b x (xint v)))

(define-macro (make-buffer . descr)
  (with-module
   compiler
   (let* ((len (compute-buffer-length descr))
	  (prototypical (make-string len #\nul))
	  (%buf (gensym)))
     (let loop ((d descr)
		(fillcode '())
		(i 0))
       (if (null? d)
	   (if (null? fillcode)
	       prototypical
	       `(let ((,%buf (clone ,prototypical)))
		  ,@(reverse fillcode)
		  ,%buf))
	   (let ((ic (compile (cadr d) $envt $envt 'value)))
	     (if (compile-time-const? ic)
		 (let (((v <fixnum>) (compile-time-const-value ic)))
		   ;; value is a constant... fill it in in the prototype
		   (case (car d)
		     ((u1:) (xbo-write-u1 prototypical i v))
		     ((u2:) (xbo-write-u2 prototypical i v))
		     ((u4:) (xbo-write-u4 prototypical i v))
		     ((s1:) (xbo-write-s1 prototypical i v))
		     ((s2:) (xbo-write-s2 prototypical i v))
		     ((s4:) (xbo-write-s4 prototypical i v)))
		   (loop (cddr d) fillcode (+ i (unit-length (car d)))))
		 ;; value is expr... generate code to fill it in
		 (loop (cddr d)
		       (cons `(,(name-for-unit-writer (car d))
			       ,%buf ,i ,(cadr d))
			     fillcode)
		       (+ i (unit-length (car d)))))))))))
			      

(define (name-for-unit-writer kwd)
  (cadr (assq kwd
	      '((u1: xbo-write-u1)
		(u2: xbo-write-u2)
		(u4: xbo-write-s4)
		(s1: xbo-write-s1)
		(s2: xbo-write-s2)
		(s4: xbo-write-s4)))))

(define (compute-buffer-length descr)
  (let loop ((len 0)
	     (d descr))
    (if (null? d)
	len
	(loop (+ len (unit-length (car d))) (cddr d)))))

(define (id->string (id <fixnum>))
  (let ((b (bvec-alloc <string> 5)))
    (xbo-write-u4 b 0 id)
    b))

(define (u32->string val)
  (let ((b (bvec-alloc <string> 5)))
    (xbo-write-u4 b 0 val)
    b))


(define (card16->string (val <fixnum>)) ; in a valuelist, elems are 4 bytes
  (let ((b (bvec-alloc <string> 5)))
    (xbo-write-u4 b 0 val)
    b))

(define (int16->string (val <fixnum>)) ; as in a valuelist
  (let ((b (bvec-alloc <string> 5)))
    (xbo-write-s4 b 0 val)
    b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; creates a <vector>

(define (unpack-list-of (count <fixnum>) (unpack1 <function>))
  (let ((v (make-vector count)))
    (let loop (((i <fixnum>) 0))
      (if (fixnum<? i count)
	  (begin
	    (vector-set! v i (unpack1))
	    (loop (add1 i)))
	  v))))

(define (unpack-list-of-str (count <fixnum>) (str <string>))
  (let ((s (open-input-string str)))
    (unpack-list-of count
                    (lambda ()
                      (let ((n (char->integer (read-char s))))
                        (read-string s n))))))
   
(define (vector->named-type type vec)
  (case type
    ((list) (vector->list vec))
    ((vector) vec)
    (else (error "Unknown result type: ~s" type))))
