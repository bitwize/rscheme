
(define (encode-runs key-vec index num-runs run-widths)
  (let ((encoded (bvec-alloc <byte-vector> (* num-runs 4))))
    (let loop ((i (* num-runs 4))
	       (w run-widths)
	       (j (vector-length key-vec)))
      (if (null? w)
	  encoded
	  (let ((k (vector-ref key-vec (vector-ref index (- j (car w))))))
	    (bvec-write-unsigned-16 encoded (- i 4) k)
	    (bvec-write-unsigned-16 encoded (- i 2) (car w))
	    (loop (- i 4) (cdr w) (- j (car w))))))))
      

(define (permute-val-vec val-vec index num-runs run-widths)
  (let ((permuted (make-vector num-runs)))
    (let loop ((i num-runs)
	       (w run-widths)
	       (j (vector-length val-vec)))
      (if (null? w)
	  permuted
	  (let ((v (vector-ref val-vec (vector-ref index (- j (car w))))))
	    (vector-set! permuted (- i 1) v)
	    (loop (- i 1) (cdr w) (- j (car w))))))))


(define (pack-keys/rle key-vec val-vec)
  (let* ((n (vector-length key-vec))
	 (index (list->vector (range n))))
    ;
    (vector-sort! index
		  (lambda ((a <fixnum> :trust-me)
			   (b <fixnum> :trust-me))
		    (fixnum<? (vector-ref key-vec a)
			      (vector-ref key-vec b))))
    ;
    (let loop ((i 0)
	       (run-lengths '())
	       (num-runs 0))
      (if (eq? i n)
	  (values (encode-runs key-vec index num-runs run-lengths)
		  (if val-vec
		      (permute-val-vec val-vec index num-runs run-lengths)
		      #f))
	  (let ((start-val (vector-ref key-vec (vector-ref index i))))
	    (let run-loop ((run-len 1))
	      (if (and (< (+ run-len i) n)
		       (eq? (vector-ref key-vec 
					(vector-ref index (+ i run-len)))
			    (+ start-val run-len))
		       (or (not val-vec)
			   (eq? (vector-ref val-vec
					    (vector-ref index (+ i run-len)))
				(vector-ref val-vec (vector-ref index i)))))
		  (run-loop (+ run-len 1))
		  (loop (+ i run-len)
			(cons run-len run-lengths)
			(+ num-runs 1)))))))))

;;;

(define (rle-find-index (tbl <byte-vector>) (key-code <fixnum>))
  (let (((n <fixnum>) (bvec-length tbl)))
    (if (eq? n 0)
        #f
        (let search (((start <fixnum>) 0)
                     ((end <fixnum>) (fixnum-quotient n 4)))
          (let* (((i <fixnum>) (div2 (fixnum+ start end)))
                 ((v <fixnum>) (bvec-read-unsigned-16
                                tbl
                                (fixnum* i 4))))
            (if (fixnum>? v key-code)
                (if (eq? i end)
                    #f
                    (search start i))
                (if (fixnum<? key-code
                              (fixnum+ v (bvec-read-unsigned-16
                                          tbl
                                          (fixnum+ (fixnum* i 4) 2))))
                    i
                    (if (eq? i start)
                        #f
                        (search i end)))))))))

;;;

(define-class <packed-char-table> (<table>)
  (cache type: <vector>)
  (full-table type: <byte-vector>)
  (tbl-size type: <fixnum>))

(define-syntax (make-cache)
  (make-vector 16))

(define (key-code-sequence (self <packed-char-table>))
  (expand-rle->vector (full-table self)
		      (tbl-size self)
		      (lambda (k i)
			k)))

(define (expand-rle->vector (ft <byte-vector>)
			    (num <fixnum>)
			    (proc <function>))
  (let (((r <vector>) (make-vector num)))
    (let loop ((i 0)
	       (k 0)
	       (vti 0))
      (if (< i (bvec-length ft))
	  (let ((code (bvec-read-unsigned-16 ft i))
		(n (bvec-read-unsigned-16 ft (+ i 2))))
	    (let run-loop ((j n))
	      (if (eq? j 0)
		  (loop (+ i 4) (+ k n) (+ vti 1))
		  (let ((j (sub1 j)))
		    (vector-set! r (+ j k) (proc (+ j code) vti))
		    (run-loop j)))))
	  r))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;    Common methods
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-method table-size ((self <packed-char-table>))
  (tbl-size self))

(define-method key-sequence ((self <packed-char-table>))
  (vector-map integer->char (key-code-sequence self)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;    Character Sets
;;;
;;;       Domain: characters
;;;       Range:  { #t, #f }
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-class <char-set> (<packed-char-table>))

(define (make-char-set)
  (make <char-set>
	full-table: (bvec-alloc <byte-vector> 0)
	tbl-size: 0
	cache: (make-cache)))

(define (set-char-set-from-elemv! (self <char-set>) (set <vector>))
  (set-cache! self (make-cache))
  (set-full-table! self (pack-keys/rle set #f))
  (values))

(define-method table-insert! ((self <char-set>) (key <char>) (val <boolean>))
  (let ((was (table-remove! self key)))
    (if val
	(begin
	  (set-char-set-from-elemv! self
				    (vector-append
				     (key-code-sequence self)
				     (vector (char->integer key))))
	  (set-tbl-size! self (add1 (tbl-size self)))))
    was))

(define-method table-remove! ((self <char-set>) (key <char>))
  ; it might be worth checking to see if the keycode is present, first
  (let* ((v (key-code-sequence self))
	 (i (vmemq (char->integer key) v)))
    (if i
	(begin
	  (set-char-set-from-elemv! self
				    (vector-append (subvector v 0 i)
						   (subvector v (+ i 1))))
	  (set-tbl-size! self (sub1 (tbl-size self)))
	  #t)
	#f)))

(define-method table-lookup ((self <char-set>) (key <char>))
  (let ((i (rle-find-index (full-table self) (char->integer key))))
    (if i
	#t
	#f)))

(define-method value-sequence ((self <char-set>))
  (make-vector (tbl-size self) #t))

(define (char-set-union! dest . rest)
  (let ((t (apply char-set-union dest rest)))
    (set-full-table! dest (full-table t))
    (set-tbl-size! dest (tbl-size t))
    (set-cache! dest (cache t))
    dest))
    
(define (char-set-union . sets)
  (let loop ((set (bvec-alloc <byte-vector> 0))
             (s sets))
    (if (null? s)
        (make <char-set>
              full-table: set
              tbl-size: (count-members set)
              cache: (make-cache))
        (loop (union-merge-ranges set (full-table (car s)))
              (cdr s)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;    Character Tables
;;;
;;;       Domain: characters
;;;       Range:  objects
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-class <char-table> (<packed-char-table>)
  (values-table type: <vector>))

(define (make-char-table)
  (make <char-table>
	full-table: (bvec-alloc <byte-vector> 0)
	values-table: '#()
	tbl-size: 0
	cache: (make-cache)))

(define-method members->char-table ((a-list <pair>))
  (let ((t (make-char-table))
	(v (list->vector a-list)))
    (set-char-tbl-from-elemv! t 
			      (vector-map (lambda (tuple)
					    (char->integer (car tuple)))
					  v)
			      (vector-map cdr v))
    t))

(define (set-char-tbl-from-elemv! (self <char-table>)
				  (keys <vector>)
				  (vals <vector>))
  (set-cache! self (make-cache))
  (bind ((k v (pack-keys/rle keys vals)))
    (set-full-table! self k)
    (set-values-table! self v)
    (values)))

(define-method table-insert! ((self <char-table>) (key <char>) val)
  (let ((was (table-remove! self key)))
    (set-char-tbl-from-elemv! self
			      (vector-append
			       (key-code-sequence self)
			       (vector (char->integer key)))
			      (vector-append
			       (value-sequence self)
			       (vector val)))
    (set-tbl-size! self (add1 (tbl-size self)))
    was))

(define-method table-remove! ((self <char-table>) (key <char>))
  ; it might be worth checking to see if the keycode is present, first
  (let* ((v (key-code-sequence self))
	 (vt (value-sequence self))
	 (i (vmemq (char->integer key) v)))
    (if i
	(begin
	  (set-char-tbl-from-elemv! self
				    (vector-append (subvector v 0 i)
						   (subvector v (+ i 1)))
				    (vector-append (subvector vt 0 i)
						   (subvector vt (+ i 1))))
	  (set-tbl-size! self (sub1 (tbl-size self)))
	  (vector-ref vt i))
	#f)))

(define-method table-lookup ((self <char-table>) (key <char>))
  (let ((i (rle-find-index (full-table self) (char->integer key))))
    (if i
	(vector-ref (values-table self) i)
	#f)))

(define-method value-sequence ((self <char-table>))
  (expand-rle->vector (full-table self)
		      (tbl-size self)
		      (lambda (k i)
			(vector-ref (values-table self) i))))

;;;

(define-method members->char-set ((self <string>))
  (members->char-set (string->list self)))

(define-method members->char-set ((self <vector>))
  (make <char-set>
	full-table: (pack-keys/rle (vector-map char->integer self) #f)
	tbl-size: (vector-length self) ;; what about dups?
	cache: (make-cache)))

(define-method members->char-set ((self <list>))
  (members->char-set (list->vector self)))

(define (range->char-set (from <char>) (to <char>))
  (ranges->char-set (list (list from to))))

(define (ranges->char-set ranges)
  (let loop ((set (bvec-alloc <byte-vector> 0))
             (count 0)
             (r ranges))
    (if (null? r)
        (make <char-set>
              full-table: set
              tbl-size: count
              cache: (make-cache))
        (bind ((set count (union-merge-ranges set (range1->rle (car r)))))
          (loop set count (cdr r))))))
    
(define-method range1->rle ((self <char>))
  (let ((encoded (bvec-alloc <byte-vector> 4)))
    (bvec-write-unsigned-16 encoded 0 (char->integer self))
    (bvec-write-unsigned-16 encoded 2 1)
    encoded))
  
(define-method range1->rle ((self <pair>))
  (let ((from-i (char->integer (car self)))
        (to-i (char->integer (cadr self))))
    (if (< to-i from-i)
        (bvec-alloc <byte-vector> 0)
        (let ((n (- to-i from-i -1))
              (encoded (bvec-alloc <byte-vector> 4)))
          (bvec-write-unsigned-16 encoded 0 from-i)
          (bvec-write-unsigned-16 encoded 2 n)
          encoded))))
          

;;;

(define (count-members (ft <byte-vector>))
  (let loop ((i 2)
             (n 0))
    (if (< i (bvec-length ft))
        (loop (+ i 4)
              (+ n (bvec-read-unsigned-16 ft i)))
        n)))

(define (union-merge-ranges (A <byte-vector>) (B <byte-vector>))
  (cond
   ((eq? (bvec-length A) 0)
    (values B (count-members B)))
   ((eq? (bvec-length B) 0)
    (values A (count-members A)))
   (else
    (union-merge-ranges* A B))))

(define (union-merge-ranges* (A <byte-vector>) (B <byte-vector>))
  (let (((output <byte-vector>) (bvec-alloc <byte-vector>
                                            (+ (bvec-length A)
                                               (bvec-length B))))
        ((k <fixnum>) 0)
        ((run-start <fixnum>) 0)
        ((count <fixnum>) 0))
    (let-syntax ((start (syntax-form (v p)
                          (bvec-read-unsigned-16 v p)))
                 (end (syntax-form (v p)
                        (+ (bvec-read-unsigned-16 v p)
                           (bvec-read-unsigned-16 v (fixnum+ p 2)))))
                 (emit+ (syntax-form (p)
                          (set! run-start p)
                          (bvec-write-unsigned-16 output k run-start)))
                 (emit- (syntax-form (p)
                          (let (((n <fixnum>) (- p run-start)))
                            (set! count (fixnum+ count n))
                            (bvec-write-unsigned-16 output
                                                    (fixnum+ k 2)
                                                    n)
                            (set! k (fixnum+ k 4)))))
                 (succ (syntax-form (p)
                         (fixnum+ p 4)))
                 (eof? (syntax-form (v p)
                         (eq? (bvec-length v) (fixnum+ p 4)))))
      ;;
      (letrec ((a0b0 (lambda (a b)
                       (if (<= (start A a) (start B b))
                           (begin
                             (emit+ (start A a))
                             (a1b0 a b))
                           (begin
                             (emit+ (start B b))
                             (a0b1 a b)))))
               (a0b1 (lambda (a b)
                       (if (< (end B b) (start A a))
                           (begin
                             (emit- (end B b))
                             (if (eof? B b)
                                 (a0bx a)
                                 (a0b0 a (succ b))))
                           (a1b1 a b))))
               (a1b0 (lambda (a b)
                       (if (< (end A a) (start B b))
                           (begin
                             (emit- (end A a))
                             (if (eof? A a)
                                 (axb0 b)
                                 (a0b0 (succ a) b)))
                           (a1b1 a b))))
               (a1b1 (lambda (a b)
                       (if (< (end A a) (end B b))
                           (if (eof? A a)
                               (axb1 b)
                               (a0b1 (succ a) b))
                           (if (eof? B b)
                               (a1bx a)
                               (a1b0 a (succ b))))))
               ;;
               (axb0 (lambda (b)
                       (emit+ (start B b))
                       (axb1 b)))
               (axb1 (lambda (b)
                       (emit- (end B b))
                       (if (eof? B b)
                           (axbx)
                           (axb0 (succ b)))))
               (a0bx (lambda (a)
                       (emit+ (start A a))
                       (a1bx a)))
               (a1bx (lambda (a)
                       (emit- (end A a))
                       (if (eof? A a)
                           (axbx)
                           (a0bx (succ a)))))
               (axbx (lambda ()
                       'done)))
        (a0b0 0 0)))
    (values
     (if (= k (bvec-length output))
         output
         (let ((trimmed (bvec-alloc <byte-vector> k)))
           (bvec-copy trimmed 0 output 0 k)
           trimmed))
     count)))

#|
(define (f . args)
  (full-table (members->char-set (list->vector args))))

(define (t1)
  (union-merge-ranges (f #\a #\b #\c) (f #\x #\y #\z))
  (newline)
  (union-merge-ranges (f #\a #\c) (f #\d #\e #\f #\x #\y #\z))
  (newline)
  (union-merge-ranges (f #\a #\c #\e) (f #\d #\e #\f #\x #\z)))
  


,(use tables)

(define *hit-count* (make-symbol-table))
(define (bump n)
  (table-insert! *hit-count* n (+ (or (table-lookup *hit-count* n) 0) 1)))

(define (triv-merge-ranges a b)
  (define (start p)
    (if (pair? p)
        (caar p)
        999999))
  (define (end p)
    (if (pair? p)
        (+ (caar p) (cadar p))
        999999))
  (define (emit what where)
    (format #t "~s ~s\n" what where))
  ;;
  (letrec ((a0b0 (lambda (a b)
                   (bump 'a0b0)
                   (if (<= (start a) (start b))
                       (begin
                         (emit '+ (start a))
                         (a1b0 a b))
                       (begin
                         (emit '+ (start b))
                         (a0b1 a b)))))
           (a0b1 (lambda (a b)
                   (bump 'a0b1)
                   (if (< (end b) (start a))
                       (begin
                         (emit '- (end b))
                         (if (null? (cdr b))
                             (a0bx a)
                             (a0b0 a (cdr b))))
                       (a1b1 a b))))
           (a1b0 (lambda (a b)
                   (bump 'a1b0)
                   (if (< (end a) (start b))
                       (begin
                         (emit '- (end a))
                         (if (null? (cdr a))
                             (axb0 b)
                             (a0b0 (cdr a) b)))
                       (a1b1 a b))))
           (a1b1 (lambda (a b)
                   (bump 'a1b1)
                   (if (< (end a) (end b))
                       (if (null? (cdr a))
                           (axb1 b)
                           (a0b1 (cdr a) b))
                       (if (null? (cdr b))
                           (a1bx a)
                           (a1b0 a (cdr b))))))
           ;;
           (axb0 (lambda (b)
                   (bump 'axb0)
                   (emit '+ (start b))
                   (axb1 b)))
           (axb1 (lambda (b)
                   (bump 'axb1)
                   (emit '- (end b))
                   (if (null? (cdr b))
                       (axbx)
                       (axb0 (cdr b)))))
           (a0bx (lambda (a)
                   (bump 'a0bx)
                   (emit '+ (start a))
                   (a1bx a)))
           (a1bx (lambda (a)
                   (bump 'a1bx)
                   (emit '- (end a))
                   (if (null? (cdr a))
                       (axbx)
                       (a0bx (cdr a)))))
           (axbx (lambda ()
                   'done)))
    (a0b0 a b)))


(define (t)
  (triv-merge-ranges
   '((10 20) (50 20) (90 10) (120 30))
   '((20 40) (80 30) (130 10))))

(define (tx)
  (triv-merge-ranges
   '((5 1) (7 1) (9 1))
   '((6 1)))
  (triv-merge-ranges
   '((6 1))
   '((5 1) (7 1) (9 1))))
  

|#
