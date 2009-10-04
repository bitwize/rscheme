;;
;;  a clustered-bit-vector implementations of sets
;;

(define $granule-bytes 4)
(define $granule-bits (* $granule-bytes 8))

(define $mask-vector '#(#x80 #x40 #x20 #x10 #x8 #x4 #x2 #x1))

(define (bit->mask (index <fixnum>))
  (vector-ref $mask-vector (bitwise-and index 7)))

(define (bit->byte (index <fixnum>))
  (quotient index 8))

;;
;;

(define-class <set> (<object>) :abstract)

(define-class <bit-set> (<set>)
  (cluster-list type: <list> init-value: '()))

(define-method deep-copy ((self <bit-set>))
  (make <bit-set>
        cluster-list: (map deep-copy (cluster-list self))))

(define-class <bit-cluster> (<object>)
  (first-index type: <fixnum>)        ;; in units of GRANULES
  (cluster-bits type: <byte-vector>)  ;; even # of GRANULES in length
  (num-bits type: <fixnum>))

(define-method deep-copy ((self <bit-cluster>))
  (let ((t (clone self)))
    (set-cluster-bits! t (clone (cluster-bits t)))
    t))

(define (cluster-intersect? (a <bit-cluster>) (b <bit-cluster>))
  (and (< (first-index a) (+ (first-index b) 
                             (quotient (cluster-bits b) $granule-bits)))
       (< (first-index b) (+ (first-index a) 
                             (quotient (cluster-bits a) $granule-bits)))))

(define-method num-bits ((self <set>))
  (reduce (lambda (n s)
            (+ n (num-bits s)))
          0
          (cluster-list self)))

;;

(define (in-bit-set? (set <bit-cluster>) (index <fixnum>))
  (let ((i (- index (first-index set))))
    (if (>= i 0)
	(let ((byte (bit->byte i))
	      (mask (bit->mask i))
	      (v (cluster-bits set)))
	  (if (< byte (bvec-length (bvec-length v)))
	      (eq? (bitwise-and (bvec-ref v byte) mask) mask)
	      #f))
	#f)))

(define-method write-object ((self <set>) port)
  (format port "#[<set>")
  (for-each-in-set
   self
   (lambda (bit)
     (format port " ~d" bit)))
  (format port "]"))


(define (for-each-in-set (self <set>) proc)
  (for-each (lambda ((cluster <bit-cluster>))
	      (let (((base <fixnum>) (* $granule-bits (first-index cluster))))
		(for-each-set-bit
		 (cluster-bits cluster)
		 (lambda (bit)
		   (proc (+ base bit))))))
	    (cluster-list self)))


(define-method set->list ((self <set>))
  (call-with-list-extending
   (lambda (proc)
     (for-each-in-set self proc))))

(define (policy-allocation (set <set>))
  *default-allocation-area*)

;; returns two values:  the start and the length (both in granules)

(define (policy-cluster-region (set <set>)
			       prev-pair
			       (rest <list>)
			       (gindex <fixnum>)) ;; index is in granules
  (let ((start gindex)
	(len (quotient 512 $granule-bits)))
    (if (and (pair? rest)
	     (> (+ start len) (first-index (car rest))))
	(set! len (- (first-index (car rest)) start)))
    (values start len)))

(define (make-cluster (area <allocation-area>) 
		      (start <fixnum>) 
		      (length <fixnum>))
  (make <bit-cluster>
	%alloc-area: area
	cluster-bits: (bvec-alloc-in-area area
					  <byte-vector> 
					  (* length (quotient $granule-bits 8))
					  0)
	first-index: start
	num-bits: 0))

(define-syntax macro-for-each
  (syntax-form (head ())
    (values))
  (syntax-form (head (last))
    (head last))
  (syntax-form (head (next . more))
    (head next)
    (macro-for-each head more)))

(define (for-each-set-bit (vec <byte-vector>) proc)
  (let loop (((i <fixnum>) 0)
	     ((j <fixnum>) 0))
    (if (< i (bvec-length vec))
	(let ((byte (bvec-ref vec i)))
	  (let-syntax ((check (syntax-form (bit)
				(if (not (eq? (bitwise-and 
					       byte
					       (logical-shift-right #x80 bit))
					      0))
				    (proc (fixnum+ j bit))))))
	    (if (not (eq? byte 0))
		(macro-for-each check (0 1 2 3 4 5 6 7)))
	    (loop (fixnum+ i 1)
		  (fixnum+ j 8)))))))

(define (find-cluster (set <set>) (grain <fixnum>) create?)
  (let loop ((l (cluster-list set))
	     (prev #f))
    (if (null? l)
	(if create?
	    (mechanism-insert set prev '() grain)
	    #f)
	(let (((b <bit-cluster>) (car l)))
	  (if (< grain (first-index b))
	      ;; it is before this one!
	      (if create?
		  (mechanism-insert set prev l grain)
		  #f)
	      ;; it is after the start of this one
	      (if (< grain (+ (first-index b) 
			      (quotient (bvec-length (cluster-bits b))
					$granule-bytes)))
		  ;; it is inside this one!
		  b
		  ;; it is after the end of this one
		  (loop (cdr l) l)))))))

;; the basic cluster-insertion mechanism
;; called when the given index lies between (car prev-pair) and (cdr prev-pair)
;; (neither of which may exist; prev-pair 

(define (mechanism-insert (set <set>)
			  prev-pair
			  (rest <list>)
			  (index <fixnum>))
  (or (policy-extend set prev-pair rest index)
      ;; not extended -- insert!
      (bind ((start len (policy-cluster-region set prev-pair rest index))
	     (new-cluster (make-cluster 
			   (policy-allocation set)
			   start
			   len)))
	(if prev-pair
	    (set-cdr! prev-pair (cons new-cluster rest))
	    (set-cluster-list! set (cons new-cluster rest)))
	new-cluster)))

;;
;; this function computes the policy function for extending
;; a cluster instead of creating a new one.  It is called
;; when a member ('index') is being added to a set which
;; does not currently have a cluster for the given index
;;
;; returns #f for no extension, or an extended <bit-cluster>
;;

(define (policy-extend (set <set>)
		       prev-pair
		       (rest <list>)
		       (index <fixnum>))
  #f) ;; never extend

;;

;;

(define (add-member! (set <set>) (elem <fixnum>))
  (let (((c <bit-cluster>) (find-cluster 
			    set
			    (quotient elem $granule-bits) 
			    #t)))
    (let ((v (cluster-bits c))
	  (bit (- elem (* (first-index c) $granule-bits))))
      (let* ((i (bit->byte bit))
	     (old (bvec-ref v i))
	     (new (bitwise-or old (bit->mask bit))))
	(if (eq? old new)
	    #f
	    (begin
	      (set-num-bits! c (add1 (num-bits c)))
	      (bvec-set! v i new)
	      #t))))))


(define (has-member? (set <set>) (elem <fixnum>))
  (let ((c (find-cluster 
	    set
	    (quotient elem $granule-bits)
	    #f)))
    (if c
	(let ((v (cluster-bits c))
              (bit (- elem (* (first-index c) $granule-bits))))
	  (let* ((cell (bvec-ref v (bit->byte bit)))
		 (new (bitwise-and cell (bit->mask bit))))
	    (not (eq? new 0))))
        #f))) 

(define (remove-member! (set <set>) (elem <fixnum>))
  (let ((c (find-cluster 
	    set
	    (quotient elem $granule-bits)
	    #f)))
    (if c
	(let ((v (cluster-bits c))
	      (bit (- elem (* (first-index c) $granule-bits))))
	  (let* ((i (bit->byte bit))
		 (old (bvec-ref v i))
		 (new (bitwise-and old (bitwise-not (bit->mask bit)))))
	    (if (eq? old new)
		#f
		(begin
		  (set-num-bits! c (sub1 (num-bits c)))
		  (bvec-set! v i new)
		  #t))))
	#f)))

;;;

(define-method list->set ((self <list>))
  (assert (list? self))
  (let ((set (make <bit-set>)))
    (for-each (lambda (k)
                (add-member! set k))
              self)
    set))

;;;

,(use rs.sys.multimethod)

(define-mm-generic-function union)
(define-mm-generic-function intersection)

(define (bor a b) (bitwise-or a b))
(define (band a b) (bitwise-and a b))

;;

(define-method union ((a <empty-list>) (b <empty-list>))
  '())

(define-method union ((a <empty-list>) (b <pair>))
  b)

(define-method union ((a <pair>) (b <empty-list>))
  a)

(define-method union ((a <pair>) (b <pair>))
  (union (list->set a) (list->set b)))

(define-method union ((a <pair>) (b <set>))
  (union (list->set a) b))

(define-method union ((a <set>) (b <pair>))
  (union a (list->set b)))

(define-method union ((a <set>) (b <set>))
  (bitset-logical-operation a b bor))

;;

(define-method intersection ((a <empty-list>) (b <list>))
  '())

(define-method intersection ((a <list>) (b <empty-list>))
  '())

(define-method intersection ((a <fixnum>) b)
  (intersection (list a) b))

(define-method intersection ((a <object>) (b <fixnum>))
  (intersection a (list b)))

(define-method intersection ((a <pair>) (b <pair>))
  (intersection (list->set a) (list->set b)))

(define-method intersection ((a <pair>) (b <set>))
  (intersection (list->set a) b))

(define-method intersection ((a <set>) (b <pair>))
  (intersection a (list->set b)))

(define-method intersection ((a <set>) (b <set>))
  (bitset-logical-operation a b band))

;;

(define (bitset-logical-operation (a <bit-set>) (b <bit-set>) op)
  (let ((t (deep-copy a)))
    (for-each-in-set b (lambda (bit) (add-member! t bit)))
    (cond
     ((eq? op bor)
      t)
     (else
      (let ((result (make <bit-set>)))
        (for-each-in-set t
                         (lambda (bit)
                           (let ((i (if (has-member? a bit) 1 0))
                                 (j (if (has-member? b bit) 1 0)))
                             (if (eq? (op i j) 1)
                                 (add-member! result bit)))))
        result)))))


#|
(define (bitset-logical-operation (a <bit-set>) (b <bit-set>) op)
  ;; the outer loop crawls over the clusters, finding spans of granules
  (let loop ((ilist (cluster-list a))
             (jlist (cluster-list b))
             (result '()))
    (cond
     ((and (null? ilist) (null? jlist))
      (reverse result))
     ((null? ilist)
      (if (eq? (op 0 1) 0)
          (reverse result)
          (loop '() (cdr jlist) (cons (cluster-op op #f (car jlist)) result))))
     ((null? jlist)
      (if (eq? (op 1 0) 0)
          (reverse result)
          (loop (cdr ilist) '() (cons (cluster-op op (car ilist) #f) result))))
     ;; check to see if the next spans intersect
     ((if (cluster-intersect? (car ilist) (car jlist))
          (format #t "intersects ~s ~s\n" (car ilist) (car jlist)))
      
        (if (< (first-index (car ilist))
               (first-index (car jlist)))
            (loop (cdr ilist) jlist result)
            (loop ilist (cdr jlist) result)))))
|#
