
;;
;; an object table for persistent objects
;;

(define-class <persistent-object-table> (<table>)
  table-size
  directory)
  
(define-class <persistent-object-table-bucket> (<object>)
  pot-bucket-keys
  pot-bucket-values
  pot-bucket-hashs)

(define (pot-install! (self <persistent-object-table>) key value)
  (let ((h (persistent-object->hash key))
	(dir (directory self)))
    (format #t "install! ~d mod ~d => ~d (~s)\n"
	    h (persistent-addr-vector-length dir)
	    (modulo h (persistent-addr-vector-length dir))
	    key)
    (let ((bucket (persistent-addr-vector-ref
		  dir
		  (modulo h (persistent-addr-vector-length dir)))))
      (if bucket
	  (bucket-add! bucket key value h)
	  (let ((b (new-bucket (ptr-allocation-area self)
			       h key value 8)))
	    (persistent-addr-vector-set!
	     dir
	     (modulo h (persistent-addr-vector-length dir))
	     b))))))

(define (bucket-add! bucket key value h)
  (let* (((hv <vector>) (pot-bucket-hashs bucket))
	 (i (if (vector-ref hv (- (vector-length hv) 1))
		;;
		;; this bucket is full, grow it
		;; and install the new data at the old size offset
		;;
		(let ((i (vector-length hv)))
		  (grow-bucket! bucket (* i 2))
		  (set! hv (pot-bucket-hashs bucket))
		  i)
		;;
		;; this bucket is not full, find an empty spot
		;;
		(vector-next-index hv))))
    (vector-set! hv i h)
    (persistent-addr-vector-set! (pot-bucket-keys bucket) i key)
    (persistent-addr-vector-set! (pot-bucket-values bucket) i value)))

(define (vector-next-index (vec <vector>))
  (let loop ((i 0))
    (if (< i (vector-length vec))
	(if (vector-ref vec i)
	    i
	    (loop (+ i 1)))
	#f)))

(define (new-bucket a h k v n)
  (let ((keys (make-persistent-addr-vector a n))
	(values (make-persistent-addr-vector a n))
	(hashs (pgvec-alloc a <vector> n)))
    (vector-set! hashs 0 h)
    (persistent-addr-vector-set! keys 0 k)
    (persistent-addr-vector-set! values 0 v)
    (pmake-gvec a 
		<persistent-object-table-bucket>
		keys values hashs)))
	   
(define (grow-bucket! (bucket <persistent-object-table-bucket>) new-len)
  (let ((a (ptr-allocation-area bucket)))
    (let ((nk (make-persistent-addr-vector a new-len))
	  (nv (make-persistent-addr-vector a new-len))
	  (nh (pgvec-alloc a <vector> new-len))
	  (old-len (vector-length (pot-bucket-hashs bucket)))
	  (ok (pot-bucket-keys bucket))
	  (ov (pot-bucket-values bucket))
	  (oh (pot-bucket-hashs bucket)))
      (let loop ((i 0))
	(if (< i old-len)
	    (begin
	      (vector-set! nh i (vector-ref oh i))
	      (persistent-addr-vector-set! nk i 
					   (persistent-addr-vector-ref ok i))
	      (persistent-addr-vector-set! nv i
					   (persistent-addr-vector-ref ov i))
	      (loop (+ i 1)))
	    (begin
	      (set-pot-bucket-keys! bucket nk)
	      (set-pot-bucket-hashs! bucket nh)
	      (set-pot-bucket-values! bucket nv)))))))

(define (pot-find (self <persistent-object-table>) key)
  (let ((h (persistent->hash key))
	(dir (directory self)))
    (let ((bucket (persistent-addr-vector-ref
		  dir
		  (modulo h (persistent-addr-vector-length dir)))))
      (if bucket
	  (let ((hv (pot-bucket-hashs bucket))
		(kv (pot-bucket-keys bucket)))
	    (let loop ((i 0))
	      (if (and (< i (vector-length hv))
		       (vector-ref hv i))
		  (if (and (eq? h (vector-ref hv i))
			   (eq? (persistent-addr-vector-ref kv i) key))
		      (values bucket i)
		      (loop (+ i 1)))
		  #f)))
	  #f))))
  

(define-method table-lookup ((self <persistent-object-table>) key)
  (bind ((b i (pot-find self key)))
    (if b
	(persistent-addr-vector-ref (pot-bucket-values b) i)
	#f)))

(define-method table-insert! ((self <persistent-object-table>) key value)
  (bind ((b i (pot-find self key)))
    (if b
	(let* ((bv (pot-bucket-values b))
	       (old-value (persistent-addr-vector-ref bv i)))
	  (persistent-addr-vector-set! bv i value)
	  old-value)
	(pot-install! self key value))))

(define (persistent-object->hash p)
  (if (ptr? p)
      (persistent->hash (transient->persistent p))
      (immob->hash p)))

(define-method table-hash-function ((self <persistent-object-table>))
    persistent-object->hash)
    
(define-method table-equal-function ((self <persistent-object-table>))
    eq?)

(define (make-persistent-object-table (ps <persistent-store>)
				      (size <fixnum>))
  (let ((area (default-allocation-area ps)))
    (pmake-gvec area
		<persistent-object-table>
		0
		(make-persistent-addr-vector area size))))
