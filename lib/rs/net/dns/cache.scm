
,(use tables)

(define *cache* (make-table string=? string->hash))
 
;; The cache is a table. The logical structure is such that a query
;; maps onto a rr set (resource record set). The internal structure of
;; the cache however, doesn't reflect this basic structure
;; however. The keys are roughly queries; a query is converted to a
;; string before it is used as a key. The values in the table are
;; <ent> objects: (expire-time rdatal) where expire-time is the
;; expiration time of the rr set and rdatal is the list of rdatum that
;; comprise the rr set. So from a pair in the table, that is a <query>
;; and <ent>, an rr set can be constructed (see the function
;; query+ent->rrs). An rr set is simply a list of resource
;; records. So, you give the cache a query, and it gives you a list of
;; rr's (or #f if there are no matches...). You can add an rr to the
;; cache, and it takes care of adding the rdatum to the correct <ent>.

;; The value side of the cache table.
;;
;; expire-time  : The expiration time of the rr set.
;;                may be an absolute <time> if we are caching the
;;                information from somewhere else, or a TTL if
;;                we are authoritative
;;
;; rdatal       : A list of rdata that comprise the rr set.

(define-generic-function expire-time)

(define-class <ent> (<object>) expire-time rdatal)

(define-method write-object ((self <ent>) port)
  (format port "#[<ent> ~s ~s]"
	  (expire-time self)
	  (rdatal self)))

;; (def (print-cache)
;;   (let ((c '()))
;;     (table-for-each *cache*
;; 		    (fn (code k v)
;; 		      (set! c (cons (list k v) c))))
;;     c))

(def (print-cache)
  (table-for-each *cache*
		  (fn (code key value)
		    (if (eq? 'NX (rdatal value))
			(begin
			  (format #t "~a~%" key)
			  (format #t "expiration: ~a~%" (expire-time value))
			  (format #t "    NXDOMAIN~%~%"))
			(begin
			  (format #t "~a~%" key)
			  (format #t "expiration: ~a~%" (expire-time value))
			  (for-each (fn (rdata) (format #t "    ~a~%" rdata))
				    (rdatal value))
			  (format #t "~%")))))
  (format #t "~%")
  (format #t "Size of cache: ~a~%" (table-size *cache*)))
  
				

;; Convert a ttl to an expiration time

(def (ttl->exp t)
  (time+interval (time) (seconds->interval t)))

;; Convert a rr to a simple ent.

(def (rr->ent r)
  (make <ent>
    expire-time: (ttl->exp (ttl r))
    rdatal: (list (rdata r))))

;; Convert a query to a string suitable for use as a table key.

(def (q->k q)
  (pr "~a ~a ~a"
      (string-downcase (name q))
      (type q)
      (class q)))

;; Shorthand interface to the table.

(def (tblget q)
  (table-lookup *cache* (q->k q)))

(def (tblchk q)
  (table-key-present? *cache* (q->k q)))

(def (tbladd q v)
  (table-insert! *cache* (q->k q) v)
  v)

(def (tblrem q)
  (table-remove! *cache* (q->k q)))

;; Interface to the cache

;; Test if an entry has expired

(define (exp? ent)
  (and (instance? (expire-time ent) <time>)
       (time>? (time) (expire-time ent))))

;; Convert a query and it's corresponding <ent> to an rr set.

;; FIXME: We shouldn't need the call to 'inexact->exact' in
;; calculating the ttl below... The expression:
;; 	(- (expire-time e) (current-seconds))
;; should return an integer...

(define (ttl-left (self <ent>))
  (let ((x (expire-time self)))
    (if (instance? x <time>)
        (inexact->exact (interval->seconds (time-time x (time))))
        x)))

(def (query+ent->rrs q e)

  (let ((name  (name q))
	(type  (type q))
	(class (class q))
	(ttl   (ttl-left e)))   ; compute remaining TTL

    (map (lambda (rdata)
	   (make <rr>
	     name:  name
	     type:  type
	     class: class
	     ttl:   ttl
	     rdata: rdata))
	 
	 (rdatal e))))

;; Retrieve a rr set from the cache

(define-class <zone> (<object>)
  zdata)


(def (cache-get q)
  (let ((t (tblget q)))
    (cond ((false? t) ;; It's not in the cache
	   #f)
          ((instance? t <zone>)
           t)
	  ((exp? t) ;; It's here, but expired
	   (tblrem q)
	   #f)
	  ((nx? t) ;; A negative result has been cached
	   'NX)
	  (else ;; Return the rr set
	   (query+ent->rrs q t)))))

;; Add an rr to the cache

;; TODO: when adding an item to a set, check it it is already a member.

(def (cache-add q r)
  ;; returns the <ent> that was added
  (let ((t (tblget q)))
    (cond ((false? t) ;; It's not in the cache
	   (tbladd q (rr->ent r)))
	  ((nx? t) ;; Strange but possible... [1]
	   (tblrem q)
	   (tbladd q (rr->ent r)))
	  ((exp? t) ;; In the cache, but expired
	   (tblrem q)
	   (tbladd q (rr->ent r)))
	  (else ;; Add this rdatum to the list
	        ;; if it's not already a member
	   (set-expire-time! t (ttl->exp (ttl r)))
	   (if (not (member (rdata r) (rdatal t)))
	       (set-rdatal! t (cons (rdata r)
				    (rdatal t))))
           t))))



;; [1] Strange because, this query is in the cache with a negative
;; result (NXDOMAIN). Now, (presumably moments later) the name
;; exists... A rare occurance.
	
;; Cache a negative result (NXDOMAIN)

(def (cache-nx q ttl)
  (tbladd q (make <ent>
	      expire-time: (ttl->exp ttl)
	      rdatal: 'NX)))

(def (nx? o)
  (and (instance? o <ent>)
       (eq? 'NX (rdatal o))))

(def (cache-add-rr rr)
  (cache-add (make-query (name rr) (type rr) (class rr)) rr))

(def (cache-add-rr/authoritative rr)
  (let ((ent (cache-add (make-query (name rr) (type rr) (class rr)) rr)))
    (set-expire-time! ent (ttl rr))
    ent))
    
  






;; Convert a query to a hash table string key

;; (def (q->k q)
;;   (pr "%s %s %s" (string-downcase (query-name q))
;;                  (query-type q)
;; 		 (query-class q)))

;; (def (cache-add r)
  
;;   (cond ((false? result)
;; 	 (hashadd (q->k (query (rr-name r)
;; 			       (rr-type r)
;; 			       (rr-class r)))
;; 		  (tblent (ttl->exp (rr-ttl r))
;; 			  (list (rr-rdata r)))))
;; 	((nxdomain? result)
;; 	 (hashrem (q->k (query (rr-name r)
;;                                (rr-type r)
;;                                (rr-class r))))
;; 	 (cache-add r))
;; 	((expired? result)
;; 	 (hashrem (q->k (query (rr-name r)
;;                                (rr-type r)
;;                                (rr-class r))))
;; 	 (cache-add r))
;; 	(else
;; 	 (hashadd (q->k (query (rr-name r)
;;                                (rr-type r)
;;                                (rr-class r)))
;; 		  (tblent (ttl->exp (rr-ttl r))
;; 			  (cons (rr-rdata r)
;; 				(tblent-rdata result)))))))

(define (dns-set name type class ttl rdata)
  (cache-add-rr
   (make <rr>
         name: name
         type: type
         class: class
         ttl: ttl
         rdata: rdata)))

(define (cache-add-zone name v)
  (table-insert! *cache* 
                 (q->k (make <query>
                             name: name
                             type: 'AXFR
                             class: 'IN))
                 (make <zone>
                       zdata: v)))


(define (soa-rr? rr)
  (and (eq? (type rr) 'SOA)
       (instance? (rdata rr) <soa>)))
