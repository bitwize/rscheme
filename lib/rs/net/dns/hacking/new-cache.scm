
,(use tables)

;; The main differences from the old "cache.scm" :
;;
;;   Case of names is preserved
;;   3 separate caches (RR, NX, NULL)
;;   Basic code reorganization

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Time utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def (exp? exp)
  (> (current-seconds) exp))

(def (ttl->exp ttl)
  (+ (current-seconds) ttl))

(def (exp->ttl exp)
  (- exp (current-seconds)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RR cache
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *cache* (make-table string=? string->hash))

(define-generic-function exp)

(define-class <ent> (<object>) pname exp rdatal)

(define-method write-object ((self <ent>) port)
  (format port "#[<ent> ~s ~s]"
	  (exp self)
	  (rdatal self)))

;; Shorthand interface to the table implementing the RR cache

(def (q->k q)
  (pr "~a ~a ~a"
      (string-downcase (name q))
      (type q)
      (class q)))

(def (tblget q)
  (table-lookup *cache* (q->k q)))

(def (tblchk q)
  (table-key-present? *cache* (q->k q)))

(def (tbladd q v)
  (table-insert! *cache* (q->k q) v))

(def (tblrem q)
  (table-remove! *cache* (q->k q)))

;; The RR cache interface
;;
;; cache-get
;; cache-add

(def (query+ent->rrs q e)
  (let ((name  (or (pname e) (name q)))
	(type  (type q))
	(class (class q))
	(ttl   (exp->ttl (exp e))))
    (map (fn (rd)
	   (make-rr name type class ttl rd))
	 (rdatal e))))

(def (cache-get q)
  (let ((t (tblget q)))
    (cond ((false? t)
	   #f)
	  ((exp? (exp t))
	   (tblrem q)
	   #f)
	  (else
	   (query+ent->rrs q t)))))

(def (rr->ent r)
  (make <ent>
    pname:  (some-uppercase? (name rr))
    exp:    (ttl->exp (ttl r))
    rdatal: (list (rdata r))))

(def (cache-add q r)
  (let ((t (tblget q)))
    (cond ((false? t)
	   (tbladd q (rr->ent r)))
	  ((exp? (exp t))
	   (tblrem q)
	   (tbladd q (rr->ent r)))
	  (else
	   (set-exp! t (ttl->exp (ttl r)))
	   (if (not (member (rdata r) (rdatal t)))
	       (set-rdatal! t (cons (rdata r)
				    (rdatal t))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NX cache
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def *nx-cache* (make-table string-ci=? string-ci->hash))

(def (cache-add-nx name ttl)
  (table-insert! *nx-cache* name (ttl->exp ttl)))

(def (cache-nx? name)
  (let ((t (table-lookup *nx-cache* name)))
    (cond ((false? t)
	   #f)
	  ((exp? t)
	   (table-remove! *nx-cache* name)
	   #f)
	  (else
	   #t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NULL cache
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def *null-cache* (make-table string? string->hash))

(def (cache-add-null q ttl)
  (table-insert! *null-cache* (q->k q) (ttl->exp ttl)))

(def (cache-null? q)
  (let ((t (table-lookup *null-cache* (q->k q))))
    (cond ((false? t)
	   #f)
	  ((exp? t)
	   (table-remove! *null-cache* (q->k q))
	   #f)
	  (else
	   #t))))
	  
	  
	  
		 
  
	  







































;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The NXDOMAIN cache
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def *nx-cache* (make-table string-ci=? string->hash))

(def (cache-nx? name)

  (let ((t (table-lookup *nx-cache* name)))

    (cond ((false? t)
	   #f)
	  
	  ((> (current-seconds) t)
	   (table-remove! *nx-cache* name)
	   #f)

	  (else
	   #t))))

(def (cache-add-nx name ttl)
  (table-insert! *nx-cache* name (ttl->exp ttl)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The no data cache
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def *no-data-cache* (make-table string-ci=? string->hash))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RR cache
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; (define-class <node-data> (<object>) type class ttl rdatal)


;; Tree based cache



(define-class <tree-node> (<object>) label data children)

;; name/rl : Name as a list of labels in descending order

;; ("" "com" "apple" "www")

(def (lookup name/rl node)

  (cond ((false? node)
	 #f)

	((and (null? (cdr name/rl))
	      (string-ci=? (label node)
			   (car name/rl)))
	 node)

	((not? (null? (cdr name/rl)))
	 (lookup (cdr name/rl)
		 (table-lookup (children node)
			       (car (cdr name/rl)))))))

		 
	      
	
	
	 

  (cond ((null? (cdr name/rl))
	 (children tree)



   

  (cond ((and (null? (cdr name/rl))
	      (string-ci=? (car name/rl)
			   (label tree)))
	 data)
	
	 
	 
	 
	 (table-lookup (car name/rl) (children tree)))
	((table-lookup (car name/rl) (children tree)) =>
	 
	 

	 (string=? "" (car name/rl))
	 (lookup (cdr name/rl) (children tree)))
	

	(children tree)


	




	 
  

  
