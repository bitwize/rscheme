;;
;;  Pass 2 (step 2XX)
;;
;;  Migrate basic config tables, the users and components
;;

(define (distinct-string-values data)
  (distinct-values data string>? string=?))
  
(define (distinct-values data cmp<> cmp=)
   (let loop ((src (sort data cmp<>))
              (dst '())
	      (prev #f))
     (if (pair? src)
	 (if (and prev (cmp= prev (car src)))
	     (loop (cdr src) dst prev)
	     (loop (cdr src) (cons (car src) dst) (car src)))
         dst)))

;;  CMVC has two answer property types: answerAccept and answerReturn;
;;  merge them into a single "answer" property

(define (merge-properties into from)
   (table-insert!
      (property-table *application*)
      into
      (let* ((src (map (lambda (fromp)
      			(cdr (property-value-type 
			      (table-lookup (property-table *application*) 
					  (cadr fromp)))))
		      from))
             (p (make <property>
		       name: into
		       property-value-type: (cons 'enum (apply append src))
		       description: into)))
	   (for-each (lambda (fromp v-list)
	   		(for-each (lambda (v)
				     (set-owner! v p)
				     (set-info! v (car fromp)))
				  v-list)
			(table-remove! (property-table *application*) (cadr fromp)))
		     from
		     src)
	    p)))

(define (merge-answer-properties)
   (merge-properties "answer"
   		     '((accept "answerAccept")
		       (return "answerReturn"))))
		       
;;  CMVC has two prefix property types: featurePrefix and defectPrefix

(define (merge-prefix-properties)
   (merge-properties "prefix"
   		     '((defect "defectPrefix")
		       (feature "featurePrefix"))))

(define (migrate-basic-config)
   ;;
   ;; load config tables
   ;;
   (let ((x (map list (distinct-string-values 
   				(map type (cmvc-table 'Config)))))
         (dflt '()))
     ;;
     (for-each
       (lambda ((c Config))
         (let (((h <pair>) (assoc (type c) x)))
	   (set-cdr! h (cons c (cdr h)))
	   (if (string=? (default c) "yes")
	       (set! dflt (cons (cons (type c) c) dflt)))))
       (cmvc-table 'Config))
     ;;
     (for-each
        (lambda (cfg)
	  (let ((prop (car cfg))
	        (vals (cdr cfg)))
	  (if (pair? vals)
	      (let ((p (make <property>
	      		     name: prop
			     description: prop
			     property-value-type: #f)))
		(table-insert! (property-table *application*) prop p)
		(set-property-value-type!
		   p
		   (cons 'enum
		         (map (lambda ((v Config))
			        (let ((pv (make <property-value>
						owner: p
						name: (name v)
						description: (description v))))
				    (set-forward! v pv)
				    pv))
			      vals)))))))
	x)
      (for-each
        (lambda (one-default)
	   (set-default-value! 
	      (table-lookup (property-table *application*) (car one-default))
	      (forward (cdr one-default))))
	dflt)
      (merge-answer-properties)
      (merge-prefix-properties)
      (table-insert!
       (property-table *application*)
       "reference"
       (make <property>
             name: "reference"
	     description: "reference"
	     property-value-type: '<string>))
      #t))

(define (migrate-user-authority)
   ;; not implemented yet
  #t
)

(define (migrate-components)
   ;;  construct the groups
   (for-each (lambda ((c Component))
   		(let ((n (make-migrated-group c)))
		   (if (not (drop-date c))
		       (table-insert! (group-table *application*) (name n) n))
		   (set-forward! c n)))
	     (cmvc-table 'Components))
    ;;
    ;;  construct their relationships
    ;;  by finding all the rank-1 elements from the comp-members relation
    ;;
    (for-each (lambda ((m CompMember))
    		(if (eq? (rank m) 1)
		    (let ((p (forward (cmvc-component (parent-id m))))
		    	  (c (forward (cmvc-component (child-id m)))))
		      (set-child-groups! p (cons c (child-groups p)))
		      (set-parent-groups! c (cons p (parent-groups c))))))
	      (cmvc-table 'CompMembers))
    ;;
    ;; rename the "root" group to "world", and make it the world group
    ;;
    (let ((r (table-lookup (group-table *application*) "root")))
       (table-remove! (group-table *application*) "root")
       (set-name! r "world")
       (set-world-group! *application* r)
       (table-insert! (group-table *application*) (name r) r)
       #t))
       
(define (make-policy-property of . choices)
  (let loop ((s choices) (r '()))
    (if (null? s)
        (if (null? r)
	    '()
	    (list (cons 'policy r)))
	(loop (cddr s)
	      (if (string=? ((car s) of) "yes")
	          (cons (cons (cadr s) #t) r)
		  r)))))

(define (make-migrated-group (c Component))
   (make <group>
	 %alloc-area: (make-area)
	 name: (name c)
	 owner: (forward (cmvc-user (user-id c)))
	 id: (+ 100 (table-size (group-table *application*)))
	 properties: (cons (cons 'description (description c))
	                   (make-policy-property c
	 				   feature-dsr 'feature-design
					   feature-verify 'feature-validate
					   defect-dsr 'defect-design
					   defect-verify 'defect-validate))
	 audit-log: (list *audit-entry*)))

(define (migrate-one-user (u User))
   (if (string=? (login u) "InheritedAccess")
       (migration-progress "  skipping user: ~s\n" (login u))
       (begin
	(migration-progress "  migrating user: ~s\n" (login u))
	(let ((n (make-migrated-user u)))
	    (table-insert! (user-table *application*) (login u) n)
	    (set-forward! u n)))))

(define (make-migrated-user (u User))
    (make <user>
        %alloc-area: (make-area)
	name: (login u)
	id: (id u)
	full-name: (name u)
	email-addr: (address u)
	remote-hosts: (let ((byhost (make-table string=? string->hash)))
			(for-each (lambda ((h Host))
				    (if (eq? (user-id h) (id u))
					(table-insert!
					 byhost
					 (name h) ;; name of the host
					 (cons (login h)
					       (or (table-lookup byhost 
								 (name h))
						   '())))))
				  (cmvc-table 'Hosts))
			(map cons 
			     (key-sequence byhost)
			     (value-sequence byhost)))
	authorities: '()
	properties: (if (string=? (superuser u) "yes")
			(list (list 'super-user #t))
			'())
	audit-log: (list *audit-entry*)
	active-items: '()))

(define (migrate-users)
  (for-each migrate-one-user (cmvc-table 'Users))
  #t)

