
(define-api (make-current-snapshot (name <string>)
			           (fs <file-system>)
				   (attribs <list>))
   (if (table-lookup (snapshot-table fs) name)
       (error "~a: snapshot already exists" name))
   (let ((snap (make <snapshot>
		    name: name
		    %alloc-area: (object-allocation-area fs)
		    versioned-object: fs
		    node-version-map: (make-snapshot-map (root-directory fs) 
		    					 current-version)
		    properties: (cons (cons 'created *audit-entry*)
				      attribs))))
    (table-insert! (snapshot-table fs) name snap)
    snap))

(define-api (make-empty-snapshot (snap-name <string>)
				 (fs <file-system>)
				 (attribs <list>))
  (if (table-lookup (snapshot-table fs) snap-name)
      (error "~a: snapshot already exists" snap-name))
  ;;
  (let ((snap (make <snapshot>
		    name: snap-name
		    %alloc-area: (object-allocation-area fs)
		    versioned-object: fs
		    node-version-map: (make-snapshot-map
				       (root-directory fs)
				       (lambda (node)
					 (value
					  (string->leaf 
					   (versions node)
					   "1.1"))))
		    properties: (cons (cons 'created *audit-entry*)
				      attribs))))
    (table-insert! (snapshot-table fs) snap-name snap)
    snap))

(define-api (make-based-snapshot (snap-name <string>)
				 (fs <file-system>)
				 (attribs <list>)
				 (based-on <snapshot>))
  (if (table-lookup (snapshot-table fs) snap-name)
      (error "~a: snapshot already exists" snap-name))
  ;;
  (if (or (not (assq 'state (properties based-on)))
	  (not (memq (cdr (assq 'state (properties based-on)))
		     '(committed complete))))
      (error "~a: not committed" (name based-on)))
  ;;
  (let ((snap (make <snapshot>
		    name: snap-name
		    %alloc-area: (object-allocation-area fs)
		    versioned-object: fs
		    node-version-map: (copy-snapshot-map 
				       (node-version-map based-on))
		    properties: (cons (cons 'created *audit-entry*)
				      attribs))))
    (table-insert! (snapshot-table fs) snap-name snap)
    snap))

(define (copy-snapshot-map src-tbl)
  (let ((tbl (make-table eq? integer->hash)))
    (table-for-each
     src-tbl
     (lambda (h k v)
       (table-install! tbl h k v)
       (values)))
    tbl))

(define (make-snapshot-map root op)
   (let ((tbl (make-table eq? integer->hash)))
      (let loop ((n root))
	;; the `op' procedure takes a node and returns
	;; an appropriate node version
        (let (((v <node-version>) (op n)))
	    (table-insert! tbl (id n) v)
	    (if (instance? v <directory-version>)
	        (for-each loop (map cdr (contents v))))))
      tbl))

(define-method state ((snap <snapshot>))
  (let ((a (assq 'state (properties snap))))
    (if a
	(cdr a)
	'undef)))

(define (snapshot-state (s <snapshot>))
  (state s))
  
(define (set-snapshot-state! (snap <snapshot>) state)
  (assert (memq state '(active committed)))
  (let ((a (assq 'state (properties snap))))
    (assert a)
    (set-cdr! a state)
    (values)))

(define (get-active-integration-requests fs (change-reqs <list>))
  (let ((ireq-list '()))
    (for-each
     (lambda ((cr <change-request>))
       (for-each
	(lambda (wi)
	  (if (and (instance? wi <integration-request>)
		   (eq? (file-system wi) fs))
	      (set! ireq-list (cons wi ireq-list))))
	(active-items cr)))
     change-reqs)
    ireq-list))


;;

(define-api (snapshot-commit (snap <snapshot>))
  (if (not (eq? (snapshot-state snap) 'active))
      (error "~a: snapshot in filesys ~a is in state `~a', not active"
	     (name snap)
	     (name (versioned-object snap))
	     (snapshot-state snap)))
  ;;
  (let ((ext (assq 'extend (properties snap)))
	(fs (versioned-object snap)))
    ;;
    (set-snapshot-state! snap 'committed)
    (set-properties! snap (cons (cons 'committed *audit-entry*)
				(properties snap)))
    ;;
    (if ext
	(let ((ireqs (apply append
			    (map (curry get-active-integration-requests fs)
				 (cdr ext)))))
	  (for-each work-item-close ireqs)))))
