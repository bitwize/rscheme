(define-class <world-schema> (<object>)
  (sys-version type: <fixnum> init-value: 1)
  (app-version type: <fixnum> init-value: 1)
  (name type: <symbol>)
  (entity-map type: <symbol-table>))

(define-class <world> (<object>)
  (creation-time init-function: time)
  (next-hash-code type: <integer> init-value: 0)
  (world-tables init-function: make-symbol-table)
  (world-assocs init-function: make-symbol-table)
  (schema type: <world-schema>)
  (txn-area type: <allocation-area>))

(define-class <world-view> (<object>)
  (real-world type: <world>)
  (app-schema type: <world-schema>)
  underlying-storage
  entity-area-map)

;;

(define-thread-var *world*)
(define-thread-var *world-view*)

(define (current-world-view)
  *world-view*)

(define (current-world)
  *world*)

(define (with-world-view (v <world-view>) thunk)
  (thread-let ((*world* (real-world v))
	       (*world-view* v))
    (thunk)))

(define-macro (new-world-view schema . opts)
  `(make-world-view
    ,(symbol-append "*rs.db.oodb*" schema "-schema")
    ,@opts))

(define (make-world-view (schema <world-schema>) #optional file)
  (let ((w (make <world>
	     schema: (assign-pstore-ids schema)
	     txn-area: *default-allocation-area*)))
    (if file
	(let* ((ps (create-persistent-store file))
	       (a1 (default-allocation-area ps))
	       (a2 (make-allocation-area a1)))
	  (register-pstore-pivots ps schema w)
	  (set-txn-area! w a2)
	  (commit ps w)
	  (make <world-view>
	    real-world: (root-object ps)
	    app-schema: schema
	    entity-area-map: (new-entity-area-map schema ps)
	    underlying-storage: ps))
	(make <world-view>
	  real-world: w
	  entity-area-map: (new-entity-area-map schema)
	  app-schema: schema
	  underlying-storage: #f))))

;;;

(define (new-entity-area-map schema #optional ps)
  (let ((tbl (make-table eq?)))
    (table-for-each
     (entity-map schema)
     (lambda (h k v)
       (table-insert! tbl v (if ps
				(make-allocation-area
				 (default-allocation-area ps))
				*default-allocation-area*))))
       tbl))

;;;

(define-syntax build-schema-map
  (syntax-form () '())
  (syntax-form (name . more) (cons (cons (mquote name) name)
				   (build-schema-map . more))))
	       
(define (make-world-schema* (name <symbol>) members)
  (let ((tbl (make-symbol-table)))
    (for-each (lambda (m)
		(table-insert! tbl (car m) (cdr m)))
	      members)
       ;
       (make <world-schema>
	 name: name
	 entity-map: tbl)))

(define-syntax (make-world-schema name . lst)
  (make-world-schema* (mquote name) (build-schema-map . lst)))

(define-macro (define-world-schema schema . member-classes)
  `(define ,(symbol-append "*rs.db.oodb*" schema "-schema")
     (make-world-schema ,schema ,@member-classes)))

(define (commit-world (w <world>) ps)
  (if ps
      (commit ps))
  (values))
