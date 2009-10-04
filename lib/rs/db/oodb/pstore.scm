
(define (register-pstore-pivots ps
				(app <world-schema>)
				(world <world>))
  (let ((tbl (make-table eq?)))
    ;; for each entity in the world, bind it
    ;; to the class in the application schema
    (table-for-each
     (entity-map (schema world))
     (lambda (h k v)
       (let ((app-class (table-lookup (entity-map app) k))
	     (page-num (vector-ref v 0))
	     (entry-num (vector-ref v 1)))
	 (if app-class
	     ;; TODO: also check for commensurate structure here...
	     (let ((vec (or (table-lookup tbl page-num)
			    (make-vector 64 #f))))
	       (vector-set! vec entry-num app-class))
	     (error "oodb entity ~s has no application class" k)))))
       ;;
       (table-for-each
	tbl
	(lambda (h k v)
	  (register-indirect-page ps k v)))
       ;;
       (register-indirect-page ps 100 (vector <world>
					      <world-schema>
					      <transaction>
					      <db-table>
					      <db-index>
					      <db-assoc>))
       ;;
       (values)))

(define (assign-pstore-ids schema)
  (let ((tbl (make-symbol-table))
	(n 0))
    (table-for-each
     (entity-map schema)
     (lambda (h k v)
       (table-insert! tbl k (vector
			     (+ 102 (logical-shift-right n 6))
			     (bitwise-and n 63)))))
       (make <world-schema>
	 app-version: (app-version schema)
	 name: (name schema)
	 entity-map: tbl)))
