
(define-api (make-group (name <string>) (parents <list>) (lead <user>))
   (if (table-lookup (group-table *application*) name)
       (error "~a: group already exists" name))
   (let ((g (make <group>
		  %alloc-area: (make-area)
   		  name: name
		  id: (+ 100 (table-size (group-table *application*)))
		  parent-groups: parents
		  owner: lead
		  audit-log: '())))
      (for-each (lambda (p)
      		   (set-child-groups! p (cons g (child-groups p))))
		parents)
      (table-insert! (group-table *application*) name g)
      g))
