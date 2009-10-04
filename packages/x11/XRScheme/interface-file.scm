
(define (load-interface-file path owner)
  (call-with-input-file
      path
    (lambda (port)
      ;; bind imported objects
      (let ((envt (make-table eq? symbol->hash)))
	;; 
	;; for each imported name
	;;
	(for-each (lambda (import-name)
		    (table-insert! envt 
				   import-name
				   (if (eq? import-name 'owner)
				       owner
				       (eval import-name *self*))))
		  (read port))
	;;
	;; build the local data structure
	;;
	(build envt (read port))
	;;
	;; for each exported value
	;;
	(for-each (lambda (defn)
		    (let ((extern-name (car defn))
			  (local-name (cadr defn)))
		      (format #t "binding owner.~s => ~s\n" extern-name local-name)
		      (let ((setter (slot-setter-for-symbol extern-name)))
			(eval `(,setter ',owner 
					',(table-lookup envt local-name))
			      *self*))))
		  (read port))))))

