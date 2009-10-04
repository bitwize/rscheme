

(define-api (make-filesystem (name <string>)
			     (owner <user>)
			     (group <group>))
   (if (table-lookup (file-system-table *application*) name)
       (error "~a: filesystem exists" name)
       (bind ((area (make-area))
       	      (fs (make <file-system>
		       %alloc-area: area
       		       name: name
		       snapshot-table: (make-table string=? string->hash)
		       root-directory: #f
		       owner: owner
		       group: group
		       properties: '()
		       audit-log: '()))
	     (root (make-dir group)))
	  (set-root-directory! fs root)
	  (table-insert! (file-system-table *application*) name fs)
	  fs)))

;;
;;  create a filesystem that (initially) shares all structure
;;  with another filesystem
;;

(define-api (make-shared-filesystem (name <string>)
				    (owner <user>)
				    (group <group>)
				    (based-on <file-system>))
  (if (table-lookup (file-system-table *application*) name)
      (error "~a: filesystem exists" name)
      (bind ((area (make-area))
	     (fs (make <file-system>
		       %alloc-area: area
       		       name: name
		       snapshot-table: (make-table string=? string->hash)
		       root-directory: #f
		       owner: owner
		       group: group
		       properties: '()
		       audit-log: '()))
	     (root (make-dir group)))
	;;
	(set-root-directory!
	 fs
	 (get-shared-node fs based-on (root-directory based-on)))
	;;
	(table-insert! (file-system-table *application*) name fs)
	fs)))

(define-api (set-filesystem-policy (fs <file-system>)
				   (policy <symbol>)
				   (value <boolean>))
  (if (not (memq policy '(require-reasons want-snapshot)))
      (error "policy `~s' is not recognized" policy))
  ;;
  (let ((c (or (assq 'policy (properties fs))
	       (let ((x (cons 'policy '())))
		 (set-properties! fs (cons x (properties fs)))
		 x))))
    (let ((p (assq policy (cdr c))))
      (if p
	  (set-cdr! p value)
	  (set-cdr! c (cons (cons policy value) (cdr c))))
      (values))))
