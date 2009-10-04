
(define (find-leaf (map <version-map>) path)
    (find-leaf* (latest-root-branch map) path))

(define (string->leaf (self <version-map>) (str <string>))
   (find-leaf self (map string->number (string-split str #\.))))

(define (find-leaf* (n <version-tree-node>) path)
    (let ((seek (car path)))
	(let loop ((x n))
	    (if (eq? (tag x) seek)
	    	(if (null? (cdr path))
		    x
		    (find-leaf* (latest-child x) (cdr path)))
		(loop (predecessor x))))))

(define (delete-leaf! (tree <version-map>) (leaf <version-tree-node>))
    (error "delete-leaf!: not implemented"))
	
(define (new-leaf (based-on <version-tree-leaf>) value)
   (if (eq? based-on (latest-child (parent based-on)))
       ;;
       ;; based on the last leaf in this branch, so just extend the branch
       ;;
       (let ((new-leaf (make <version-tree-leaf>
       			     parent: (parent based-on)
			     tag: (add1 (tag based-on))
			     value: value
			     predecessor: (latest-child (parent based-on)))))
	 (set-latest-child! (parent based-on) new-leaf)
	 new-leaf)
	;;
	;; there is already a leaf based on that leaf, so
	;; create a new branch
	;;
	(let* ((new-branch (make <version-tree-branch>
				predecessor: (latest-child based-on)
				tag: (if (latest-child based-on)
					 (add1 (tag (latest-child based-on)))
					 1)
				parent: based-on))
	      (new-leaf (make <version-tree-leaf>
	      		      parent: new-branch
			      value: value)))
	   (set-latest-child! new-branch new-leaf)
	   (set-latest-child! based-on new-branch)
	   new-leaf)))

(define (leaf-name (leaf <version-tree-leaf>))
  (let loop ((name '())
  	     (node leaf))
    (if (instance? node <version-map>)
        name
	(loop (cons (tag node) name) (parent node)))))

(define (version-tag->string (tag <version-tree-leaf>))
   (string-join #\. (map number->string (leaf-name tag))))

(define-method to-string ((self <version-tree-leaf>))
  (version-tag->string self))

(define (make-version-map value)
    (let* ((b (make <version-tree-branch>
		    parent: #f))
	(m (make <version-map>
		    latest-root-branch: b))
        (new-leaf (make <version-tree-leaf>
	      		      parent: b
			      value: value)))
     (set-latest-child! b new-leaf)
     (set-parent! b m)
     (values m new-leaf)))

(define-method print ((self <version-map>))
  (for-each-version
    self
    (lambda (path node)
      (format #t "~a => ~s\n" path (value node))))
    ; (print-version-map '() (latest-root-branch self))
   self)

#|
(define	(print-version-map name-path node)
   (if node
       (let ((p (cons (tag node) name-path)))
	(print-version-map name-path (predecessor node))
	(if (instance? node <version-tree-leaf>)
	    (format #t "~a => ~s\n" 
	    	    (string-join #\. (map number->string (reverse p)))
		    (value node)))
	(print-version-map p (latest-child node)))))
|#

(define	(for-each-version vmap proc)
  (let loop ((name-path '())
  	     (node (latest-root-branch vmap)))
   (if node
       (let ((p (cons (tag node) name-path)))
	(loop name-path (predecessor node))
	(if (instance? node <version-tree-leaf>)
	    (proc (string-join #\. (map number->string (reverse p))) node))
	(loop p (latest-child node))))))

