
;; the return protocol here is a little wierd due to historical accident,
;; whereby find-version* signalling an error is necessary for some
;; cases, but not resolve-path should be able to implement stat(2)


(define (resolve-path (fs <file-system>)
		      (path <fs-absolute-path>)
		      (select-version <function>))
  (let loop (((s <pair>) (steps path))
             (v (select-version (root-directory fs))))
     (if (null? s)
     	 v
	 (if (instance? v <directory-version>)
	    (bind ((link (car s))
	           (link-name link-vers (if (pair? link)
		   	                    (values (car link) (cdr link))
				            (values link #f)))
		   (node (fs-dir-lookup v link-name)))
	      (if node
		  (loop (cdr s)
			(if link-vers
			    (value (string->leaf (versions node) link-vers))
			    (select-version node)))
		  (if (null? (cdr s))
		      (values #f #f)
		      (values #f "~a: ~a no such link" path link-name))))
	    (values #f "~a: not a directory" path)))))

(define-method find-version* ((self <snapshot>)
			      (path <fs-absolute-path>))
   (bind ((r m #rest a (resolve-path (versioned-object self)
				     path
				     (let ((tbl (node-version-map self)))
				       (lambda ((n <node>))
					 (table-lookup tbl (id n)))))))
     (if r
	 r
	 (if m
	     (apply error m a)
	     #f))))

(define-method find-version* ((self <file-system>)
			      (path <fs-absolute-path>))
  (bind ((r m #rest a (resolve-path self path current-dir-version)))
    (if r
	r
	(if m
	    (apply error m a)
	    #f))))

(define-method stat-node ((self <snapshot>)
			  (path <fs-absolute-path>))
  (or (resolve-path (versioned-object self)
		    path
		    (let ((tbl (node-version-map self)))
		      (lambda ((n <node>))
			(table-lookup tbl (id n)))))
      #f))

(define-method stat-node ((self <file-system>)
			  (path <fs-absolute-path>))
  (or (resolve-path self path current-dir-version) #f))
