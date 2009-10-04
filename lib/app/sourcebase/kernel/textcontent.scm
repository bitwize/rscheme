
(define (string->shared-content (str <string>) 
				(relative-to <text-file-content>))
   (let ((tbl (make-table string=? string->hash)))
    (for-each-line
	relative-to
	(lambda (line)
	    (table-insert! tbl line line)))
      ;;
      (make <text-file-content>
            line-tree: (list->vector
	    		(map (lambda (line)
			       (or (table-lookup tbl line) line))
			     (string-split str #\newline))))))

(define (string->content (str <string>) #optional binary?)
  (if binary?
      (make <binary-file-content>
            data: str)
      (make <text-file-content>
            line-tree: (list->vector
	    		(string-split str #\newline)))))

(define (for-each-line (self <text-file-content>) proc)
    (let loop ((v (line-tree self)))
	(if (string? v)
	    (proc v)
	    (vector-for-each loop v))))

(define-method content->string ((self <text-file-content>))
   (call-with-output-string
     (lambda (p)
       (let ((any #f))
          (for-each-line
	    self
	    (lambda (v)
		(if any
		    (output-port-write-char p #\newline))
		(write-string p v)
		(set! any #t)))))))

(define-method content->string ((self <binary-file-content>))
  (let* (((d <byte-vector>) (data self))
	 (a (bvec-alloc <string> (add1 (bvec-length d)))))
    (bvec-copy a 0 d 0 (bvec-length d))
    a))
