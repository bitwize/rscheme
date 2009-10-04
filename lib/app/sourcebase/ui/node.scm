
(define-method render-full ((self <node-version>) fs)
  (render-full (versioned-object self) fs))

(define-method render-full ((self <node>) fs)
  (let ((cv (current-version self)))
    (format #t "File:    node ~d\n" (id self))
    (format #t "Release: ~a\n" (name fs))
    ;;
    (let ((p (node->paths fs self)))
      (if (null? p)
	  (format #t "Paths:  *none*\n")
	  (begin
	    (format #t "Paths:  ~a\n" (fs-path->string (car p)))
	    (for-each (lambda (p)
			(format #t "        ~a\n" (fs-path->string p)))
		      (cdr p)))))
    ;;
    (format #t "Version: ~a\n" (version-tag->string (version-tag cv)))
    (format #t "Group: ~a\n" (name (group self)))
    (format #t "Permissions: ~03o\n" (permissions cv))
    ;;
    (let ((c (active-checkout self)))
      (if c
	  (begin
	    (format #t "Locked:  [~a] by ~a\n" 
		    (time->string (checkout-time c) "%Y-%m-%d %H:%M:%S")
		    (name (user c)))
	    (if (instance? c <dir-checkout>)
		(begin
		  (format #t "\nChanges after ~a\n"
			  (version-tag->string (version-tag cv)))
		  (format #t "-------------------\n")
		  (for-each render-dir-diff-line
			    (dir-diffs (contents (current-version self))
				       (contents (new-version c)))))))))
    ;;
    (if (or (pair? (version-properties cv))
	    (pair? (stable-properties self)))
	(begin
	  (format #t "Properties\n")
	  (format #t "----------\n")
	  (for-each (lambda (p)
		      (format #t "  vers ~s => ~s\n" (car p) (cdr p)))
		    (version-properties cv))
	  (for-each (lambda (p)
		      (format #t "  file ~s => ~s\n" (car p) (cdr p)))
		    (stable-properties self))))
    ;;
    (format #t "\nHistory\n")
    (format #t "-------\n")
    (for-each-version
     (versions self)
     (lambda (label tag)
       (let (((v <node-version>) (value tag)))
	 (let loop ((l (string-split (or (comment v) 
					 "Initial version") 
				     #\newline))
		    (first? #t))
	   (if (pair? l)
	       (begin
		 (if first?
		     (format #t "~4a [~a] " 
			     label
			     (time->string (modification-time v)
					   "%Y-%m-%d"))
		     (format #t "     "))
		 (format #t "~a\n" (car l))
		 (loop (cdr l) #f))
	       (if (pair? (fs-change-items v))
		   (format #t "[by ~a, change requests: ~j]\n"
			   (name (owner (car (fs-change-items v))))
			   (map id (map base-request 
					(fs-change-items v))))
                   (if (audit-change-item v)
                       (format #t "[by ~a]\n" 
                               (name (user (audit-change-item v)))))))))))))

(define (node-version-author (self <node-version>))
  (let ((fsc (fs-change-items self)))
    (if (pair? fsc)
        (owner (car fsc))
        (let ((a (audit-change-item self)))
          (if a
              (user (audit-change-item self))
              #f)))))

(define (render-dir-diff-line (d <pair>))
  (case (car d)
    ((link)
     (format #t "   link     ~a\n" (cadr d)))
    ((unlink)
     (format #t "   unlink   ~a\n" (cadr d)))
    ((link)
     (format #t "   rename   ~a => ~a\n" (cadr d) (caddr d)))
    (else
     (format #t "   ? ~a\n" d))))

(define (dir-diffs* old-contents new-contents)
  (let ((fmap (map (lambda ((p <pair>))
		     (cons (cdr p) (car p)))
		   old-contents)))
    (let loop ((n new-contents)
	       (r '()))
      (if (null? n)
	  r
	  (let ((a (assq (cdr (car n)) fmap)))
	    (if a
		;; still here
		(if (string=? (cdr a) (car (car n)))
		    ;; still same name, too
		    (loop (cdr n) 
			  r)
		    ;; changed names
		    (loop (cdr n) 
			  (cons (list 'rename (cdr a) (caar n)) r)))
		;; created
		(loop (cdr n)
		      (cons (list 'link (car (car n))) r))))))))

(define (dir-diffs old-c new-c)
  (sort (append 
	 (map (lambda ((p <pair>))
		(cons 'unlink (cdr p)))
	      (select (lambda ((p <pair>))
			(eq? (car p) 'link))
		      (dir-diffs* new-c old-c)))
	 (dir-diffs* old-c new-c))
	(lambda ((a <pair>) (b <pair>))
	  (string<? (cadr a) (cadr b)))))

