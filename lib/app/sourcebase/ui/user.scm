
(define-method render-full ((self <user>))
  (format #t "User:       ~a\n" (name self))
  (format #t "Address:    ~a <~a>\n" (full-name self) (email-addr self))
  (if (pair? (properties self))
      (begin
	(format #t "\nProperties\n")
	(format #t "----------\n")
	(for-each (lambda (p)
		    (format #t "  ~a => ~s\n" (car p) (cdr p)))
		  (properties self))))
  ;;
  (if (pair? (check-outs self))
      (begin
	(format #t "\nChecked out files\n")
	(format #t "-----------------\n")
	(for-each (lambda (co)
		    (format #t "  ~a ~8a ~5a ~a\n"
			    (time->string (checkout-time co)
					  "%Y-%m-%d")
			    (name (file-system co))
			    (version-tag->string
			     (version-tag (checked-out co)))
			    (let ((p (node->paths (file-system co)
						  (versioned-object 
						   (checked-out co)))))
			      (if (null? p)
				  "*unreachable*"
				  (fs-path->string (car p))))))
		  (check-outs self))))
  ;;
  (format #t "\nLogin hosts\n")
  (format #t "-----------\n")
  (for-each (lambda (h)
	      (format #t "  from ~a: ~j\n" (car h) (cdr h)))
	    (remote-hosts self))
  (format #t "\nActive items\n")
  (format #t "------------\n")
  (for-each render-work-line-item 
	    (sorted-work-items
	     (active-items self))))

(define (sorted-work-items lst)
  (sort lst
	(lambda ((a <work-item> :trust-me)
		 (b <work-item> :trust-me))
	  (fixnum<? (id (base-request a)) (id (base-request b))))))
