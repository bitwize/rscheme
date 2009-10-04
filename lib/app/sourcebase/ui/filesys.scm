(define-method render-full ((self <snapshot>))
  (define (with-property prop proc)
    (let ((x (assq prop (properties self))))
      (if x
	  (proc (cdr x)))))
  ;;
  (format #t "File system:   ~a\n" (name (versioned-object self)))
  (format #t "Snapshot:      ~a\n" (name self))
  (format #t "State:         ~a\n" (snapshot-state self))
  (with-property
   'created
   (lambda (a)
     (format #t "Created:       ~a\n" (timestamp a))))
  (with-property
   'committed
   (lambda (a)
     (format #t "Committed:     ~a\n" (timestamp a))))
  (with-property
   'basis
   (lambda (s)
     (format #t "Based on:      ~a\n" (name s))))
  (newline)
  ;;
  (let ((hdr? #f))
    (for-each (lambda (p)
		(if (not (memq (car p) '(committed 
					 created
					 basis
					 extend
					 state)))
		    (begin
		      (if (not hdr?)
			  (begin
			    (format #t "Properties\n")
			    (format #t "----------\n")
			    (set! hdr? #t)))
		      (format #t " ~s => ~s\n" (car p) (cdr p)))))
	      (properties self)))
  ;;
  (with-property
   'extend
   (lambda (x)
     (format #t "Integrated change requests\n")
     (format #t "--------------------------\n")
     (for-each (lambda (lst)
		 (for-each cr-in-snap lst))
	       x))))

(define (cr-in-snap (self <change-request>))
  (format #t "~-5d ~a ~a\n"
	  (id self)
	  (time->string
	   (timestamp (activate-audit-entry
		       (last (if (null? (history self))
				 (active-items self)
				 (history self)))))
	   "%y.%m.%d")
	  (title self)))

(define-method render-full ((self <file-system>))
  (format #t "File system:   ~a\n" (name self))
  ;;
  (format #t "\nProperties\n")
  (format #t "----------\n")
  (for-each (lambda (p)
	      (format #t " ~s => ~s\n" (car p) (cdr p)))
	    (properties self))
  ;;
  (format #t "\nSnapshots\n")
  (format #t "---------\n")
  (for-each (lambda (s)
	      (format #t " ~a  (~a)\n"
		      (name s)
		      (snapshot-state s)))
	    (value-sequence
	     (snapshot-table self))))


