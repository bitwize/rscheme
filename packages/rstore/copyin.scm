;;
;; this is the default parameter passed to make-make-std-copy-iner,
;; and is the procedure which does the actual copy.
;; <symbol>'s are not passed to this procedure
;;

(define (default-copy-in-obj area item)
  (dm 610 "copying ~#*@40s into ~s" item area)
  (if (class? item)
      (em 611 "~a: pointer to class ~s escaped" 
	  (pstore-file (allocation-area->store area))
	  item)
      (copy-into-pstore area item)))

;;
;; copies in only certain "canonical" classes, which
;; are the ones specified plus standard scheme datum classes
;;

(define (make-strict-copy-in-obj . allow)
  (set! allow (append (list <pair>
			    <vector>
			    <double-float>
			    <string>)
		     allow))
  (lambda (area item)
    (if (memq (object-class item) allow)
	(copy-into-pstore area item)
	(dm "~a: pointer to ~s escaped" 
	    (pstore-file (allocation-area->store area))
	    item))))

;;
;; the "make-copy-iner" procedure returns two
;; procedures.
;;
;; the first is responsible for copying a single object
;; into the pstore, and takes two arguments:
;;   (1) the object to be copied
;;   (2) an accumulation list of more objects that will be copied
;; it returns a possibly-extended list of objects still to be
;; copied

(define (make-make-std-copy-iner copy-one-obj)
  (lambda ((ps <persistent-store>) reloc-table)
    (let ((new-symbols '())
	  (symbols-indir-page #f)
	  (symbol-pers-id #f)
	  (area (default-allocation-area ps)))
      (values
       (lambda (i more)
	 (if (symbol? i)
	     ;;
	     ;; we know how to handle symbols
	     ;;
	     (if (memq i new-symbols)
		 (begin
		   (dm 210 "already made known: ~s" i)
		   more)
		 (begin
		   (if (not symbol-pers-id)
		       (begin
			 (set! symbols-indir-page (alloc-indirect-page ps))
			 (set! symbol-pers-id (* 64 symbols-indir-page))))
		   (dm 211 "making known: ~s [~d]"
		       i
		       symbol-pers-id)
		   (set! new-symbols (cons i new-symbols))
		   (table-insert! (pivots ps) i symbol-pers-id)
		   (if (eq? (length new-symbols) 64)
		       (begin
			 (flush-symbols-to-indirect
			  ps
			  symbols-indir-page
			  (reverse new-symbols))
			 (set! new-symbols '())
			 (set! symbol-pers-id #f))
		       (set! symbol-pers-id (+ symbol-pers-id 1)))
		   more))
	     ;;
	     ;; not a symbol
	     ;;
	     (begin
	       (if (table-lookup reloc-table i)
		   (dm 213 "already copied: ~s" i)
		   (begin
		     (dm 212 "copying in: ~s" i)
		     (let ((new (copy-one-obj area i)))
		       (table-insert! reloc-table i new)
		       ;;
		       ;; return a new list of objects, which is the old
		       ;; list extended (prepended) with the objects that
		       ;; need to be copied in order to satisfy references
		       ;; in the just-copied object
		       ;;
		       (let ((nw (scan-object ps new '() reloc-table)))
			 (let loop ((n nw)
				    (r more))
			   (if (null? n)
			       r
			       (loop 
				(cdr n)
				(if (table-lookup reloc-table (car n))
				    r
				    (cons (car n) r))))))))))))
       ;;
       ;; this is the procedure that will be called
       ;; when the current queue of objects to be copied
       ;; is empty.  We use it to actually flush any remaining new symbols
       ;; into indirect pages
       ;;
       (lambda ()
	 (if symbol-pers-id
	     (flush-symbols-to-indirect ps
					symbols-indir-page
					(reverse new-symbols))))))))

(define (flush-symbols-to-indirect ps indir-page symbol-list)
  (dm 219 "flushing ~d symbols to page ~d: ~s"
      (length symbol-list)
      indir-page
      symbol-list)
  (let ((data (symbol-list->indirect-page symbol-list)))
    ;(print data)
    (write-indirect-page-data ps
			      indir-page
			      $symbol-indirect-page-constructor
			      0 ;; instance_id
			      data)))

(define $symbol-indirect-page-constructor 0)

(define (make-copy-iner ps)
  (make-make-std-copy-iner default-copy-in-obj))

(define (copy-in* (ps <persistent-store>) reloc-table list)
  (bind ((copy-iner done ((make-copy-iner ps) ps reloc-table)))
    (let loop ((list list))
      (if (null? list)
	  (done)
	  (let ((more '()))
	    (for-each 
	     (lambda (i)
	       (set! more (copy-iner i more)))
	     list)
	    (loop more))))))
