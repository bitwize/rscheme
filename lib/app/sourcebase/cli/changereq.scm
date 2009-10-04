
(define (handle-cr-request args req inp out (u <user>))
  (cond
   ((null? args)
    (cr-open args req inp out u))
   ((assq 'comment req)
    (cr-comment args req inp out u))
   ((assq 'modify req)
    (cr-modify args req inp out u))
   ((assq 'assign req)
    (cr-assign args req inp out u))
   ((assq 'research req)
    (cr-complete args req inp out u 'open 'research))
   ((assq 'fixing req)
    (cr-complete args req inp out u 'research 'fixing))
   ((assq 'done req)
    (cr-done args req inp out u))
   ((assq 'ok req)
    (cr-complete args req inp out u 'check-off 'closed))
   ((assq 'duplicate req)
    (cr-duplicate args req inp out u))
   (else
    (let ((ren (rendition-proc <change-request> req render-full)))
      (for-each (lambda (a)
		  (client-print-message
		   out
		   (with-output-to-string
		     (lambda ()
		       (ren (string->changereq a))))))
		args)))))

(define (cr-done args req inp out u)
  (if (assq 'with req)
      (let ((done-with (map string->filesystem (cdr (assq 'with req)))))
	(for-each
	 (lambda (cr)
	   (for-each
	    (lambda (with)
	      (fs-change-complete cr u with))
	    done-with))
	 (map string->changereq args)))
      (cr-complete args req inp out u 'fixing 'check-off)))

(define (cr-complete args req inp out u olds news)
  (let ((crs (map string->changereq args)))
    (for-each (lambda (cr)
		(if (not (eq? (state cr) olds))
		    (service-error 17
				   "Change request ~a is not in the `~a' state"
				   (id cr)
				   olds)))
	      crs)
    (for-each (lambda (cr)
		(change-request-work-complete cr u)
		(service-message
		 out
		 18
		 "Change request ~a is now in the `~a' state\n"
		 (id cr)
		 (state cr)))
	      crs)))

(define (cr-assign args req inp out (u <user>))
  (let ((crs (map string->changereq args))
        (to (string->user (get-exactly-one req 'owner))))
    (for-each (lambda (cr)
                (change-request-assign cr to))
              crs)))


(define (cr-comment args req inp out (u <user>))
  (let ((text (get-remarks-arg req inp out)))
    (for-each 
     (lambda ((cr <change-request>))
       (change-request-add-comment cr text))
     (map string->changereq args))))
		    
(define (cr-modify args req inp out (u <user>))
  (map (compute-cr-modify-proc req inp out)
       (map string->changereq args)))

(define (compute-cr-modify-proc req inp out)
  (let ((ops '()))
    (define (pushop proc)
      (set! ops (cons proc ops)))
    ;;
    ;; `--title NEWTITLE'
    ;;
    (if (assq 'title req)
	(let ((new-title (get-exactly-one req 'title)))
	  (pushop
	   (lambda ((cr <change-request>))
	     (change-request-change-title cr new-title)))))
    ;;
    ;; `--summary NEWSUMMARY'
    ;;
    (if (assq 'summary req)
	(let ((new-summ (immediate-or-snarf-stdin
			 (get-exactly-one req 'summary)
			 inp
			 out)))
	  (pushop
	   (lambda ((cr <change-request>))
	     (change-request-change-summary cr new-summ)))))
    ;;
    ;; `--property PROP1 VALUE1 PROP2 VALUE2 ...'
    ;;
    (if (assq 'property req)
	(let ((kvv (list->vector (cdr (assq 'property req)))))
	  (for-each
	   (lambda (i)
	     (let* ((k (vector-ref kvv (* i 2)))
		    (v (vector-ref kvv (+ 1 (* i 2))))
		    (p (table-lookup (property-table *application*) k)))
	       (if (not p)
		   (service-error 593 "`--property ~a ~a' property not recognized"
				  k v))
	       (let ((ppv (parse-property-value p v)))
		 (pushop
		  (lambda ((cr <change-request>))
		    (change-request-set-property cr p ppv))))))
	   (range (quotient (vector-length kvv) 2)))))
    ;
    (if (null? ops)
	(service-error 597 "No `--changereq --modify' subcommands"))
    (lambda (cr)
      (for-each (lambda (op)
		  (op cr))
		ops)
      (service-message out 402 "Change request ~d updated\n" (id cr)))))

(define (cr-open args req inp out (u <user>))
  (let ((title (get-exactly-one req 'title))
	(group (get-group-arg req))
	(summary (let ((a (assq 'summary req)))
		   (if a
		       (string-join #\newline (cdr a))
		       #f)))
	(rem (get-remarks-arg req inp out))
	(props (cli-properties 
		req 
		(get-property *application* 'cr-optional '())
		(get-property *application* 'cr-required '()))))
    (let ((cr (make-change-request title group summary rem props)))
      (service-message
       out
       401
       "Change request ~d has been opened\n"
       (id cr)))))

(define (cr-duplicate args req inp out (u <user>))
  (let ((dup-of (string->changereq (get-exactly-one req 'of))))
    (for-each 
     (lambda (cr)
       (request-is-duplicate cr dup-of))
     (map string->changereq args))))
