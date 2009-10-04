
(define (view-feature-responder n)
  (html
   (title (display "CMVC Feature Query"))
   (header-1 (display "CMVC Feature Query"))
   (let* ((cmd (format #f "Feature -view ~d -long" n))
	  (p (open-input-process cmd)))
     (par)
     (display "Command: ")
     (code (display cmd))
     (newline)
     (par)
     (if (not p)
	 (internal-error
	  "Could not launch Feature process: ~a\n"
	  (errorstr (errno))))
     ;;
     (preformatted
      (copy-lines p (current-output-port)))
     (horz-rule)
     (if (not (close-input-port-ok? p))
	 (cmvc-access-error
	  (lambda ()
	    (bold
	     (format #t "Feature ~d" n))
	    (display " does not exist, or you do not ")
	    (display "have permission to view it")))))))

(define (feature-responder remains query data)
  (if (null? remains)
      (huh? '())
      (if (equal? remains '("open"))
	  (fluid-let ((*login* (check-cmvc-authority query 'feature 'open)))
	    (open-feature-form query))
	  (if (equal? remains '("open" "submit"))
	      (fluid-let ((*login* (check-cmvc-authority query 
							 'feature 
							 'open)))
		(open-feature-post query data))
	      (if (and (string->number (car remains))
		       (null? (cdr remains)))
		  (view-feature-responder (string->number (car remains)))
		  (huh? remains))))))
