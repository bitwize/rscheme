;;
;;  code for flushing the mail queue
;;

(define (group-interest (group <group>) (item <work-item>))
  '())

(define (flush-mail-queue)
  (let ((u (make-object-table)))
    ;; 
    ;; collect all the outbound mail by user
    ;;
    (for-each (lambda ((i <work-item>))
		(format #t "flushing for ~s: ~s\n" (owner i) (base-request i))
		(let ((notify (append 
			       (list (owner i))
			       (interest (base-request i))
			       (group-interest (group (base-request i)) i))))
		  (for-each (lambda (p)
			      (table-insert! u 
					     p
					     (cons i (or (table-lookup u p) 
							 '()))))
			    (unionq notify '()))))
	      (mail-queue *application*))
    ;;
    ;; send the mail
    ;;
    (for-each
     (lambda (k v)
       (format #t "~d messages for ~s\n" (length v) k)
       (let ((p (open-output-process (string-append
				      "mail -s 'SourceBase Mailbox' "
				      (email-addr k)))))
	 (with-output-to-port
	     p
	   (lambda ()
	     (format #t
		     "You have ~d new items requiring your attention.\n"
		     (length v))
	     (display (make-string 70 #\-))
	     (newline)
	     (for-each render-work-line-item v)))
	 (close-output-port p)))
     (key-sequence u)
     (value-sequence u))
    ;;
    (set-mail-queue! *application* '())))
