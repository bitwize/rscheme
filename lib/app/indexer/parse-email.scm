(define-module-extend calendar ()

  (add-time-pattern!
   rfc-822-time
   (reg-expr->proc 
    '(entire
      (seq (? (seq (or "Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")
                   #\,
                   (+ #\space)))
           (let day (+ digit))
           (+ #\space)
           (let month (seq (range #\A #\Z)
                           (range #\a #\z)
                           (range #\a #\z)))
           (+ #\space)
           (let year (+ digit))
           (+ #\space)
           (let hh (+ digit))
           #\:
           (let mm (+ digit))
           (? (seq #\:
                   (let ss (+ digit))))
           (+ #\space)
           (let tz (or (+ (range #\A #\Z))
                       ;; technically, the +/- is required
                       ;; but lets be slightly more forgiving...
                       (seq (? (or #\+ #\-))
                            digit digit digit digit)))
           (? (seq 
               (* #\space)
               #\(
               (* (not #\)))
               #\)))))))
)

(define (parse-envelope (env <string>))
  (let ((i (string-search env #\space)))
    (if i
	(values (substring env 0 i)
		(substring env (+ i 1)))
	(values env #f))))

(define match-header-first
  (reg-expr->proc '(prefix
		    (seq (let key (* (not (or #\: #\space))))
			 (? #\:)
			 (* space)
			 (let value (* any))))))

(define (split-off-mail-header lines)
  (let loop ((h '())
	     (s lines))
    (if (or (null? s)
	    (string=? (car s) ""))
	(values (reverse h)
		(if (null? s) 
		    '() 
		    (cdr s)))
	(let ((l (car s)))
	  (if (char-whitespace? (string-ref l 0))
	      ;; it's a continuation line
	      (if (null? h)
		  (error "continuation line before first field: ~s\n" l)
		  (loop (cons (append! (car h) (list l)) (cdr h))
			(cdr s)))
	      ;; it's a new header field
	      (bind ((start end field value (match-header-first l)))
		(if start
		      (loop (cons (list field value) h)
			    (cdr s))
		      (error "badly formatted header field: ~s\n" l))))))))

(define (header-list->alist headers)
  (let ((al '()))
    (for-each
     (lambda (h)
       (let ((key (table-lookup *known-header-fields* (car h))))
	 (if key
	     (set! al (cons (cons key
				  (string-join #\space (cdr h)))
			    al)))))
     headers)
    (reverse al)))

;;;

(define *known-header-fields* (make-table string-ci=? string-ci->hash))

(for-each (lambda (p)
	    (table-insert! *known-header-fields* (car p) (cadr p)))
	  '(("From" from)
	    ("To" to)
	    ("Cc" to)
	    ("Apparently-To" to)
	    ("Message-Id" message-id)
	    ("In-Reply-To" in-reply-to)
	    ("References" references)
	    ("Subject" subject)
	    ("Date" date)))

;;;

(define (get-subject-and-time-from-email (msg <string>))
  (bind ((lines (string-split msg #\newline))
	 (headers (split-off-mail-header (cdr lines)))
	 (hdrs (header-list->alist headers))
	 (subj-e (assq 'subject hdrs))
	 (date-e (assq 'date hdrs))
	 (subj (if subj-e
		   (cdr subj-e)
		   #f))
	 (time (if date-e
		   (string->time (cdr date-e))
		   #f)))
    (values subj time)))

(define (get-keywords-from-email (msg <string>))
  (bind ((lines (string-split msg #\newline))
	 (env (car lines))
	 (headers body (split-off-mail-header (cdr lines)))
	 (hdrs (header-list->alist headers))
	 (q (make-dequeue)))
    ;;
    (define (kl section keywords)
      (if (not (null? keywords))
	  (dequeue-push-back! q (cons section keywords)))
      (values))
    ;; accumulate keywords from headers
    (for-each
     (lambda (h)
       (case (car h)
	 ((date)
	  (let ((t (string->time (cdr h))))
	    (if t 
		(kl 'date (list (time->string t "%Y-%m-%d"))))))
	  ((message-id references in-reply-to)
	   (kl (car h) (list (cdr h))))
	  ((from to)
           (let ((data (parse-mail-addr-list (cdr h))))
             ;; insert the full email addresses
             (kl (car h) (map car data))
             ;; insert the constituent words
             (let ((also '()))
               (for-each
                (lambda (e)
                  (set! also (append (parse-text (car e)) also))
                  (if (cdr e)
                      (set! also (append (parse-text (cdr e)) also))))
                data)
               (dm 103 " also adding: ~s" also)
               (kl (car h) also))))
	  (else
	   (kl (car h) (parse-text (cdr h))))))
      hdrs)
    ;;
    (for-each
     (lambda (line)
       (kl 'body (parse-text line)))
     body)
    ;;
    (for-each (lambda (line) (kl 'domain (parse-domain-names line))) body)
    (for-each (lambda (h) 
		(if (not (memq h '(message-id references)))
		    (kl 'domain (parse-domain-names (cdr h)))))
	      hdrs)
    ;;
    (vector->list (dequeue-state q))))

;;;

(define (for-each-mail-message port proc)
  (letrec ((start (lambda (l)
                    (let ((a (open-output-string)))
                      (write-string a l)
                      (newline a)
                      (body a))))
           (body (lambda (accum)
                   (let ((l (read-line port)))
                     (if (eof-object? l)
                         (proc (get-output-string accum))
                         (if (and (> (string-length l) 5)
                                  (string=? (substring l 0 5) "From "))
                             (begin
                               (proc (get-output-string accum))
                               (start l))
                             (begin
                               (write-string accum l)
                               (newline accum)
                               (body accum))))))))
    ;;
    (let ((l (read-line port)))
      (if (not (eof-object? l))
          (start l)))))
    
;;

(define (t)
  (get-keywords-from-email (file->string "msg1")))

(define (ta)
  (let ((ix (make-document-index)))
    (call-with-input-file
        "/tmp/x1"
      (lambda (port)
        (for-each-mail-message
         port
         (lambda (text)
           (bind ((subj t (get-subject-and-time-from-email text)))
             (let ((msg (make <inline-email-message>
                              content: (list->vector
                                        (string-split text #\newline))
                              subject-line: subj
                              time-sent: t)))
               (add-document ix msg)
               (for-each
                (lambda (kwdlist)
                  (add-to-section ix msg (car kwdlist) (cdr kwdlist)))
                (get-keywords-from-email text))))))))
    ix))



    
