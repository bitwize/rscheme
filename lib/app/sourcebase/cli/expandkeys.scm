(define key-pattern (reg-expr->proc '(seq #\% alpha #\%)))

(define (expand-keys (text <string>) keylist)
  ;;
  (define (expansion k)
    (let ((v (assq k keylist)))
      (if v
          (if (procedure? (cdr v))
              ((cdr v))
              (cdr v))
          #f)))
  ;;
  (call-with-output-string
   (lambda (port)
     (let loop ((i 0))
       (bind ((s e (key-pattern text i)))
	 (if s
	     (begin
	       (write-string port (substring text i s))
	       (let ((x (expansion (string-ref text (+ s 1)))))
		 (if x
		     (write-string port x)
		     (write-string port (substring text s e)))
		 (loop e)))
	     (write-string port (substring text i))))))))

(define (time->date t)
  (string->date (time->string t "%Y-%m-%d")))

(define (nth-change-on-date (v <file-version>))
  (let ((d (time->date (modification-time v))))
    (let loop ((k 1)
	       (v (previous-version v)))
      (if (and v (date=? d (time->date (modification-time v))))
	  (loop (+ k 1) (previous-version v))
	  (values d k)))))

(define (dns-style-serial-number (v <file-version>))
  (bind ((d k (nth-change-on-date v))
         (y m d (date->ymd d)))
    (format #f "~04d~02d~02d~02d" y m d k)))

(define (expanded-file-content (fs <file-space>) 
			       (p <fs-absolute-path>)
			       (v <file-version>)
			       (global-keys <list>))
  (if (instance? (contents v) <text-file-content>)
      (let ((kx (cons* 
                 (cons #\I (version-tag->string
                            (version-tag v)))
                 (cons #\p (substring (fs-path->string p) 1))
                 (cons #\E (time->string (modification-time v) "%Y-%m-%d"))
                 (cons #\U (time->string (modification-time v) "%H:%M:%S"))
                 (cons #\f (name fs))
                 (cons #\g (name (group (versioned-object v))))
                 (cons #\S (lambda () (dns-style-serial-number v)))
                 global-keys)))
        (expand-keys (content->string (contents v)) 
                     (append
                      kx
                      (let ((author (node-version-author v)))
                        (if author
                            (list (cons #\a (name author))
                                  (cons #\A (full-name author))
                                  (cons #\M (email-addr author)))
                            '())))))
      (content->string (contents v))))
