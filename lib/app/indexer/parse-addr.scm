;;  wilson@cs.utexas.edu (Paul Wilson) => "wilson@cs.utexas.edu" "Paul Wilson"

(define mail-addr-comment-form 
  (reg-expr->proc '(entire 
		    (seq (let addr (+ (not space)))
			 (+ space)
			 #\(
			 (let name (+ any))
			 #\)))))

;;  Donovan Kolbly <donovan@tkg.com>  => "donovan@tkg.com" "Donovan Kolbly"

(define mail-addr-<>-form
  (reg-expr->proc '(entire 
		    (seq (let name (* any))
			 #\<
			 (let who (* (not #\>))) 
			 #\>))))

;;  "Curt Finch 1.2" <curt@tkg.com> => "curt@tkg.com" "Curt Finch 1.2"

(define mail-addr-quoted-form
  (reg-expr->proc '(entire
		    (seq #\" 
			 (let name (+ (not #\"))) 
			 #\"
			 (* space)
			 #\<
			 (let who (* (not #\>))) 
			 #\>))))

;;  foo@place.bar  ==>  "foo@place.bar"

(define mail-addr-fallback-form
  (reg-expr->proc '(let addr
		       (seq (+ (not (or #\space
					(range #\nul #\us)
					#\( #\) #\< #\> #\@
					#\, #\; #\: #\\ #\"
					#\. #\[ #\])))
			    #\@
			    (+ (not (or #\space
					(range #\nul #\us)
					#\( #\) #\< #\> #\@
					#\, #\; #\: #\\ #\"
					#\[ #\])))))))
;;
;; returns two values: mail-addr person
;; or #f

(define (parse-mail-addr addr-spec)
  (bind ((s e addr name (mail-addr-comment-form addr-spec)))
    (if s
	(values addr (chop-whitespace name))
	(bind ((s e name addr (mail-addr-quoted-form addr-spec)))
	  (if s
	      (values addr (chop-whitespace name))
	      (bind ((s e name addr (mail-addr-<>-form addr-spec)))
		(if s
		    (values addr (let ((c (chop-whitespace name)))
                                   (if (string=? c "")
                                       #f
                                       c)))
		    (bind ((s e addr (mail-addr-fallback-form addr-spec)))
		      (if s
			  (values addr #f)
			  #f)))))))))

(define (chop-whitespace str)
  (let loop ((i 0))
    (if (< i (string-length str))
	(if (char-whitespace? (string-ref str i))
	    (loop (+ i 1))
	    (let loop ((j (string-length str)))
	      (if (and (> j i)
		       (char-whitespace? (string-ref str (- j 1))))
		  (loop (- j 1))
		  (if (and (eq? i 0)
			   (eq? j (string-length str)))
		      str
		      (substring str i j)))))
	"")))

;;; returns a list of ("email-address" . "full-name") pairs

(define (parse-mail-addr-list addr-list)
  (let ((q (make-dequeue)))
    (for-each
     (lambda (elem)
       (bind ((addr name (parse-mail-addr (chop-whitespace elem))))
	 (if addr 
	     (dequeue-push-back! q (cons addr name)))
	 (values)))
     (string-split addr-list #\,))
    (vector->list (dequeue-state q))))
