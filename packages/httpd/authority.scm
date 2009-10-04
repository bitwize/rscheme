
;;
;;  a realm is an authorization domain
;;

(define-class <realm> (<object>)
  name
  user-table)

(define-class <realm-user> (<object>)
  passwords)

(define parse-basic-authorization
  (reg-expr->proc '(seq "Basic"
			(* space)
			(let encoded-string (* (not space))))))

(define (check-authority (realm <realm>) query)
  (let ((a (assq 'authorization query)))
    (if a
	(bind ((start end encoded-string (parse-basic-authorization (cdr a)))
	       (info (if start
			 (string-split (PEM-decode-string encoded-string) #\:)
			 '())))
	  (if (eq? (length info) 2)
	      (let ((user-name (car info))
		    (user (table-lookup (user-table realm) (car info)))
		    (pass (cadr info)))
		(xaction "authorization: user ~s pass ~s ==> ~s\n valid: ~s\n" user-name pass user (passwords user))
		(if (and user
			 (member pass (passwords user)))
		    user
		    (authorization-denied realm user-name)))
	      (no-authorization realm)))
	(no-authorization realm))))

(define (no-authorization realm)
  (let ((msg (format #f "HTTP/1.0 401 Authorization required\nWWW-Authenticate: Basic realm=~s\n"
		     (name realm))))
    (fd-write *client-fd* msg 0 (string-length msg))
    (summary (cons (cons 'disposition
			 'no-authorization)
		   *request-info*))
    (*handler-exit* 'no-authorization)))

(define (authorization-denied realm user)
  (let ((msg (with-output-to-string (lambda ()
				      (authentication-failure realm user)))))
    (fd-write *client-fd* msg 0 (string-length msg))
    (summary (cons (cons 'disposition
			 'authorization-denied)
		   *request-info*))
    (*handler-exit* 'authorization-denied)))
 
(define (authentication-failure realm user)
  (let ((msg (with-output-to-string
	       (lambda ()
		 (html
		  (title "401 Authentication Failure")
		  (header-1 "401 Authentication Failure")
		  (if user
		      (begin
			(format #t "The user '~a' is not authorized to access this resource\n" user)
			(format #t "in the ~a realm,\n" (name realm))
			(format #t "or the password you provided does not authenticate you\n")
			(format #t "as user '~a' in realm '~a'.\n" user (name realm)))
		      (begin
			(format #t "You must log in to the system by providing\n")
			(format #t "a username and password\n"))))))))
    (format #t "HTTP/1.0 401 Authorization failed\n" msg)
    (format #t "WWW-Authenticate: Basic realm=~s\n" (name realm))
    (format #t "MIME-Version: 1.0\n")
    (format #t "Server: X-Exp-RSscheme/1.0\n")
    (format #t "Date: ~a\n" (make-time-string))
    (format #t "Content-Type: text/html\n")
    (format #t "Content-Length: ~d\n" (string-length msg))
    (newline)
    (write-string (current-output-port) msg)))
  

