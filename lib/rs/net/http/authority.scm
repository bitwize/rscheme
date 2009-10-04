
(define parse-basic-authorization
  (reg-expr->proc '(seq "Basic"
			(* space)
			(let encoded-string (* (not space))))))

(define (report-no-authorization (realm <string>))
  (format #t "HTTP/1.0 401 Authorization required\r\n")
  (format #t "WWW-Authenticate: Basic realm=~s\r\n" realm))

(define (gen-authorization-denied-page (realm <string>) (user <string>))
  (with-output-to-html-string
   (lambda ()
     (html
      (title "401 Authentication Failure")
      (header-1 "401 Authentication Failure")
      (if user
	  (begin
	    (format #t "The user '~a' is not authorized to access\n" user)
	    (format #t "this resource in the ~a realm,\n" (name realm))
	    (format #t "or the password you provided does not authenticate\n")
	    (format #t "you as user '~a' in the ~a realm.\n" 
		    user 
		    (name realm)))
	  (begin
	    (format #t "You must log in to the system by providing\n")
	    (format #t "a username and password\n")))))))

(define (report-authorization-denied (realm <string>) (user <string>))
  (let ((msg (gen-authorization-denied-page realm user)))
    (format #t "HTTP/1.0 401 Authorization failed\n" msg)
    (format #t "WWW-Authenticate: Basic realm=~s\n" (name realm))
    (format #t "MIME-Version: 1.0\n")
    (format #t "Server: X-Exp-RSscheme/1.0\n")
    (format #t "Date: ~a\n" (make-time-string))
    (format #t "Content-Type: text/html\n")
    (format #t "Content-Length: ~d\n" (string-length msg))
    (newline)
    (write-string (current-output-port) msg)))
  

