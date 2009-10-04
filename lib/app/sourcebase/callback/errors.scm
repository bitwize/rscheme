(define *short-error-list*
  '((400 "invalid version tag")
    (404 "no such file or directory")
    (405 "not a directory")
    (491 "access denied")
    (406 "user login already exists")
    (407 "user login already exists")
    (492 "filesystem read-only")))
     
;;
;;  generic error rendition support
;;

(define-class <error-rendition-port> (<output-port>)
   (underlying-port type: <output-port>)
   (line-number init-value: 0 type: <fixnum>)
   (beginning-of-line? init-value: #t)
   (line-prefix type: <string>))
   
(define (make-error-rendition-port port code)
  (make <error-rendition-port>
        underlying-port: port
	line-prefix: (number->string code)))

(define-method output-port-write-char ((self <error-rendition-port>) char)
   (if (eq? char #\newline)
       (begin
	    (output-port-write-char (underlying-port self) #\newline)
	    (set-line-number! self (add1 (line-number self)))
	    (set-beginning-of-line?! self #t))
	(begin
	    (if (beginning-of-line? self)
	        (begin
		  (write-string (underlying-port self) (line-prefix self))
		  (output-port-write-char (underlying-port self)
		  			  (if (eq? (line-number self) 0)
					      #\space
					      #\-))
		  (output-port-write-char (underlying-port self)
		  			  #\space)))
	    (set-beginning-of-line?! self #f)
	    (output-port-write-char (underlying-port self) char))))
	    

(define-syntax (error-rendition port code . body)
   (write-string port "***  ========================================================================\n")
   (with-output-to-port
     (make-error-rendition-port port code)
     (lambda ()
       (begin . body))))

(define (basic-error-rendition port code info more-thunk)
  (error-rendition port code
     (format #t "~a: ~a\n" info (cadr (assq code *short-error-list*)))
     (more-thunk)))
     

;;
;;------------------------------------------------------------------------

(define-class <recovery-protocol-failed> (<error>)
    expected-types
    returned-objects)

(define-method display-object ((self <recovery-protocol-failed>) port)
   (error-rendition port 490
     (format #t "recovery protocol failed\n")
     (for-each (lambda (r x)
		(format #t "returned: ~s\n" r)
		(format #t "expected a ~s\n" x))
	    (returned-objects self)
	    (expected-types self))))

;;

(define-syntax (require t v)
  (let ((temp v))
    (if (instance? temp t)
	temp
	(error (make <recovery-protocol-failed>
	 	     expected-types: (list t)
		     returned-objects: (list temp))))))

;;------------------------------------------------------------------------
;;
;;  restart protocol:  must return a <version-tag>

(define-class <invalid-version-tag> (<condition>)
    (in-path-string type: <string>)
    (tag-string type: <string>)
    (for-component type: <string>))

(define-method display-object ((self <invalid-version-tag>) port)
   (error-rendition port 400
       (format #t "Invalid version tag ~s\n" (tag-string self))
	(format #t "version tag ~s applied to component ~s\n"
	    (tag-string self)
	    (for-component self))
	(format #t "of ~s is invalid\n" (in-path-string self))))

;;------------------------------------------------------------------------
;;
;;  restart protocol:  must return a <fs-node-version> which is result
;;			of entire path lookup

(define-class <path-error> (<condition>)
    fs			;; <fs-version>
    path		;; <fs-absolute-path>
    rest)		;; <fs-relative-path>
    
(define-class <no-path> (<path-error>))
(define-class <path-exists> (<path-error>))

(define-method display-object ((self <no-path>) port)
   (error-rendition port 404
     (format #t "~a: no such file or directory\n" 
     		(path self))))
		
(define-method display-object ((self <path-exists>) port)
   (error-rendition port 407
     (format #t "~a: file or directory already exists\n" 
     		(path self))))

;;------------------------------------------------------------------------
;;
;;  restart protocol:  must return a <fs-node-version> which is result
;;			of entire path lookup

(define-class <not-directory> (<path-error>))

(define-method display-object ((self <not-directory>) port)
   (error-rendition port 405
     (format #t "~a: not a directory\n" (path self))))

;;------------------------------------------------------------------------

(define-class <user-access-denied> (<error>)
    user
    capability)

(define-method display-object ((self <user-access-denied>) port)
   (error-rendition port 491
      (format #t "access denied: user ~a lacks capability ~a\n"
		(name (user self))
      		(name (capability self)))))

;;------------------------------------------------------------------------

(define-class <fs-read-only> (<error>)
    fs-version
    reason)

(define-method display-object ((self <fs-read-only>) port)
   (basic-error-rendition port 492 
      (string-append (name (versioned-object (fs-version self)))
      		     "#"
		     (or (name (fs-version self))
		         "ANON"))
      (lambda ()
        (format #t "because ")
        (case (reason self)
	  ((committed) (format #t "it is a committed snapshot"))
	  ((build) (format #t "it is locked for builds"))
	  (else (format #t "of reason ~s" (reason self))))
	(format #t ".\n"))))

;;------------------------------------------------------------------------
;; recovery protocol: restart w/ a <user> that is
;; the new object

(define-class <user-exists> (<condition>)
    user)

(define-method display-object ((self <user-exists>) port)
   (error-rendition port 406
      (format #t "~a: user login already exists\n"
		(name (user self)))))
