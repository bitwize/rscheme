;;
;; protect us from accessing another family...
;;

(define *cmvc-family* '#uninit)
(define *login* '#uninit)
(define *cmvc-release* '#uninit)

(define *valid-families* '("rsfam"))

(define (cmvc-init)
  (set-env "CMVC_FAMILY" "rsfam")
  (set-env "CMVC_BECOME" "rswebd")
  (set-env "PATH" (string-append "/usr/lpp/cmvc/bin" ":" (getenv "PATH")))
  (export-environ))

(define (main . args)
  (cmvc-init)
  (format #t "running on port 8080...\n")
  (run 8080))

(define-class <cmvc-user> (<realm-user>)
  name
  full-name
  authority)

(define (authority->authorized-actions auth)
  (case auth
    ((developer)
     '((file view get lock create list)
       (defect view open)))
    (else
     '())))

(define (authorized-become (user <cmvc-user>))
  (format *console-output-port* "BECOME ~a (~a)\n"
	  (full-name user)
	  (name user))
  (set-env "CMVC_BECOME" (name user))
  (export-environ)
  user)

(define (no-cmvc-authority object action)
  (cmvc-access-error
   (lambda ()
     (display "You are not authorized to perform the ")
     (bold (display action))
     (display " action on ")
     (bold (display object))
     (display " objects."))))

(define (check-cmvc-authority query object action)
  (let ((user (check-authority *cmvc-realm* query)))
    (format *console-output-port* "checking CMVC authority: ~s for ~s on ~s\n"
	    user action object)
    (if user
	(if (memq 'super (authority user))
	    (authorized-become user)
	    (let loop ((a (authority user)))
	      (if (null? a)
		  (no-cmvc-authority object action)
		  (let ((h (assq object
				 (authority->authorized-actions (car a)))))
		    (if (and h (memq action (cdr h)))
			(begin
			  (format *console-output-port*
				  "  access granted on basis of: ~s\n" (car a))
			  (authorized-become user))
			(loop (cdr a)))))))
	(no-cmvc-authority object action))))

(define *cmvc-realm*
  (make <realm>
	name: "TKG CMVC Gateway"
	user-table: (let ((t (make-table string=? string->hash)))
		      (table-insert! t
				     "donovan"
				     (make <cmvc-user>
					   name: "donovan"
					   full-name: "Donovan Kolbly"
					   authority: '(super)
					   passwords: '("regular-guy")))
		      (table-insert! t
				     "wilson"
				     (make <cmvc-user>
					   name: "wilson"
					   full-name: "Paul Wilson"
					   authority: '(developer)
					   passwords: '("gc-black-belt")))
		      t)))

(define (cmvc-responder remains query data)
  (set-env "CMVC_BECOME" "rswebd")
  (export-environ)
  (if (or (not (pair? remains))
	  (not (member (car remains) *valid-families*)))
      (cmvc-access-error
       (lambda ()
	 (format #t "Illegal cmvc access path: ~j\n" remains)))
      (fluid-let ((*cmvc-family* (car remains)))
	(console "(family: ~s)\n~s\n" (car remains) (cdr remains))
	(family-responder (cdr remains) query data))))

(define (family-responder remains query data)
  (if (and (pair? remains)
	   (member (car remains) '("user" "defect" "feature" "file")))
      (begin
	(console "(family: ~s object: ~a)\n" *cmvc-family* (car remains))
	(case (string->symbol (car remains))
	  ((user) (user-responder (cdr remains) query data))
	  ((defect) (defect-responder (cdr remains) query data))
	  ((feature) (feature-responder (cdr remains) query data))
	  ((file) (file-responder (cdr remains) query data))
	  (else 'internal-error)))
      (cmvc-access-error
       (lambda ()
	 (if (pair? remains)
	     (format #t "Unrecognized object type: ~s\n" (car remains))
	     (format #t "Missing object type\n"))))))

(define (user-responder remains query data)
  (if (equal? remains '("new"))
      (new-user-form query)
      (if (equal? remains '("new" "submit"))
	  (new-user-post query data)
	  (huh? remains))))

(define (huh? remains)
  (cmvc-access-error
   (lambda ()
     (format #t "Unsupported cmvc access path: ~a\n"
	     (string-join #\/ remains)))))

(define (copy-lines src-port dst-port)
  (let loop ((n 0))
    (let ((l (read-line src-port)))
      (if (eof-object? l)
	  n
	  (begin
	    (write-string dst-port l)
	    (newline dst-port)
	    (loop (+ n 1)))))))

(define (cmvc-access-error display-error-thunk)
  (http-error-handler 400 "Access error" display-error-thunk))

