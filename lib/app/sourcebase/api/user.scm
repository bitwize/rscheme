
(define *protected-user-properties* '(super-user))

(define-api (make-user login full-name email attribs)
   (if (table-lookup (user-table *application*) login)
       (error "~a: user already exists" login))
   (let ((u (make <user>
		  %alloc-area: (make-area)
   		  name: login
		  id: (+ 300 (table-size (user-table *application*)))
		  full-name: full-name
		  email-addr: email
		  properties: attribs)))
      (table-insert! (user-table *application*) login u)
      u))
