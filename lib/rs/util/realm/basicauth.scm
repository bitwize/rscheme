(define-class <auth-realm> (<object>) :abstract)

(define-generic-function authenticate)

(define-class <basic-auth-realm> (<auth-realm>)
  name
  user-table)

(define-method lookup-realm-user ((self <basic-auth-realm>)
                                  (name <string>))
  (let ((e (table-lookup (user-table self) name)))
    (if (pair? e)
        (cdr e)
        #f)))

(define-method add-realm-user! ((self <basic-auth-realm>) 
                                user-name 
                                user-passwd
                                datum)
  (table-insert! (user-table self)
                 user-name
                 (cons user-passwd datum)))

(define-method authenticate ((self <basic-auth-realm>) name passwd)
  (let ((e (table-lookup (user-table self) name)))
    (if (and (pair? e) (string=? (car e) passwd))
        (cdr e)
        #f)))

(define-method chap-authenticate ((self <basic-auth-realm>) 
                                  name 
                                  response
                                  hasher)
  (let ((e (table-lookup (user-table self) name)))
    (if (and (pair? e)
             (string=? (hasher (car e)) response))
        (cdr e)
        #f)))

(define (make-basic-auth-realm name)
  (make <basic-auth-realm>
        name: name
        user-table: (make-string-table)))


(define-method need-password? ((self <auth-realm>) user)
  #t)

