,(use tables syscalls rs.util.properties)

;;

;;(define-class <application-object> (<object>) :abstract
;;  (oid type: <fixnum>))

(define-class <application-root> (<object>) :abstract
  (properties init-value: '#())
  (next-id init-value: 1)
  (user-index init-function: make-string-table)
  (domain-index init-function: make-string-table)
  (priviledge-index init-function: make-string-table)
  (global-history init-value: '())
  (root-domain))

(define (alloc-item-id)
  (let* ((ro (current-application-root-object))
         (n (next-id ro)))
    (set-next-id! ro (+ n 1))
    n))

;;;

(define-class <p-object> (<object>) :abstract
  (id type: <fixnum> init-value: 0)             ; locally unique id
  (properties init-value: '#())                 ; related and owned objects
  (relations init-value: '#())                  ; related but not owned objects
  (history init-value: '()))                    ; transactions against self

;;

(define-class <user> (<p-object>)
  (name type: <string>)
  (fullname type: <string>)
  (email init-value: #f)
  (authority-vector type: <vector> init-value: '#())
  (super-user? type: <boolean> init-value: #f)
  (access-host-list type: <list> init-value: '()))

(define-method migrate ((self <object>))
  self)

(define-method migrate ((self <user.old>))
  (make <user>
        name: (name self)
        fullname: (fullname self)
        authority-vector: (authority-vector self)
        super-user?: (super-user? self)
        access-host-list: (access-host-list self)))

(define-class <user.old> (<object>)         ; XXX inherit from <p-object>
  (name type: <string>)
  (fullname type: <string>)
  (authority-vector type: <vector> init-value: '#())
  (super-user? type: <boolean> init-value: #f)
  (access-host-list type: <list> init-value: '()))

(define-method email ((self <user-1_0>))
  (if (string=? (name self) "donovan")
      "donovan@tippingpoint.com"
      #f))

(define-method user-area ((self <user-1_0>)) 
  #f)

;;

(define-class <user> (<p-object>)
  (name type: <string>)
  (fullname type: <string>)
  (email init-value: #f)
  (authority-vector type: <vector> init-value: '#())
  (super-user? type: <boolean> init-value: #f)
  (access-host-list type: <list> init-value: '()))

(define-method set-user-area! ((self <user>) area)
  (set-property! self 'area area))

(define-method user-area ((self <user>)) 
  (get-property self 'area #f))

;;

(define-class <transaction> (<object>) :abstract
  (process-time init-function: time)
  (user type: <user>)
  (operation init-value: #f))

(define-thread-var *transaction*)

(define (current-transaction)
  *transaction*)

(define (current-user)
  (user (current-transaction)))


(define (with-transaction (xact <transaction>) thunk)
  (thread-let ((*transaction* xact))
    (thunk)))

;;;

(define-class <relationship-1_1> (<object>)
  establish-xaction
  kind
  participants)

(define-class <ibis-relationship> (<relationship>)
  (establish-xaction init-function: current-transaction))
  
(define (establish-relation name . parts)
  (apply make-relation
         <ibis-relationship>
         name
         parts))


