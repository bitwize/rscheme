
(define-api (database-connect path mode)
  (case mode
    ((read)
     (read-database path))
    ((update)
     (open-database path))
    (else
     (error "invalid connect mode: ~s" mode))))

(define (make-database #rest r)
  (if (keyword? (car r))
      (apply make-database-using-kwds r)
      (apply make-database* r)))

(define (make-database-using-kwds #key 
                                  repository
                                  database-name
                                  su-id
                                  su-fullname
                                  su-email-addr
                                  su-host-user)
  (bind ((s e user host ((reg-expr->proc '(seq (save (+ (not #\@)))
                                               #\@
                                               (save (+ any)))) su-host-user)))
    (make-database* repository
                    database-name
                    su-id
                    su-fullname
                    su-email-addr
                    user
                    host)))

(define-api (make-database* path db-name user-name fullname mail login-name login-host)
    (let* ((u (make <user>
    		   name: user-name
		   id: 0
		   full-name: fullname
		   email-addr: mail
		   remote-hosts: (list (list login-host login-name))
		   authorities: '()
		   properties: '((super-user . #t))
		   audit-log: '()))
	   (g (make <group>
	  	    name: "world"
		    id: 0
		    owner: u
		    audit-log: '()))
	   (a (make-application db-name)))
	;;
	(set-world-group! a g)
	(table-insert! (user-table a) (name u) u)
	(table-insert! (group-table a) (name g) g)
        (set-service-state! a 'available)
	(create-database a path)))


(define (commit-sourcebase)
  (commit *pstore*))
