
#|
(define-class <n.user> (<user>)
  (name type: <string>)
  (fullname type: <string>)
  (email init-value: #f)
  (authority-vector type: <vector> init-value: '#())
  (super-user? type: <boolean> init-value: #f)
  (access-host-list type: <list> init-value: '())
  (area type: <string>)
  (properties init-value: '()))
|#

          
;;;************************************************************
;;;       FROM
;;;      _   ___  
;;;     / | / _ \ 
;;;     | || | | |
;;;     | || |_| |
;;;     |_(_)___/ 

(define migrate-user-1_0 (make-simple-migrator <user-1_0> <user>))

(define-method obj-migrate-1_0-1_1 ((self <object>))
  self)

(define-method obj-migrate-1_0-1_1 ((self <user-1_0>))
  (migrate-user-1_0 self))

(define (migrate-1_0-1_1 root)
  (migrate-pstore root obj-migrate-1_0-1_1))

;;;************************************************************
;;;       FROM
;;;      _   _ 
;;;     / | / |
;;;     | | | |
;;;     | |_| |
;;;     |_(_)_|
;;;

(define migrate-relation-1_1 (make-simple-migrator <relationship-1_1>
                                                   <ibis-relationship>))

(define-method obj-migrate-1_1-1_2 ((self <object>))
  self)

(define-method obj-migrate-1_1-1_2 ((self <relationship-1_1>))
  (migrate-relation-1_1 self))

(define (migrate-1_1-1_2 root)
  (migrate-pstore root obj-migrate-1_1-1_2))

;;;************************************************************
;;;        FROM
;;;      _   ____  
;;;     / | |___ \ 
;;;     | |   __) |
;;;     | |_ / __/ 
;;;     |_(_)_____|
;;;           
;;;
;;;
;;;  returns multiple values: 
;;;    [0] the new root
;;;    [1] statistics on the migration == (<n-visits> <n-migrates>)
;;;

(define *migrate-fns*
  `(
    ((0 1) ,migrate-1_0-1_1)
    ((1 2) ,migrate-1_1-1_2)
    ))

(define (get-migration-fn from to)
  (let ((a (assoc (list from to) *migrate-fns*)))
    (if a
        (cadr a) 
        #f)))

#|

  Object Model Differences
  ========================

  1.0 => 1.1
  ----------
  + added <p-object> abstract superclass,
  ! made <user> inherit from <p-object> instead of <object>
  + added `email' attribute to <user>

  1.1 => 1.2
  ----------
  ! made <ibis-relationship> a subclass of <relationship>,
    moving established-xaction slot from slot[0] to slot[2]

|#

