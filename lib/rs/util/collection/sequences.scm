
(define-method sequence-length ((self <empty-list>))
  0)

(define-method sequence-length ((self <pair>))
  (length self))

(define-method sequence-length ((self <vector>))
  (vector-length self))

;;;

(define-method sequence->vector ((self <vector>))
  self)

(define-method sequence->vector ((self <empty-list>))
  '#())

(define-method sequence->vector ((self <pair>))
  (list->vector self))

;;;

(define-method sequence->list ((self <vector>))
  (vector->list self))

(define-method sequence->list ((self <list>))
  self)

;;;

(define (sequence-as (seq <sequence>) type)
  (cond
   ((eq? type <vector>)
    (sequence->vector seq))
   ((subclass? type <list>)
    (sequence->list seq))
   (else
    (error "sequence-as: unsupported target type ~s" type))))

;;
;; special hack to work around bug in object system (#undef)
;;

(define-method initial-state ((self <object>))
  #f)