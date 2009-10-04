;;;
;;;  A <relation> is an association between object instances
;;;

;;; <relation-participant> describes a participant (role) 
;;; in a relation.  In addition to describing the participation,
;;; it is also a slot in an instance of a class of type <<relation>>

(define-method write-object ((self <slot-descriptor>) port)
  (format port "#[~a ~s]" (name (object-class self)) (name self)))

(define-class <relation-participant> (<slot-descriptor>)
  (use-slot init-value: #f)
  (in-properties? init-value: #f)
  (in-relation init-value: #f))

(define-class <<relation>> (<<standard-class>>)
  (candidate-keys init-value: '())
  (keep-extent? init-value: #t))

(define-class <relation-row> (<object>) :gvec :abstract)

(define-class <relation-no-extent> (<condition>)
  (relation type: <<relation>>))

(define-class <relation-already-established> (<condition>)
  (tuple type: <relation-row>)
  (relation-key type: <list>))

(define-class <relation-extent> (<object>)
  (total-count type: <fixnum> init-value: 0 :sealed)
  (instance-list type: <list> init-value: '() :sealed))

(define-method extent ((self <<relation>>))
  (if (keep-extent? self)
      (let ((x (extents (current-universe))))
        (or (table-lookup x (name self))
            (let ((e (make <relation-extent>)))
              (table-insert! x (name self) e)
              e)))
      (signal (make <relation-no-extent>
                    relation: self))))

(define (extent-register (self <relation-extent>) item)
  (set-total-count! self (add1 (total-count self)))
  (set-instance-list! self (cons item (instance-list self))))

(define (extent-unregister (self <relation-extent>) item)
  (assert (memq item (instance-list self)))
  (set-total-count! self (add1 (total-count self)))
  (set-instance-list! self (delq! item (instance-list self))))
  
(define (extent->list (self <relation-extent>))
  (instance-list self))

(define (append-slot (self <<class>>) name type init)
  (let ((slot (make <slot-descriptor>
                    name: name
                    initialization-mode: 'prohibited
                    init-value: init
                    type-restriction: type
                    index: (instance-size self)
                    getter: #f
                    setter: #f
                    properties: '((sealed? . #t))
                    init-keyword: #f)))
    (set-instance-size! self (+ 1 (instance-size self)))
    (set-direct-slots! self (append (direct-slots self) (list slot)))
    slot))

;;; Determine if an object in the given role can only in one relation
;;; at a time.  This is true if the role is, by itself, a candidate key

(define (single-membership? (self <relation-participant>) (for <<relation>>))
  (and (member (list self) (candidate-keys for)) #t))


(define (bind-slot (for <<relation>>) (self <relation-participant>))
  (cond
   ((eq? (use-slot self) #t)
    (let ((n (symbol-append "relation-"
                            (name for)
                            ":as:"
                            (name self))))
      (append-slot (type-restriction self) 
                   n
                   <object>
                   (if (single-membership? self for)
                       #f
                       '()))
      (set-use-slot! self n)
      (bind-slot for self)))
   ((eq? (use-slot self) 'properties)
    (set-in-properties?! self #t)
    (or (slot-descriptor-by-name (type-restriction self) 'properties)
        (error "properties not available on: ~s" 
               (type-restriction self)))
    (set-use-slot! self (symbol-append "relation-"
                                       (name for)
                                       ":as:"
                                       (name self))))
   ((symbol? (use-slot self))
    (set-use-slot! self
                   (or (slot-descriptor-by-name (type-restriction self) 
                                                (use-slot self))
                       (error "no such slot: ~s" (use-slot self)))))))

(define-method finalize-class ((self <<relation>>))
  (next-method)
  (set-candidate-keys! self
                       (map (lambda (ck)
                              (map (lambda (n)
                                     (or (slot-descriptor-by-name self n)
                                         (error "no such role: ~s" n)))
                                   ck))
                            (candidate-keys self)))
  ;;
  (for-each (lambda ((slot <relation-participant>))
              ;(format #t "~s role: ~s\n" (name self) (name slot))
              (set-in-relation! slot self)
              (bind-slot self slot))
            (direct-slots self)))


;;;
;;;  Usage:
;;;
;;;    (define-relation parent-child
;;;      candidate-keys: '((child))
;;;      (parent type: <person> use-slot: #t)           ; allocate new slot
;;;      (child type: <person> use-slot: parent))       ; use existing slot

(define-macro (define-relation name . opts)
  ;;
  (define (parse-off-slots lst)
    (if (null? lst)
        (values '() '())
        (if (keyword? (car lst))
            (bind ((o s (parse-off-slots (cddr lst))))
              (values (cons* (car lst) (cadr lst) o) s))
            (values '() lst))))
  ;;
  (bind ((opts slots (parse-off-slots opts)))
    `(begin
       (define-class ,name (<relation-row>)
         metaclass: <<relation>>
         ,@opts
         ,@(map (lambda (x)
                  (append x '(metaclass: <relation-participant>)))
                slots))
       (define-queries ,name))))


;;;

(define (direct-subclasses (self <<class>>))
  (if (eq? self <object>)
      (vector->list (all-instances <<standard-class>>))
      '()))

;;;

(define (retract (join <relation-row>))                         ; "delete"
  (let (((self <<relation>>) (object-class join)))
    ;;
    (if (keep-extent? self)
        (extent-unregister (extent self) join))
    ;;
    (for-each
     (lambda ((p <relation-participant>))
       (let ((x (gvec-ref join (index p))))
         (if (in-properties? p)
             (if (single-membership? p self)
                 (remove-property! x (use-slot p))
                 (let* ((l (get-property x (use-slot p)))
                        (r (delq! join l)))
                   (if (not (eq? r l))
                       (if (null? r)
                           (remove-property! x (use-slot p))
                           (set-property! x (use-slot p) r)))))
             (if (use-slot p)
                 (let ((k (index (use-slot p))))
                   (if (single-membership? p self)
                       (gvec-set! x k #f)
                       (gvec-set! x k (delq! join (gvec-ref x k))))
                   (values))))))
     (direct-slots self))
    ;;
    (values)))

(define (establish (self <<relation>>) #rest participants)      ; "insert"
  (let ((join (apply make-instance self participants)))
    ;;
    (constraint-check-establish join)
    ;;
    (if (keep-extent? self)
        (extent-register (extent self) join))
    ;;
    (for-each
     (lambda ((p <relation-participant>))
       (let ((x (gvec-ref join (index p))))     ;participant
         (if (in-properties? p)
             (set-property! x
                            (use-slot p)
                            (if (single-membership? p self)
                                join
                                (cons join (get-property x (use-slot p) '()))))
             (if (use-slot p)
                 (let ((k (index (use-slot p))))
                   (if (single-membership? p self)
                       (gvec-set! x k join)
                       (gvec-set! x k (cons join (gvec-ref x k)))))))
         (values)))
     (direct-slots self))
    join))

(define (bind-keyset (self <<relation>>) keyset)
  (map (lambda (n)
         (or (slot-descriptor-by-name self n)
             (error "No such role in ~s: ~s" (name self) n)))
       keyset))

  
;(define pc0 (make <*parent-child*> parent: donovan child: lane))

(if-implements (available rs.db.rstore)
  (with-module rs.db.rstore
    (register-pivot! '(module rs.util.relation <universe>) 
                     <universe>)
    (register-pivot! '(module rs.util.relation <relation-extent>) 
                     <relation-extent>)))

