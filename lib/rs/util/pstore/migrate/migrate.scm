
(define (simple-migrator-f new-class slot-procs)
  (lambda (obj)
    (apply make-gvec
           new-class
           (map (lambda (p) 
                  (p obj))
                slot-procs))))

(define (slot-migrator slot override from-slot)
  (cond
   (override
    (if (procedure? override)
        override
        (lambda (obj)
          override)))
   (from-slot
    (let (((k <fixnum>) (index from-slot)))
      (lambda (obj)
        (gvec-ref obj k))))
   (else
    (case (initialization-mode slot)
      ((optional prohibited)
       (let ((v (init-value slot)))
         (lambda (obj)
           v)))
      ((function)
       (let ((f (init-value slot)))
         (lambda (obj)
           (f))))
      ((required)
       (error "Initial value for slot `~a' not supplied" (name slot)))))))

(define (make-simple-migrator (from <<class>>) (to <<class>>) . overrides)
  (let ((from-slots (map (lambda (s)
                           (cons (name s) s))
                         (class-compute-slots from)))
        (to-slots (sort (class-compute-slots to)
                        (lambda (a b)
                          (> (index a) (index b)))))
        (overv (keyword-value-list->vector overrides)))
    (let loop ((to-slots to-slots)
               (slot-procs '()))
      (if (null? to-slots)
          (simple-migrator-f to slot-procs)
          (let* ((slot (car to-slots))
                 (o (vassq (init-keyword slot) overv))
                 (f (assq (name slot) from-slots)))
            ;(format #t " (~s)  ~s ~s\n" (name slot) o f)
            (loop (cdr to-slots)
                  (cons (slot-migrator slot
                                       (and o (vector-ref overv o))
                                       (and f (cdr f)))
                        slot-procs)))))))
    
(define (migrate-pstore root migrate)
  (let ((visited (make-object-table))
        (q (make-dequeue))
        (rr (object->allocation-area root))
        ((nedits <fixnum>) 0))
    ;;
    (define (in-heap? o)
      (eq? (object->allocation-area o) rr))
    ;;
    (define (scan-gvec o)
      (let (((n <fixnum>) (gvec-length o)))
        (let iloop (((i <fixnum>) 0))
          (if (eq? i n)
              (values)
              (let ((r (gvec-ref o i)))
                ;(format #t "      [~d] ~#*@60s\n" i r)
                (if (in-heap? r)
                    (let ((z (table-lookup visited r)))
                      (if z                             ; already migrated
                          (if (not (eq? z r))
                              (begin
                                ;(format #t "\t\treplace => ~#*@40s\n" z)
                                (gvec-set! o i z)))
                          (let ((z (migrate r)))        ; need migration
                            (if (not (eq? z r))
                                (begin
                                  ;(format #t "\t\treplace (new) => ~#*@40s\n" z)
                                  (set! nedits (add1 nedits))
                                  (gvec-set! o i z)))
                            (table-insert! visited r z)
                            (if (gvec? z)
                                (dequeue-push-back! q z))))))
                (iloop (add1 i)))))))
    ;;
    (let ((zr (migrate root)))
      (table-insert! visited root zr)
      (dequeue-push-back! q zr))
    ;;
    (let loop (((n <fixnum>) 0))
      (if (dequeue-empty? q)
          (values (table-lookup visited root) 
                  (list n nedits))
          (let ((o (dequeue-pop-front! q)))
            ;(format #t "pop ~#*@60s\n" o)
            (scan-gvec o)
            (loop (add1 n)))))))
