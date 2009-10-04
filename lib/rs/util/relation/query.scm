(define (build-query-plan keyset)
  (let ((launch (pick-smallest-cost keyset)))
    (cons launch (delq launch keyset))))

(define (pick-smallest-cost keyset)
  (let* ((s (car keyset))
         (c (estimate-cost (car keyset))))
    (if (or (null? (cdr keyset)) (= c 1))
        (values s c)
        (bind ((s+ c+ (pick-smallest-cost (cdr keyset))))
          (if (< c c+)
              (values s c)
              (values s+ c+))))))

(define (estimate-cost (s <relation-participant>))
  (let ((s? (single-membership? s (in-relation s))))
    (cond
     (s?
      1)
     ((in-properties? s)
      30)
     ((use-slot s)
      10)
     (else
      100))))

;;;

;(define (ppd x) (pp x) x)

(define-macro (define-queries rname)
  (let* ((qname (symbol-append rname ":query"))
         (rel (s-expr->value rname))
         (clist (key-combinations rel))
         (ilist (range (- (length clist) 1))))
    ;;
    `(begin
       ,@(map (lambda (i c)
                `(define (,(symbol-append qname ":" i) 
                          ,@(map 
                             (lambda (attr)
                               (list (name attr) 
                                     (name (type-restriction attr))))
                             c))
                   (let ((_rel ,rname))
                     ,(gen-query-body rel c))))
              ilist
              clist)
       ;;
       (define-macro ,qname
         (macro-rules ()
                      ,@(map (lambda (i c)
                               (combo->macro-rule
                                c
                                (list 
                                 'quasiquote
                                 (cons (symbol-append qname ":" i) 
                                       (map (lambda (a)
                                              (list 'unquote (name a)))
                                            c)))))
                             ilist
                             clist)
                      ((_) ,(list 'quasiquote
                                  `(extent->list (extent ,rname))))))
       ;;
       ;; Check to make sure a given tuple (an instance of 'self') can
       ;; be established in the relation without violating candidate key
       ;; constraints
       (define-method constraint-check-establish ((self ,rname))
         ,@(map (lambda (ck)
                  `(if (pair? (query ,rname 
                                     ,@(apply append
                                              (map 
                                               (lambda (c)
                                                 (list
                                                  (symbol->keyword (name c))
                                                  `(gvec-ref self ,(index c))))
                                               ck))))
                       (signal (make <relation-already-established>
                                     tuple: self
                                     relation-key: ',(map name ck)))))
                (candidate-keys rel))))))
       ;;

(define (gen-query-body (rel <<relation>>) given)
  (if (= (length given) 1)
      (gen-query-body-1 rel (car given) (lambda (e) '()))
      (let ((order (build-query-plan given)))
        (gen-query-body-1 rel 
                          (car order)
                          (lambda (e)
                            (map (lambda (c)
                                   `(eq? (gvec-ref ,e ,(index c))
                                         ,(name c)))
                                 (cdr order)))))))


(define (gen-query-body-1 (rel <<relation>>) (s <relation-participant>)
                          fclausef)
  (cond
   ;;
   ((not (use-slot s))
    ;(format #t "using extents to query ~s\n" s)
    (let ((j (index s)))
      `(let ((_lst (extent->list (extent _rel)))
             (_tmp ,(name s)))
          ;;
         (let loop ((l _lst)
                    (r '()))
           (if (null? l)
               r
               (loop (cdr l)
                     (let (((_row ,(name rel)) (car l)))
                       (if (and (eq? (gvec-ref _row ,j) _tmp)
                                ,@(fclausef '_row))
                         (cons _row r)
                         r))))))))
   ;;
   ((in-properties? s)
    ;(format #t "using property ~s to query ~s\n" (use-slot s) s)
    (let ((c (type-restriction s))
          (k (use-slot s)))
      (if (single-membership? s rel)
          `(let ((_ptr (get-property ,(name s) ',k #f)))
             (if (and _ptr ,@(fclausef '_ptr))
                 (cons _ptr '())
                 '()))
          (let ((check (fclausef '_row)))
            (if (null? check)
                `(get-property ,(name s) ',k '())
                (gen-scanner (name rel)
                             `(get-property ,(name s) ',k '())
                             `(and ,@check)))))))
   ;;
   (else
    ;(format #t "using slot ~s to query ~s\n" (use-slot s) s)
    (let ((c (type-restriction s))
          (j (index (use-slot s))))
      (if (single-membership? s rel)
          `(let ((_ptr (gvec-ref ,(name s) ,j)))
              (if (and _ptr ,@(fclausef '_ptr))
                  (cons _ptr '())
                  '()))
          (let ((check (fclausef '_row)))
            (if (null? check)
                `(gvec-ref ,(name s) ,j)
                (gen-scanner (name rel)
                             `(gvec-ref ,(name s) ,j)
                             `(and ,@check)))))))
   ;;
   ))
  
(define (gen-scanner row-type list-expr test-expr)
  `(let loop ((l ,list-expr)
              (r '()))
     (if (null? l)
         r
         (loop (cdr l)
               (let (((_row ,row-type) (car l)))
                 (if ,test-expr
                     (cons _row r)
                     r))))))

#|
(define (dequeue-select! deq (proc <function>))
  (let loop (((n <fixnum>) (dequeue-count deq)))
    (if (eq? n 0)
        deq
        (let ((item (dequeue-pop-front! deq)))
          (if (proc item)
              (dequeue-push-back! deq item))
          (loop (sub1 deq))))))
|#

(define (combo->macro-rule combo body)
  (if (null? combo)
      `((_) ,body)
      `((_ #key ,@(map name combo)) ,body)))

(define (key-combinations (self <<relation>>))
  (sort (combos (direct-slots self))
        (lambda (a b)
          (> (length a) (length b)))))

(define (combos lst)
  (if (null? lst)
      '(())
      (let ((r (combos (cdr lst))))
        (append (map (lambda (c)
                       (cons (car lst) c))
                     r)
                r))))


;;;

(define-macro (query target . opts)
  `(,(symbol-append target ":query") ,@opts))

