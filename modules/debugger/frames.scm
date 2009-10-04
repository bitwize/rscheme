
(define-class <frame> (<object>) :abstract
  (index type: <fixnum> init-value: -1))

(define-class <continuation-frame> (<frame>)
  (delegate type: <partial-continuation>))

(define-class <apply-frame> (<frame>)
  (delegate type: <vector>))

(define-class <fluid-contour-frame> (<frame>)
  (delegate type: <fluid-tl-contour>))
  

;;;

(define (build-frames (pc <partial-continuation>) (ds <list>) stop?)
  (let loop ((pc pc)
             (ds ds)
             (r '()))
    (if (or (not pc) (stop? pc))
        (begin
          (for-each set-index! r (range (length r)))
          (reverse! r))
        (if (eq? (pc-jump-addr pc) $fin-w-call-pc)
            (if (or (eq? (gvec-ref pc 4) ds) (null? ds))
                (loop (pc-continuation-reg pc) ds r)
                (loop pc 
                      (cdr ds)
                      (push-a-frame (car ds) r)))
            (loop (pc-continuation-reg pc) 
                  ds
                  (push-c-frame pc r))))))

(define-method push-a-frame ((self <fluid-tl-contour>) r)
  (cons (make <fluid-contour-frame>
              delegate: self)
        r))

(define-method push-a-frame ((self <vector>) r)
  (cons (make <apply-frame>
              delegate: self)
        r))

(define-method push-a-frame ((self <object>) r)
  r)
    
(define (push-c-frame (self <partial-continuation>) r)
  (cons (make <continuation-frame>
              delegate: self)
        r))

;;;

(define (frames #optional (skip default: 0))
  (frames* skip 'eval (get-dynamic-state-reg)))

(define (vm-state->frame-list (pc0 <partial-continuation>) 
                              dyn-state
                              stopkey)
  (bind ((name monotones (get-c-function-descr
                          (linkage-info (template backstop))))
         (backstop-pc (cadr monotones)))
    (build-frames pc0
                  dyn-state
                  (lambda (pc)
                    (and (eq? (pc-jump-addr pc) backstop-pc)
                         (pair? (pc-reg-ref pc 0))
                         (eq? (car (pc-reg-ref pc 0)) stopkey))))))

(define (frames* skip stopkey dyn-state)
  (list-tail
   (low-level-call/cc
    (lambda (ll-continuation)
      (vm-state->frame-list (ll->partial ll-continuation) dyn-state)))
   skip))

(define (condition-frames* stack stopkey)
  (bind ((name monotones (get-c-function-descr
                          (linkage-info (template backstop))))
         (backstop-pc (cadr monotones)))
    (build-frames (vm-continuation-reg stack)
                  (vm-dynamic-state-reg stack)
                  (lambda (pc)
                    (and (eq? (pc-jump-addr pc) backstop-pc)
                         (pair? (pc-reg-ref pc 0))
                         (eq? (car (pc-reg-ref pc 0)) stopkey))))))
                    

;;;

(define-method print-1-frame ((self <fluid-contour-frame>) port)
  (format port "_______________(~d)_______________ FLUID-BINDING\n"
          (index self))
  ;;
  (let ((bdg (ftlc-bindings (delegate self)))
        (val (ftlc-inside-values (delegate self))))
    (for-each (lambda (k)
                (format port "       ~s => ~#@*60s\n"
                        (name (vector-ref bdg k))
                        (vector-ref val k)))
              (range (vector-length bdg)))))

(define-method print-1-frame ((self <apply-frame>) port)
  (format port "_______________(~d)_______________ CALL\n"
          (index self))
  ;;
  (let ((proc (vector-ref (delegate self) 0))
        (argv (subvector (delegate self) 1)))
    (if (procedure? proc)
        (let ((n (vector-length argv)))
          (format port "call of ~s\n" proc)
          ;;
          (let loop ((j 0))
            (if (< j n)
                (begin
                  (format port "  with [~d] = ~@*#59s\n" 
                          j
                          (vector-ref argv j))
                  (loop (add1 j)))))))))

(define-method print-1-frame ((self <continuation-frame>) port)
  (format port "===============(~d)================\n"
          (index self))
  (print-1-pc (delegate self)))
