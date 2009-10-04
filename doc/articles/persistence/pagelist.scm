
(define *page-height* 50)
(define *page-width* 75)

(define *page-defs* (make-symbol-table))

(define-thread-var *node-map*)

(define (draw-dotted-frame f left? right?)
  (format #t "dframe ~s ~s ~s\n" left? right? f))

(define-class <layer-device> (<graphics-device>)
  layer-table
  ctm)

(define (with-layer-device proc)
  (make <layer-device>
        layer-table: (make-table)
        ctm: $identity-transform))

(define-method flush-layers ((self <graphics-device>) (source <layer-device>))
  
(define (draw-page-list x y lis)
  (thread-let ((*node-map* (make-symbol-table)))
    (let ((layers (build-page-layers x y lis)))
      

(define (build-page-layers x y lis)
  (let loop ((x x)
             (l lis)
             (p 0)
             (r '()))
    (if (null? l)
        x
        (cond
         ((integer? (car l))
          (loop x (cdr l) (car l) r))
         ((eq? (car l) '...)
          (bind ((dx thunk (dotted-frame-drawer
                            (make-rect x y *page-width* *page-height*)
                            (eq? l lis)
                            (null? (cdr l)))))
            (loop (+ x dx) (cdr l) (+ p 1)
                  (cons (cons 1 thunk) r))))
         ((symbol? (car l))
          (loop (+ x *page-width*)
                (cdr l)
                (+ p 1)
                (append
                 (page-drawers (car l) (make-rect x y *page-width* *page-height*))
                 r)))
         (else
          (error "huh? ~s" l))))))

(define (page-drawers page-name frame)
  (list
   ;; Layer 0: Background fill
   (cons 0 (lambda ()
              (rectfill frame)))
   ;; Layer 1: Frame stroke
   (cons 1 (lambda ()
             (rectstroke frame)))))
             
(define-macro (defpage (name) spec)
  (table-insert!
   *page-defs*
   name
   (lambda (f)
     (execute-page-spec f spec))))

(define (execute-page-spec (frame <rect>) spec)

(define (draw-node f name relx rely)
  ...)

(define (draw-edge from to 
  (table-lookup *node-map* a

(defpage (p0)
  (node (a) 4 3)
  (node (b) 6 5)
  (edge (a b) 1))



  

 
'(... 201 p0 p1 p2 ...)
'(... 1642 p0 p1 ... 1776 p3 p4 ...)
