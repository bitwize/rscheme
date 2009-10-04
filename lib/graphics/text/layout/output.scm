
(define (show-packed-lines dev x0 y0 lines leading)
  (set! leading (qty->scaled leading))
  (let ((x0 (qty->scaled x0)))
    (let loop ((i 0)
               (y (qty->scaled y0)))
      (if (< i (vector-length lines))
          (let (((p <packed-line>) (vector-ref lines i)))
            (show-packed-line dev x0 (- y (height p)) p)
            (loop (+ i 1) (- y (depth p) (height p) leading)))))))


(define (show-packed-line dev x0 baseline (p <packed-line>))
  (let ((set (glue-set p)))
    (let runs ((l (vector->list (line p)))
               (x x0))
      (if (pair? l)
          (bind ((rest xr (xshow-run dev x baseline l set)))
            (runs rest xr))))))

(define (xshow-run dev x y lst setting)
  (let ((accum (open-output-string))
        (widths (make-dequeue))
        (fnt #f))
    ;;
    (define (flush)
      (if fnt
          (begin
            ;(dequeue-push-back! widths ...)
            ;(dequeue-pop-front! widths)
            (setfont dev fnt)
            (moveto dev (make-point (* x 0.001) (* y 0.001)))
            (xshow dev 
                   (get-output-string accum)
                   (map (lambda (dx)
                          (* dx 0.001))
                        (vector->list (dequeue-state widths)))))))
    ;;
    (let loop ((l lst)
               (sx x))
      (cond
       ((null? l)
        (flush)
        (values '() sx))
       ;;
       ((or (instance? (car l) <glue-node>)
            (instance? (car l) <kern-node>))
        (let ((w (set-size (car l) setting)))
          (if (and fnt (> w 0))
              (begin
                (output-port-write-char accum #\space)
                (dequeue-push-back! widths w)
                (loop (cdr l) (+ sx w)))
              (let ((n (dequeue-count widths)))
                (if (eq? n 0)
                    (begin
                      (set! x (+ x w))
                      (loop (cdr l) x))
                    (begin
                      (dequeue-set! widths
                                    (sub1 n)
                                    (+ (dequeue-ref widths (sub1 n)) w))
                      (loop (cdr l) (+ sx w))))))))
       ;;
       ((instance? (car l) <char-node>)
        (if (not fnt)
            (set! fnt (font (car l))))
        (if (not (eq? fnt (font (car l))))
            (begin
              (flush)
              (values l sx))
            (let ((w (width (car l))))
              (dequeue-push-back! widths w)
              (output-port-write-char accum (content (car l)))
              (loop (cdr l) (+ sx w)))))
       ;;
       (else
        (loop (cdr l) sx))))))

(define-method set-size ((self <glue-node>) (setting <glue-set>))
  (set-size (content self) setting))

(define-method set-size ((self <kern-node>) (setting <glue-set>))
  (width self))

(define-method set-size ((self <glue>) (setting <glue-set>))
  (if (eq? setting $zero-glue-set)
      (natural self)
      (if (eq? (glue-set-sign setting) 1)
          (if (eq? (stretch-order self) (glue-set-order setting))
              (+ (natural self)
                 (* (glue-set-ratio setting) (stretch self)))
              (natural self))
          (if (eq? (shrink-order self) (glue-set-order setting))
              (- (natural self)
                 (* (glue-set-ratio setting) (shrink self)))
              (natural self)))))
          
