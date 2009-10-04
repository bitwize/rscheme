
(define (render-layout dev runs start)
  (let loop ((r runs)
             (p start))
    (if (null? r)
        p
        (bind ((run (car r))
               (cf (style-compile (car run)))
               (dy (query-style (car run) 'baseline-shift))
               (start items (shift-start p (cdr run)))
               (widths (map (lambda (ent)
                              (let ((tcw (+ (cadr ent) (caddr ent))))
                                (if (integer? tcw)
                                    tcw
                                    (exact->inexact tcw))))
                            items)))
          (moveto dev (point+ start (make-size 0 dy)))
          (cf dev
              (list->string (map car items))
              widths)
          (loop (cdr r)
                (point+ start (make-size (reduce + 0 widths) 0)))))))

(define (shift-start start items)
  (if (and (pair? items)
           (pair? (car items))
           (eq? (caar items) 'strut))
      (shift-start (point+ start (make-size (cadar items) 0))
                   (cdr items))
      (values start items)))

(define (cf-afm cf)
  (query-style (query-style cf 'font) 'afm))

(define (cf-size cf)
  (query-style (query-style cf 'font) 'size))

