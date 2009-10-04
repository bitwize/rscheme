(define-method x-fillarea ((self <pair>) win gc)
  (for-each (lambda (sub)
              (x-fillarea sub win gc))
            self))

(define-method x-fillarea ((self <rect>) win gc)
  (draw-rectangle win gc (x self) (y self) (width self) (height self) #t))

(define-method x-fillarea ((self <rect>) win gc)
  (draw-rectangle win gc (x self) (y self) (width self) (height self) #t))
  
(define-method x-fillarea ((self <path-bounded-area>) win gc)
  (if (pair? (subpaths self))
      (let ((q (make-dequeue))
            (x0 #f)
            (y0 #f))
        ;;
        (define (go (pt <point>))
          (dequeue-push-back! q (x pt))
          (dequeue-push-back! q (y pt))
          pt)
        ;;
        (define (do-subpath sp)
          (let loop ((l (path-points sp)))
            (if (null? l)
                (go (position (car (path-points sp))))      ; close the path
                (begin
                  (go (position (car l)))
                  (loop (cdr l))))))
        ;;
        (let ((p0 (do-subpath (car (subpaths self)))))
          ;;
          (for-each
           (lambda (sp)
             (do-subpath sp)
             (go p0))
           (cdr (subpaths self)))
          ;;
          (draw-lines win gc (dequeue-state q) fill?: #t)))))
