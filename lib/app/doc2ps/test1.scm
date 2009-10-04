(define (tgs)
  (reset *dev*)
  (test1 *dev*)
  (flush-output-port *dev*))

(define (tpr)
  (let ((p (open-ps-device "/tmp/out.ps")))
    (test1 p)
    (close-graphics-device p)))

(define (test1 dev)
  ;(moveto *dev* (make-point 10 10))
  ;(lineto *dev* (make-point 20 20))
  ;(stroke *dev*)
  (let ((s1 (style-compile 'prototype-char-style))
        (s2 (style-compile 'argument-char-style))
        (s3 (style-compile 'prototype-symbol-style))
        (text-frame (make <text-frame>
                          frame: (make-rect 36 36 (* 72 6.5) (* 72 10))
                          num-columns: 2
                          primary-sidebar: 'left)))
    (let ((col-0-frame (list-ref (column-subframes text-frame) 0))
          (col-1-frame (list-ref (column-subframes text-frame) 1)))
      (for-each
       (lambda (y info)
         (moveto dev (make-point (origin-x col-0-frame) y))
         (s1 dev (format #f "(~a" (caar info)))
         (for-each (lambda (x)
                     (s1 dev " ")
                     (s2 dev (format #f "~a" x)))
                   (cdar info))
         (s1 dev ")")
         (moveto dev (make-point (origin-x col-1-frame) y))
         (s3 dev "\336")
         (moveto dev (make-point (+ (origin-x col-1-frame) 14) y))
         (let ((r (cadr info)))
           (if (null? r)
               (s2 dev "no values")
               (for-each (lambda (x)
                           (s2 dev (format #f "~a " x)))
                         r))))
       (map (lambda (k)
              (- 400 (* k 12)))
            (range (length *test-proto*)))
       *test-proto*))
    ;;
    (render-outline text-frame dev)
    ;;
    (let* ((col-0-frame (list-ref (column-subframes text-frame) 0))
           (lines (insert-line-breaks 
                   (layout-line '("This is a test of the silly "
                                  "broadcasting system that we know "
                                  (font emphasis-char-style)
                                  "and"
                                  (font body-char-style)
                                  " love.  Aya to y'all, and to you"
                                  " all a good night!  This is all I"
                                  " bade you, and this is all thou "
                                  "shalt respondest to.")
                                'body-char-style)
                   line-width: (size-width col-0-frame))))
      (for-each
       (lambda (y l last?)
         (render-layout dev (fold-over-struts 
                             (if last?
                                 (no-stretch l)
                                 (apply-stretch 
                                  l
                                  (- (size-width col-0-frame)
                                     (line-width l)))))
                        (point+ (upper-left col-0-frame)
                                (make-size 0 (* -14 (+ y 1))))))
       (range (length lines))
       lines
       (reverse (cons #t (map (lambda (x) #f)
                              (cdr lines))))))
    ))

                         

;;;

(define *test-proto*
  '(((chmod path mode) ())
    ((fchmod filedes mode) ())
    ((stat path) (statbuf))))

    
