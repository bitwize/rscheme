
(define-class <graphic-tic-style> (<object>) :abstract)

(define-class <graphic-tic-mark> (<graphic-tic-style>)
  (inside-length init-value: 0)         ; #t ==> all the way across frame
  (outside-length init-value: 2)
  (inside-far-side? init-value: #f))    ; #t ==> also draw `inside' on far side

(define-class <graphic-tic-label> (<graphic-tic-style>)
  (font type: <text-font> init-value: $default-tic-label-font)
  (anchor init-value: '(right center))
  (distance init-value: 4)
  (cached-z-bbox type: <rect> init-value: $zero-rect))

(define (z-bbox (self <graphic-tic-label>))
  (if (eq? (cached-z-bbox self) $zero-rect)
      (set-cached-z-bbox! self (string-bbox (font-metrics (font self)) "0")))
  (cached-z-bbox self))

;;;

(define-class <graphic-tic-set> (<object>)
  (container type: <graphic-axis>)
  (count type: <fixnum>)
  (generator type: <function>)
  (style type: <graphic-tic-style>))

;;;

(define (union-points lst)
  (if (null? lst)
      #f
      (let loop ((l (cdr lst))
                 (minx (x (car lst)))
                 (maxx (x (car lst)))
                 (miny (y (car lst)))
                 (maxy (y (car lst))))
        (if (null? l)
            (bbox-rect minx miny maxx maxy)
            (let (((p <point>) (car l)))
              (loop (cdr l)
                    (min minx (x p))
                    (max maxx (x p))
                    (min miny (y p))
                    (max maxy (y p))))))))

(define-method tic-mark-style-bbox ((self <graphic-tic-label>)
                                    (for <graphic-tic-set>)
                                    frame
                                    value-generator)
  (let ((box (union-rects
              (map
               (lambda (x)
                 (let* ((l (cons 'span (compute-label-at (container for) x)))
                        (p (point+ (point-on (container for) x)
                                   (size* (outside-unit (container for))
                                          (distance self))))
                        (box (rendering-bbox 
                              l
                              (font self)
                              (point+ p (attachment-point self l)))))
                   ;;(format #t "[bbox for ~s => ~s]\n" (cdr l) box)
                   box))
               (value-generator)))))
    ;;
    ;;(format #t "[bbox for ~s => ~s]\n" self box)
    box))

(define-method tic-mark-style-bbox ((self <graphic-tic-mark>)
                                    (for <graphic-tic-set>)
                                    frame
                                    value-generator)
  (let* ((a (origin (container for)))           ; origin
         (b (point+ a (size (container for))))  ; end point (along axis)
         (u (outside-unit (container for))))    ; outside unit length
    ;;
    (define (slice o)           ; slice relative to `o'
      (let* ((t1 (point+ o (size* u (outside-length self))))
             (t2 (point+ o (size* u (- (inside-length self)))))
             (far (point+ o (cross-size (container for) frame)))
             (t4 (point+ far (size* u (inside-length self))))
             (near-side (union-points (list t1 t2)))
             (far-side (delay (union-points (list far t4)))))
        #|(format #t "~s (~s): cross size ~s, far side ~s\n"
                self
                o
                (cross-size (container for) frame)
                far)
        (format #t "         (~s ~s) => ~s\n"t1 t2 near-side)
        (format #t "         ~s ~s\n" near-side (force far-side))
        |#
        (if (inside-far-side? self)
            (union-rect near-side (force far-side))
            near-side)))
    ;;
    (let* ((slicea (slice a))
           (sliceb (slice b))
           (box (union-rect slicea sliceb)))
      ;;(format #t "[bbox for ~s => ~s + ~s = ~s]\n" self slicea sliceb box)
      box)))

(define (tic-mark-set-bbox (self <graphic-tic-set>) frame)
  (tic-mark-style-bbox (style self)
                       self
                       frame
                       (lambda ()
                         (map (lambda (i)
                                ((generator self) i))
                              (range (count self))))))

(define (draw-tic-mark-set (self <graphic-tic-set>) dev frame)
  (for-each
   (lambda (i)
     (let ((x ((generator self) i)))
       ;(format #t "tic mark [~d] at ~s\n" i x)
       (draw-tic (style self) self x dev frame)))
   (range (count self))))

;;;

(define-method draw-tic ((self <graphic-tic-label>) 
                         (for <graphic-tic-set>)
                         x
                         dev
                         frame)
  (let* ((l (cons 'span (compute-label-at (container for) x)))
         (p (point+ (point-on (container for) x)
                    (size* (outside-unit (container for))
                           (distance self)))))
    (do-rendering l (font self) dev (point+ p (attachment-point self l)))))

(define-method draw-tic ((self <graphic-tic-mark>) 
                         (for <graphic-tic-set>)
                         x
                         dev
                         frame)
  (let* ((o (point-on (container for) x))
         (u (outside-unit (container for))))
    (moveto dev (point+ o (size* u (outside-length self))))
    (lineto dev (point+ o (size* u (- (inside-length self)))))
    (stroke dev)
    (if (inside-far-side? self)
        (let ((far (point+ o (cross-size (container for) frame))))
          (moveto dev far)
          ;; sense of `outside-unit' 
          ;; flips on the far side...
          (lineto dev (point+ far (size* u (inside-length self))))
          (stroke dev)))))
        
(define (cross-size (self <graphic-axis>) frame)
  (let ((u (size* (outside-unit self) -1)))
    (size*
     u
     (inner-product u (make-size (size-width frame) (size-height frame))))))

;;;  figure out where, relative to the tic endpoint, we have to move
;;;  in order to get things hooked up right

(define (attachment-point (self <graphic-tic-label>) text)
  (let* ((w (calculate-rendering-width text (font self)))
         (zbb (z-bbox self)))
    (bbox-anchor 
     ;; note that we are intentionally ignoring any effect on the
     ;; bbox from superscripts
     (make-rect (origin-x zbb) (origin-y zbb) w (size-height zbb))
     (car (anchor self))
     (cadr (anchor self)))))

(define (bbox-anchor (box <rect>) x-anchor y-anchor)
  (make-size
   (case x-anchor
     ((left) (- (origin-x box)))
     ((center) (- (center-x box)))
     ((right) (- (limit-x box))))
   (case y-anchor
     ((bottom) (- (origin-y box)))
     ((center) (- (center-y box)))
     ((top) (- (limit-y box))))))

(define (style-parms head font)
  (let ((f (case head
             ((span) font)
             ((symbol) (get-text-font "Symbol" "Regular" (font-size font)))
             ((sup) (smaller-font font))))
        (dy (case head
              ((span symbol) 0)
              ((sup) (/ (font-size font) 2)))))
    (values f dy)))



(define-method rendering-bbox ((self <string>) font at)
  (values
   (offset-rect (string-bbox (font-metrics font) self) (x at) (y at))
   (point+ at (make-size (string-width font self) 0))))


(define-method rendering-bbox ((self <pair>) font at)
  (bind ((f dy (style-parms (car self) font)))
    (let loop ((l (cdr self))
               (at at)
               (r '()))
      (if (null? l)
          (values (union-rects r) at)
          (bind ((box new-at (rendering-bbox (car l) 
                                             f 
                                             (point+ at (make-size 0 dy)))))
            (loop (cdr l) new-at (if box (cons box r) r)))))))


(define-method do-rendering ((self <string>) font dev at)
  (moveto dev at)
  (setfont dev font)
  (show dev self)
  (point+ at (make-size (string-width font self) 0)))

(define-method do-rendering ((self <pair>) font dev at)
  (bind ((f dy (style-parms (car self) font)))
    ;;
    (let loop ((l (cdr self))
               (at at))
      (if (null? l)
          at
          (loop
           (cdr l)
           (do-rendering (car l) f dev (point+ at (make-size 0 dy))))))))
        
(define-method calculate-rendering-width ((self <string>) font)
  (string-width font self))

(define-method calculate-rendering-width ((self <pair>) font)
  (bind ((f dy (style-parms (car self) font)))
    (reduce + 0 (map (rcurry calculate-rendering-width f) (cdr self)))))
  

(define (smaller-font font)
  (get-text-font (font-family font)
                 (font-style font)
                 (* 0.8 (font-size font))))
