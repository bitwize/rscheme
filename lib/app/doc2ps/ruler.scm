
(define-method scale ((self <gs-device>) sx sy)
  (format (ps-port self) "~d ~d scale\n" sx sy))

(define (test-ruler dev)
  (with-gstate-saved
   dev
   (lambda ()
     (translate dev (make-point 36 36))
     (scale dev 5 5)
     ;;
     (setfont dev (get-text-font "Times" "Roman" 18))
     (moveto dev $zero-point)
     (show dev "Aya")
     ;;
     (translate dev (make-point 0 18))
     ;;
     (draw-rule dev 0 100)
     ;;
     (draw-margin dev '(l b) 0 18)
     (draw-margin dev '(l t) 0 #f)
     (draw-margin dev '(r b) 100 #f)
     (draw-margin dev '(r t) 100 18)
     ;;
     (draw-tab dev 'left (* 18 2) 18)
     (draw-tab dev 'right (* 18 3) #f)
     (draw-tab dev 'center (* 18 3) #f)
     (draw-tab dev #\. (* 18 4) #f)
     ;;
     )))

(define $major-tic-len 3)
(define $minor-tic-len 1)
(define $margin-indicator-gap 0.5)
(define $margin-indicator-height 3)

(define (draw-rule dev b0 b1)
  (setlinewidth dev 0.25)
  (moveto dev (make-point b0 (- $major-tic-len)))
  (lineto dev (make-point b0 0))
  (lineto dev (make-point b1 0))
  (lineto dev (make-point b1 (- $major-tic-len)))
  (stroke dev)
  (let loop ((i 1))
    (let ((x (+ b0 (* i 18))))
      (if (< x b1)
          (begin
            (moveto dev (make-point x 0))
            (lineto dev (make-point x (- (if (zero? (modulo i 4))
                                             $major-tic-len
                                             $minor-tic-len))))
            (stroke dev)
            (loop (+ i 1)))))))

(define (draw-tab dev t x h)
  (let ((a (- x (/ $margin-indicator-height 2)))
        (b (+ x (/ $margin-indicator-height 2)))
        (c $margin-indicator-height)
        (d (* 2 $margin-indicator-height)))
    (moveto dev (make-point x 0))
    (lineto dev (make-point a c))
    (lineto dev (make-point b c))
    (closepath dev)
    (fill dev)
    ;;
    (setlinewidth dev 0.75)
    (moveto dev (make-point x c))
    (lineto dev (make-point x d))
    (case t
      ((left) (lineto dev (make-point b d)))
      ((right) (lineto dev (make-point a d))))
    (stroke dev)
    ;;
    (draw-vrule dev x h)))

(define (draw-vrule dev x h)
  (if h
      (with-gstate-saved
       dev
       (lambda ()
         (setdash dev '#(1 1) 0)
         (setlinewidth dev 0.25)
         (moveto dev (make-point x 0))
         (lineto dev (make-point x (- h)))
         (stroke dev)))))

(define (draw-margin dev t x h)
  (let* ((lr (car t))
         (tb (cadr t))
         (a (case tb
              ((t) (+ $margin-indicator-gap $margin-indicator-height))
              ((b) (- $margin-indicator-height (/ $margin-indicator-gap 2)))))
         (b (case tb
              ((t) (+ (* 2 $margin-indicator-height)
                      $margin-indicator-gap))
              ((b) 0)))
         (c (case lr
              ((l) (+ x $margin-indicator-height))
              ((r) (- x $margin-indicator-height)))))
    (moveto dev (make-point x (+ a 0.125)))
    (lineto dev (make-point x (+ b 0.125)))
    (lineto dev (make-point c (+ a 0.125)))
    (closepath dev)
    (fill dev)
    (draw-vrule dev x h)))
              