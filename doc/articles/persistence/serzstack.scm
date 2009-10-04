,(use graphics.fontmgr)

(define *annotation-font* (get-text-font "Times" "Italic" 7))

(define *stack-item-font* (get-text-font "Times" "Roman" 10))
(define *stack-item-height* 20)
(define *stack-item-gap* 25)
(define *stack-item-width* 100)
(define *stack-item-round* (/ *stack-item-height* 2))
 
(define (stack-item-frame i)
  (make-rect 50 (* i (+ *stack-item-gap* *stack-item-height*))
             *stack-item-width*
             *stack-item-height*))

(define (stack-item i type label)
  (let ((r (stack-item-frame i)))
    (case type
      ((round) (roundrectstroke r *stack-item-round*))
      ((square) (rectstroke r)))
    ;;
    (setfont *stack-item-font*)
    (cshow (center-x r) (- (center-y r) 3) label)))

;;;  Return a <line> that represents the 
;;;  extent of the stacked items 
;;;  (and, horizontally, is off a bit to the right)

(define (stack-span i n)
  (let ((f0 (stack-item-frame i))
        (f1 (stack-item-frame (- (+ i n) 1))))
    (make-line (+ 10 (limit-x f0))
               (limit-y f1)
               (+ 10 (limit-x f0))
               (origin-y f0))))

(define (updown-arrows i label)
  (let ((f0 (stack-item-frame i))
        (f1 (stack-item-frame (+ i 1)))
        (dx 10))
    (arrowstroke (list (make-point (- (center-x f0) dx)
                                   (limit-y f0))
                       (make-point (- (center-x f0) dx)
                                   (origin-y f1)))
                 setback: 0.5)
    (arrowstroke (list (make-point (+ (center-x f0) dx)
                                   (origin-y f1))
                       (make-point (+ (center-x f0) dx)
                                   (limit-y f0)))
                 setback: 0.5)
    (with-gstate-saved
     (lambda ()
       (translate (make-point (+ (center-x f0) dx)
                              (/ (+ (origin-y f1) (limit-y f0)) 2)))
       (label)))))
  

(define (draw)
  (stack-item 4 'square "page")
  (stack-item 3 'round "serializer")
  (stack-item 2 'round "LRU model")
  (stack-item 1 'round "libz")
  (stack-item 0 'square "disk")
  ;;
  (updown-arrows 0 (lambda ()
                     (setfont *annotation-font*)
                     (moveto 7 -2)
                     (show "bytes")))
  (updown-arrows 1 (lambda ()
                     (setfont *annotation-font*)
                     (moveto 7 -2)
                     (show "symbols")))
  (updown-arrows 2 (lambda ()
                     (setfont *annotation-font*)
                     (moveto 7 -2)
                     (show "words")))
  (updown-arrows 3 (lambda ()
                     (setfont *annotation-font*)
                     (moveto 7 -2)
                     (show "words")))
  ;;
  (draw-long-brace base: (stack-span 0 2)
                   ontip: (lambda ()
                            (rotate 90)
                            (moveto 7 -2)
                            (setfont *annotation-font*)
                            (show "LSS")))
  (draw-long-brace base: (stack-span 2 3)
                   ontip: (lambda ()
                            (rotate 90)
                            (moveto 7 -2)
                            (setfont *annotation-font*)
                            (show "RStore"))))
                            
