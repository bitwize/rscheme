(load "~/p/boxer/src/device.scm")

(define (cshow x y fnt text)
  (let ((dev (current-device)))
    (moveto dev (make-point (- x (/ (string-width fnt text) 2)) y))
    (setfont dev fnt)
    (show dev text)))

(define (rshow x y fnt text)
  (let ((dev (current-device)))
    (moveto dev (make-point (- x (string-width fnt text)) y))
    (setfont dev fnt)
    (show dev text)))

(define (chessboard frame)
  ;;
  (let ((cw (/ (size-width frame) 8))
        (ch (/ (size-height frame) 8))
        (dev (current-device))
        (fnt (get-text-font "Helvetica" "Bold" 9)))
    ;;
    (define (square ix iy)
      (with-gstate-saved
       dev
       (lambda ()
         (let* ((y (* iy ch))
                (x (* ix cw))
                (ymax (+ y ch))
                (epsilon 1)
                (ystep 3)
                (dx (+ cw (* 2 epsilon)))
                (dy (+ ch (* 2 epsilon)))
                (x0 (- x epsilon)))
           (rectclip dev (make-rect x y cw ch))
           (let loop ((y (- y cw epsilon)))
             (if (< y ymax)
                 (begin
                   (moveto dev (make-point x0 y))
                   (lineto dev (make-point (+ x0 dx) (+ y dy)))
                   (loop (+ y ystep)))
                 (stroke dev)))))))
    ;;
    (with-gstate-saved
     dev
     (lambda ()
       (translate dev (origin frame))
       ;;
       (with-gstate-saved
        dev
        (lambda ()
          (setlinewidth dev 0.1)
          (rectstroke dev (make-rect 0 0 (size-width frame) 
                                     (size-height frame)))
          ;;(setcolor dev (device-color dev '(cmyk 1 1 0 0)))
          (for-each
           (lambda (i)
             (for-each
              (lambda (j)
                (if (eq? (bitwise-and (bitwise-xor i j) 1) 0)
                    (square i j)))
              (range 8)))
           (range 8))))
       ;;
       (with-gstate-saved
        dev
        (lambda ()
          (for-each 
           (lambda (i)
             (let ((file (string (string-ref "abcdefgh" i)))
                   (rank (to-string (+ i 1))))
               (cshow (* (+ i 0.5) cw) -9 fnt file)
               (rshow -2 (+ (* i ch) 9) fnt rank)))
           (range 8))))
       ;;
       (values)))))


(define (c)
  (with-output-to-device
   "/tmp/chessboard.ps"
   (lambda ()
     (let* ((media (make-rect 0 0 (* 72 8.5) (* 72 11)))
            (margin (inset-rect media 36 36))
            (edge (* 8 25)))
       ;;
       (startpage (current-device))
       (if #f
           (with-gstate-saved
            (current-device)
            (lambda ()
              (setdash (current-device) '#(3 3) 0)
              (rectstroke (current-device) margin))))
       ;;
       (for-each
        (lambda (y)
          (for-each
           (lambda (x)
             (chessboard 
              (make-rect x y edge edge)))
           (distribute 2 edge (origin-x margin) (limit-x margin))))
        (distribute 3 edge (origin-y margin) (limit-y margin)))
       ;;
       (endpage (current-device))))))


;;;
;;;  Find out where `count' things should start
;;;  in order to distribute them evenly from `from'
;;;  to `to' if they are `size' big.
;;;
;;;  For example, distributing 3 things that are 10
;;;  wide from 10 to 50
;;;
;;;  0         10                                     50
;;;  ..........----------------------------------------
;;;            +--------+     +--------+     +--------+
;;;            ^10            ^25            ^40
;;;

(define (distribute count size from to)
  (let ((spacing (/ (- (- to from) size) (- count 1))))
    (map (lambda (i)
           (+ from (* i spacing)))
         (range count))))
