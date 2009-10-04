,(use graphics.fontmgr)

(load "figures-util.scm")

(define *member-name-font* (get-text-font "Helvetica" "Regular" 8))
(define *annotation-font* (get-text-font "Times" "Italic" 7))

(define *word-width* 40)
(define *word-height* 15)

(define (word-frame i n)
  (make-rect (+ 100 (* i *word-width*))
             100
             (* n *word-width*)
             *word-height*))

(define (word-span i n)
  (let ((f (word-frame i n)))
    (make-line (limit-x f)
               (- (origin-y f) 7)
               (origin-x f)
               (- (origin-y f) 7))))

(define (words i n label)
  (let ((f (word-frame i n)))
    (rectstroke f)
    (cshow (center-x f) (+ (origin-y f) 4) label)))

(define (words* i n label)
  (let ((f (word-frame i n)))
    (rectstroke-middash-h f)
    (cshow (center-x f) (+ (origin-y f) 4) label)))

(define (word-brace i n text)
  (draw-long-brace 
   base: (word-span i n)
   ontip: (lambda ()
            (rotate 180)
            (setfont *annotation-font*)
            (cshow 0 -10 text))))
  
(define (draw)
  (setfont *member-name-font*)
  (words 0 1 "magic")
  (words 1 1 "recnum")
  (words 2 1 "space")
  (words 3 1 "length")
  (words* 4 2 "payload")
  (words* 6 1 "padding")
  ;;
  (word-brace 0 4 "Record Header"))
