(load "heappic.scm")

(define *heap-picture-size* (make-size 100 60))

(define *heap-grid* (make-size 8 4))


(define (draw* upgen black-list gray-list)
  (translate (make-point 100 100))
  ;(background-grid)
  ;;
  (bind ((proot (show-heap 0 'll "Persistent"))
         (troot (show-heap 1 'ul "Transient"))
         (a (obj 1  1 2))
         (b (obj 1  3 3))
         (c (obj 1  6 3))
         ;;
         (d (obj 0  4 3))
         (e (obj 0  3 1))
         (f (obj 0  6 1)))
    ;;
    (define (label n name)
      (let ((draw-color (cond
                         ((memq name black-list) 'draw-black)
                         ((memq name gray-list) 'draw-gray)
                         (else 'draw))))
        (gsaved
         (n draw-color)
         (case draw-color
           ((draw-black) (setcolor (device-color 'white))))
         (cshow (x (n 'at)) (- (y (n 'at)) 2) (symbol->string name)))))
    ;;
    (setfont *label-font*)
    (label a 'a)
    (label b 'b)
    (label c 'c)
    (label d 'd)
    (label e 'e)
    (label f 'f)
    ;;
    (edge troot a)
    (edge proot d)
    (edge a b)
    (edge b c)
    (edge f e)
    (upgen b c d e f)))


(define (draw)
  (draw* (lambda (b c d e f) (edge d f) (edge c f) (edge b e)) '() '()))

;;; Get partway through the transient GC traversal

(define (draw1)
  (draw* (lambda (b c d e f) (edge d f) (edge c f) (edge b e)) '(a b) '(c e)))

;;; Now do some mutations

(define (draw2)
  (draw* (lambda (b c d e f) (edge d e) (edge c e) (edge b f)) '(a b) '(c e)))

;;; Finally, finish the transient GC traversal

(define (draw3)
  (draw* (lambda (b c d e f) (edge d e) (edge c e) (edge b f)) '(a b c) '(e)))
  

