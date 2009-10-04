
;;;   (TYPE TITLE (CHILD ...))

(define *ibis-tree-icons* (gd-image-create-from-png "ibis-ico.png"))

(define-class <image-map-accum> (<object>)
  (items init-value: '()))

(define (image-map-append! (self <image-map-accum>) item)
  (set-items! self (cons item (items self))))

(define (gen-ibis-chart tree)
  (let* ((n (compute-num-lines tree))
         (w 300)
         (h (* n 18))
         (img (gd-image-create w h))
         (black (gd-image-color-resolve img 0 0 0))
         (gray (gd-image-color-resolve img 128 128 128))
         (white (gd-image-color-resolve img 255 255 255))
         (map-info (make <image-map-accum>)))
    (gd-image-filled-rectangle img 0 0 w h white)
    (render-ibis-node tree img 0 0 (vector black) map-info)
    (let ((p (open-output-string)))
      ;(gd-image-png img "/tmp/out.png")
      (gd-image-png-ctx img (open-gd-io-ctx p))
      (values (close-output-port p)
              map-info))))
    
(define (composite-ibis-icon img icon x y)
  (gd-image-copy img *ibis-tree-icons*
                 x y
                 (* icon 18) 0
                 18 18))

(define (render-ibis-node tree img x y col imap)
  ;; draw the icon
  (case (car tree)
    ((issue ?) (composite-ibis-icon img 0 x y))
    ((position p) (composite-ibis-icon img 2 x y))
    ((support +) (composite-ibis-icon img 1 x y))
    ((object -) (composite-ibis-icon img 3 x y))
    (else (error "unknown node type: ~s" (car tree))))
  (image-map-append! imap `((rect ,x ,y 14 14) ,(cadr tree)))
  ;; draw the title
  (gd-image-string-ft img 
                      (vector-ref col 0)
                      "/usr/X11R6/lib/X11/fonts/TTF/luxisr.ttf"
                      10.0
                      0.0
                      (+ x 22) (+ y 14)
                      (cadr tree))
  ;; draw the children
  (bind ((y1 y2 (render-ibis-children (caddr tree) img (+ x 18) (+ y 18) col
                                      imap)))
    (if (pair? (caddr tree))
        (gd-image-line img 
                       (+ x 9) (+ y 17)
                       (+ x 9) (+ y2 9)
                       (vector-ref col 0)))
    y1))

(define (render-ibis-children nodes img x y col imap)
  (let loop ((y y)
             (lst nodes)
             (y1 y))
    (if (null? lst)
        (values y y1)
        (let ((yc (render-ibis-node (car lst) img x y col imap)))
          (gd-image-line img 
                         (- x 9) (+ y 9)
                         x (+ y 9)
                         (vector-ref col 0))
          (loop yc (cdr lst) y)))))
    

(define (compute-num-lines tree)
  (+ 1 (reduce + 0 (map compute-num-lines (caddr tree)))))

(define *test-ibis-tree*
  '(? "Shall we foo?"
      ((p "By all means..."
          ((+ "It's a blast" ())
           (- "It's expensive" ())))
       (p "Never!"
          ((+ "It's gauche" ()))))))

       
