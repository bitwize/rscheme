,(use graphics.fontmgr)

(define *word-font* (get-text-font "Helvetica" "Regular" 8))
(define *annotation-font* (get-text-font "Times" "Italic" 7))

(load "figures-util.scm")

(define (draw)
  (let ((y 200)
        (x0 100)
        (w 100)
        (wordh 13))
    ;;
    (define (alloc-points n)
      (let ((y0 y))
        (set! y (- y n))
        (make-rect x0 y w n)))
    ;;
    (define (alloc-named . lst)
      (let* ((n (length lst))
             (r (alloc-points (* n wordh))))
        ;;
        (rectstroke r)
        (for-each (lambda (i item)
                    (with-gstate-saved
                     (lambda ()
                       (translate (make-point 
                                   (origin-x r) 
                                   (- (limit-y r) (* wordh (+ 1 i)))))
                       (item))))
                  (range n)
                  lst)
        r))
    ;;
    (define (word str)
      (setfont *word-font*)
      (moveto 3 3)
      (show str))
    ;;
    (alloc-named
     (lambda () (word "page_type_id"))
     (lambda () (word "num_page_refs")))
    ;;
    (let* ((r (alloc-named
               (lambda () (word "base_page_num"))
               (lambda () (word "flags"))))
           (r2 (alloc-points (* 2 wordh))))
      (rectstroke-middash-v r2)
      ;;
      (draw-long-brace base: (make-line (+ 8 (limit-x r))
                                        (limit-y r)
                                        (+ 8 (limit-x r))
                                        (origin-y r2))
                       ontip: (lambda ()
                                (rotate 90)
                                (moveto 7 -2)
                                (setfont *annotation-font*)
                                (show "page refs"))))
    ;;

    (let ((r (alloc-points 50)))
      (rectstroke-middash-v r)
      (setfont *word-font*)
      (cshow (center-x r)
             (center-y r)
             "page contents"))))
