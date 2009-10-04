
(define-method x-extent ((self <hlist>))
  (values (x self) (width self)))

(define-method x-extent ((self <hline>))
  (let loop ((xmin 0)
             (xmax 0)
             (lst (content self)))
    (if (null? lst)
        (values xmin (- xmax xmin))
        (bind ((x w (x-extent (car lst))))
          (loop (min x xmin)
                (max xmax (+ x w))
                (cdr lst))))))

(define-method bbox ((self <hline>))
  (bind ((x w (x-extent self)))
    (make-rect x (- (height self)) w (height self))))

(define-method bbox ((self <vlist>))
  (let* ((r (subframe-rect (placement self)))
         (x0 (origin-x r)))
    (let loop ((b #f)
               (l (content self))
               (y (limit-y r)))
      (if (null? l)
          (or b $zero-rect)
          (let* ((n (bbox (car l)))
                 (n2 (offset-rect n x0 y)))
            (format #t "vlist entry ~s bbox => ~s\n" (car l) n2)
            (loop (if b (union-rect b n2) n2)
                  (cdr l)
                  (- y (height n))))))))

;;; this is just for testing, so the page owner can be just the string
;;; when we generate a test Part Page (see `test-part-page' below)

(define-method page-owner->part-label ((self <string>))
  self)

(define-method page-owner->part-label ((self <toc-node>))
  (format-number (car (label self)) 'Roman))

(define (render-part-label-graphic dev frame #key char-style)
  ;;
  ;;  We are going to introspect on the page we're on to find out
  ;;  where the flow layout ends; that allows the title block
  ;;  to flow into multiple lines, and we can still draw our
  ;;  thick line underneath it.
  ;;
  (let* (((fl <flow-layout>) (car (content (current-page dev))))
        ((vl <vlist>) (car (content fl)))
        (bb (bbox vl))
        (nm (page-owner->part-label (current-page-owner dev)))
        ((tf <text-font>) (query-style (query-style char-style 'font) 'font))
        (r (string-bbox (font-metrics tf) nm))
        (x (+ (origin-x frame) 12))
        (y (+ (origin-y frame) 18))
        (h (+ (size-height r) 36))
        (w (+ (width r) 24)))
    ;;  Draw the background rectangle
    (setcolor dev (device-color dev '(gray 0.5)))
    (rectfill dev (make-rect (origin-x frame) (origin-y frame) w h))
    ;;  Draw the text inside
    (moveto dev (make-point x y))
    ((style-compile char-style) dev nm)
    ;;
    ;(setcolor dev '(rgb 0 0 1)) (rectstroke dev bb)
    ;;
    (setlinewidth dev 3)
    (setcolor dev (device-color dev 'black))
    (let ((y (- (origin-y bb) 18)))
      (moveto dev (make-point (origin-x frame) (+ (origin-y frame) h)))
      (lineto dev (make-point 1in y))
      (lineto dev (make-point 7.75in y))
      (stroke dev))))
#|
                        (make <line-graphic>
                              line-start: (make-point 1in 9in)
                              line-end: (make-point 7.75in 9in)
                              line-style: 'heavy-stroke))
|#

;(car (current-page-owner dev))))

(add-script-renderer! 'part-label-graphic render-part-label-graphic)
  
;;;

#|
(define (test-part-page)
  ;(set! *show-outlines* #t)
  (reset-pages)
  ;;
  (layout-flow-in-page-sequence
   (make <flow>
         flow-tag: 'a
         content: (list
                   (make <para>
                         style: 'part-title-para-style
                         content: (list
                                   (make <text-run>
                                         style: 'part-title-char-style
                                         content: "Technical Guide")))))
   (make <simple-page-sequence>
         initial-page-styles: '()
         blank-page-style: #f
         repeat-page-styles: '(part-first-page))
   '"IV")
  ;;
  ;(make-page ps 'part-first-page #f)
  (tgsp 0))
|#
