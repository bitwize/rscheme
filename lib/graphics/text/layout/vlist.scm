

(define-class <vlist-packed-line> (<vlist-node>)
  (content type: <packed-line>)
  baseline              ; where in the vbox the baseline lives
  height)               ; this is the height of the vbox

(define-class <vskip> (<vlist-node>)
  (content type: <glue>))

(define (make-vskip (v <glue>))
  (make <vskip>
        content: v))

(define-method vitem-space ((self <vlist-packed-line>))
  (make <space>
        natural: (height self)))

(define-method vitem-space ((self <vskip>))
  (glue->space (content self)))

;;;

(define (vlist-space vlist)
  (let ((total-h (make <space>)))
    (for-each
     (lambda (vitem)
       (space+! total-h (vitem-space vitem)))
     vlist)
    total-h))
    
(define (vlist-set vlist height)
  (let ((total-h (vlist-space vlist)))
    (set-glue (pair? vlist)
              total-h
              (- (qty->scaled height)
                 (natural total-h)))))

;;;

(define (render-vlist dev (origin <point>) vlist (setting <glue-set>))
  (let ((x0 (qty->scaled (x origin))))
    (let loop ((y (qty->scaled (y origin)))
               (lst vlist))
      (if (null? lst)
          (make-point (x origin) (scaled->qty y))
          (let ((i (car lst)))
            (cond
             ((instance? i <vskip>)
              (let ((dy (set-size (content i) setting)))
                ;;
                #|
                (with-gstate-saved
                 dev
                 (lambda ()
                   (let ((x (scaled->qty x0))
                         (y (scaled->qty y))
                         (dy (scaled->qty dy)))
                     (moveto dev (make-point (- x 5) y))
                     (lineto dev (make-point x y))
                     (lineto dev (make-point (+ x 5) (- y dy)))
                     (lineto dev (make-point (+ x 10) (- y dy)))
                     (stroke dev))))
                |#
                ;;
                (loop (- y dy) (cdr lst))))
             ((instance? i <vlist-packed-line>)
              (let (((p <packed-line>) (content i)))
                (show-packed-line dev x0 (- y (baseline i)) p)
                (loop (- y (height i))
                      (cdr lst))))))))))

#|

(with-line-width
 (size-width frame)
 (lambda ()
   (line-break 
    (vector->list (text->hlist (sxml:children item)
                               style)))))))

;;;

    

        
(define-font-style default-font ()
   family: "Helvetica"
   size: 12
   angle: 'oblique
   weight: 'normal
   width: 'normal
   variation: 'none
   stretch: 0)
|#

;;;  Does line breaking on some input text and
;;;  inserts the result into the given vlist.  Note
;;;  that the input is processed via 'text->hlist', which
;;;  interprets some SXML structures

(define (break-text-into-vlist (vlist <list>) text (style <text-style>))
  (let* ((usual-fm (font-metrics (font style)))
         (line-height (* 14/12 (qty->scaled (font-size (font style)))))
         (ascent (qty->scaled (get-property usual-fm 'Ascender)))
         (descent (qty->scaled (- (get-property usual-fm 'Descender))))
         (leading (make <glue>
                        natural: (- line-height ascent descent)))
         (q (make-dequeue)))
    ;;
    (vector-for-each
     (lambda ((p <packed-line>))
       (let ((h (max (height p) ascent))
             (d (max (depth p) descent)))
         ;;
         (if (not (dequeue-empty? q))
             (dequeue-push-back! q (make <vskip> content: leading)))
         ;;
         (dequeue-push-back! 
          q
          (make <vlist-packed-line>
                height: (+ h d)
                baseline: h
                content: p))
         (values)))
     (line-break
      (vector->list (text->hlist text style))))
    ;;
    (append vlist (vector->list (dequeue-state q)))))
   


#|

(with-line-width
 200
 (lambda ()
   (break-text-into-vlist
    '()
    '("Now is the time for our discontent, "
      "be it with friends, or with the enemies who are "
      "all of ours together.")
    (make-text-style))))

(with-line-width
 200
 (lambda ()
   (line-break
    (vector->list
     (text->hlist '("Now is the time for our discontent, "
                    "be it with friends, or with the enemies who are "
                    "all of ours together.")
                  (make-text-style))))))
 


|#
