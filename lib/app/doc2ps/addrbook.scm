;;;
;;;   Address book sheet layout
;;;
;;;   two-sided folding, small pages, 
;;;   two input ABOOK sheets per ouput LETTER sheet
;;;   the outer page on a separate output sheet (i.e., card stock)

(define-class <super-page> (<object>)
  (side type: <symbol>)
  (page-label type: <string>)
  (constituents type: <vector>))


(define (address-book-sheet-layout pages)
  (let* ((n (length pages))
         (m (quotient (+ n 3) 4))
         (v (list->vector (map (lambda (i)
                                 (vector #f #f #f #f i))
                               (range m))))
         (half (* m 2)))
    ;;
    (for-each 
     (lambda (i p)
       (bind ((sheet side sub (if (< i half)
                                  (values (quotient i 2) 
                                          (modulo i 2)
                                          'bottom)
                                  (values (+ m (quotient (- half i) 2) -1)
                                          (- 1 (modulo i 2))
                                          'top)))
              (k (+ (* side 2) (if (eq? sub 'top) 1 0))))
         (format #t "input page [~d] on sheet [~d.~a] ~a\n" i sheet side sub)
         (vector-set! (vector-ref v sheet) k p)))
     (range n)
     pages)
    ;;
    (apply 
     append
     (map (lambda (i v)
            (define (pl j)
              (if (vector-ref v j)
                  (list (page-label (vector-ref v j)))
                  '()))
            ;;
            (list
             (make <super-page>
                   side: 'front
                   page-label: (string-join "," (append (pl 0) (pl 1)))
                   constituents: (subvector v 0 2))
             (make <super-page>
                   side: 'back
                   page-label: (string-join "," (append (pl 2) (pl 3)))
                   constituents: (subvector v 2 4))))
          (range m)
          (vector->list v)))))

(define (transform-to-subpage dev side sp w h sw sh)
  (case side
    ((front)
     (translate dev (make-point (/ (- sw h) 2) 
                                (+ (/ sh 2) (* sp w))))
     (rotate dev -90))
    ((back)
     (translate dev (make-point (/ (+ sw h) 2) 
                                (+ (/ sh 2) (* (- sp 1) w))))
     (rotate dev 90))))

(define-method fake-print-page ((self <super-page>) dev)
  ;;
  (define (subpage page)
    (bind ((ps (style p))
           (page-group (get-page-style ps))
           (o w h (get-style-attributes/m ps 
                                          '(orientation
                                            page-width
                                            page-height))))
      ;;
      (if (eq? o 'landscape)
          (begin
            (rotate dev 90)
            (translate dev (make-point 0 (- h)))))
      ;;
      (rectstroke dev (make-rect 0 0 w h))))
  ;;
  (let ((w (* 3.25 72))
        (h (* 6 72))
        (sw (* 8.5 72))
        (sh (* 11 72)))
    (for-each
     (lambda (j)
       (if (vector-ref (constituents self) j)
           (with-gstate-saved
            dev
            (lambda ()
              (transform-to-subpage dev (side self) j w h sw sh)
              (subpage (vector-ref (constituents self) j))))))
     '(0 1))
    (values)))


(set! *pre-prp-hook*
      (lambda ()
        (set! *all-pages* (address-book-sheet-layout *all-pages*))))


;;;

(define-style 'address-book
  (page-style basis: 'letter
              page-width: 3.25in
              page-height: 6in))

(define-style 'address-book-body
  (page-style basis: 'address-book
              numbering-group: 'body
              numbering-format: 'arabic))

(define-style 'abook-cover-page
  (page-style basis: 'address-book
              content: (list
                        ...)
              numbering-format: 'roman
              numbering-group: 'preface
              page-start: 'recto))

(define $page-box-side 12)

(define (verso-page-num)
  (list
   (make <text-box>
         frame: (make-rect 6 6 $page-box-side $page-box-side)
         content: '((page-number))
         style: 'pagenum-char-style
         align: 'left)
   (make <script-graphic>
         frame: (make-rect 6 6 (- 6in 12) $page-box-side)
         script: (lambda (dev (f <rect>))
                   (setcolor dev (device-color dev '(gray 0.666)))
                   (rectfill dev (make-rect (origin-x f)
                                            (origin-y f)
                                            (size-width f)
                                            3))
                   (rectfill dev (make-rect (origin-x f)
                                            (origin-y f)
                                            $page-box-side
                                            $page-box-side))))))

(define (recto-page-num)
  (list
   (make <text-box>
         frame: (make-rect (- 6in 6 $page-box-side)
                           6
                           $page-box-side $page-box-side)
         content: '((page-number))
         style: 'pagenum-char-style
         align: 'right)
   (make <script-graphic>
         frame: (make-rect 6 6 (- 6in 12) $page-box-side)
         script: (lambda (dev (f <rect>))
                   (setcolor dev (device-color dev '(gray 0.666)))
                   (rectfill dev (make-rect (origin-x f)
                                            (origin-y f)
                                            (size-width f)
                                            3))
                   (rectfill dev (make-rect (- (limit-x f) $page-box-side)
                                            (origin-y f)
                                            $page-box-side
                                            $page-box-side))))))

(define-style 'abook-blank-page
  (page-start
   basis: 'address-book-body
   content: (list (make <text-box>
                        frame: (make-rect 0 2.8in 6in 0.5in)
                        content: '("(This Page Intentionally Left Blank)")
                        align: 'center
                        style: 'emphasis-char-style)
                  (make <text-box>
                        frame: (make-rect 0.75in 0.5in 0.75in 0.25in)
                        content: '((page-number))
                        style: 'emphasis-char-style
                        align: 'left)
                  (make <text-box>
                        frame: (make-rect 4.5in 0.5in 3in 0.25in)
                        content: '((ref version))
                        style: 'emphasis-char-style
                        align: 'right))))
              
(define-style 'abook-body-page
  

