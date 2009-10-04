;;;
;;;  Show off a font map
;;;
;;;  (similar to the encoding vector appendices of Adobe PostScript books)

,(use graphics.fontmgr
      graphics.afm
      graphics.device
      graphics.geometry)

(define (char-box dev (i <rect>))
  (with-gstate-saved
    dev
    (lambda ()
      (setlinewidth dev 0.25)
      (setlinecap dev 'round)
      (setdash dev '#(0 1) 0)
      ;;
      (let ((o (inset-rect i -3 -2)))
        (moveto dev (make-point (origin-x o) (origin-y i)))
        (lineto dev (make-point (limit-x o) (origin-y i)))
        
        (moveto dev (make-point (origin-x o) (limit-y i)))
        (lineto dev (make-point (limit-x o) (limit-y i)))
        
        (moveto dev (make-point (origin-x i) (origin-y o)))
        (lineto dev (make-point (origin-x i) (limit-y o)))
        
        (moveto dev (make-point (limit-x i) (origin-y o)))
        (lineto dev (make-point (limit-x i) (limit-y o)))
        (stroke dev)))))

(define (show-font-glyph dev font ch)
  (areafill 
   dev
   (transform
    (get-outline font ch)
    (scale (translate $identity-transform
                      (make-point 25 5))
           (/ 12 1000))))
  (let ((dx (car (char-widths (font-metrics (get-text-font font 12))
                              (list ch)))))
    (if dx
        (char-box dev (make-rect 25 5 dx 12))))
  ;;
  (moveto dev (make-point 1 2))
  (show dev (substring (format #f "~s" ch) 2)))

(define (show-font-map (dev <graphics-device>) (font <font-entry>))
  (startpage dev)
  (with-gstate-saved
   dev
   (lambda ()
     (let ((cell-w (/ (* 7 72) (+ 8 1)))
           (cell-h (/ (* 9 72) (+ 32 1)))
           (gray (device-color dev '(gray 0.667))))
       (define (base x y)
         (make-point (* x cell-w) (* (- y) cell-h)))
       ;;
       (translate dev (make-point (+ 36 cell-w) (+ 36 (* 32 cell-h))))
       ;;
       (setfont dev (get-text-font "Helvetica" "Regular" 6))
       (for-each
        (lambda (y)
          (for-each
           (lambda (x)
             (with-gstate-saved
              dev
              (lambda ()
                (translate dev (base x y))
                (handler-case
                 (show-font-glyph dev font (integer->char (+ (* y 8) x)))
                 ((<condition>)
                  (setcolor dev gray)
                  (rectfill dev (make-rect 0 0 cell-w cell-h)))))))
           (range 8)))
        (range 32))
       ;;
       (setlinewidth dev 0.25)
       ;;
       (setfont dev (get-text-font "Times" "Italic" 12))
       ;;
       (for-each (lambda (y)
                   (moveto dev (point+ (base -1 y) (make-size 15 5)))
                   (show dev (format #f "\\~02ox" y)))
                 (range 32))
       (for-each (lambda (y)
                   (moveto dev (base -1 y))
                   (lineto dev (base 8 y))
                   (stroke dev))
                 (cons -2 (cons -1 (range 32))))
       ;;
       (for-each (lambda (x)
                   (moveto dev (point+ (base x -1) (make-size 40 5)))
                   (show dev (to-string x)))
                 (range 8))
       (for-each (lambda (x)
                   (moveto dev (base x -2))
                   (lineto dev (base x 31))
                   (stroke dev))
                 (cons -1 (range 9)))
       ;;
       (setfont dev (get-text-font "Helvetica" "Bold" 9))
       (moveto dev (point+ (base -1 -2) (make-size 5 17)))
       (show dev (font-family font))
       (moveto dev (point+ (base -1 -2) (make-size 5 5)))
       (show dev (font-style font))
       (show dev "  (12pt)")
       )))
  (endpage dev))
       
(define (do-font-map psfile . query-opts)
  (let ((dev (open-ps-device psfile)))
    (show-font-map dev (car (apply query-font-database query-opts)))
    (close-graphics-device dev)))
  
(define (edw) (do-font-map "/tmp/edw.ps"
                           family: "Edwardian"))

(define (dingbats) (do-font-map "/tmp/dingbat.ps" family: "ZapfDingbats"))
(define (times) (do-font-map "/tmp/times.ps" postscript: "Times-Roman"))
(define (symbol) (do-font-map "/tmp/symbol.ps" family: "Symbol"))
(define (minion) (do-font-map "/tmp/minion.ps" 
                              family: "Minion"
                              weight: 'bold
                              angle: 'normal
                              width: 'condensed))
(define (univers) (do-font-map "/tmp/univers.ps" 
                               family: "Univers"
                               weight: 'normal
                               angle: 'normal
                               width: 'ultra-condensed))
