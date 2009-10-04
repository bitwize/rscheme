,(use graphics.device
      graphics.geometry
      graphics.charpath
      graphics.color
      graphics.afm
      graphics.fontmgr
      graphics.styles)

(define (t1-ps)
  (let ((d (open-ps-device "/tmp/test1.ps")))
    (include-font d (font-shape (get-text-font "Minion" "Condensed" 12)))
    (gen1 d #t)
    (close-graphics-device d)))

(define (t1-pdf)
  (let ((d (open-pdf-device "/tmp/test1.pdf")))
    (gen1 d #f)
    (close-graphics-device d)))

(define (gen1 d more-patterns?)
  (startpage d)
  ;;
  (for-each (lambda (c)
              (setcolor d (device-color d (make-color cyan: (/ c 10))))
              (moveto d (make-point (+ 10 c) 10))
              (lineto d (make-point (+ 30 c) 20))
              (stroke d)
              
              (setcolor d (device-color d (make-color cyan: (/ c 10)
                                                      black: 0.5)))
              (moveto d (make-point (+ 30 c) 20))
              (lineto d (make-point (+ 50 c) 30))
              (stroke d))
            
            (range 11))
  ;;
  (setcolor d (device-color d '(pattern redpolka)))
  (areafill d (make-rect 100 200 200 100))
  ;;
  (setcolor d (device-color d '(pattern nwshading (gray 0.5))))
  (areafill d (make-rect 100 300 200 100))
  ;;
  (setcolor d (device-color d '(pattern crosshatch (rgb 0 0 1))))
  (areafill d (make-rect 100 400 200 100))
  ;;
  (setcolor d (device-color d '(pattern neshading (gray 0.5))))
  (areafill d (make-rect 100 500 200 100))
  ;;
  (setcolor d (device-color d 'black))
  (areastroke d (make-rect 100 200 200 100))
  (areastroke d (make-rect 100 300 200 100))
  (areastroke d (make-rect 100 400 200 100))
  (areastroke d (make-rect 100 500 200 100))
  ;;
  (setcolor d (device-color d '(gray 0.667)))
  (areastroke d (make-rect 50 50 (* 333/1000 200) 200))

  (setcolor d (device-color d '(gray 0.333)))
  (areastroke d (transform (get-charpath "Times-Roman" "zcaron")
                           (scale
                            (translate $identity-transform
                                       (make-point 50 50))
                            200)))
  ;;
  (call-with-stylesheet
   (make-stylesheet)
   (lambda ()
     ;;
     (define-solid-paint-style black () color: 'black)
     (define-solid-paint-style white () color: 'white)
     ;;
     (define-solid-paint-style light-gray () color: '(gray 0.8))
     (define-pattern-paint-style black-nw () 
       color: 'black 
       pattern: 'nwshading)
     ;;
     (define-pattern-paint-style white-nw ()
       color: 'white 
       pattern: 'nwshading)
     ;;
     (define-pattern-paint-style white-wavy ()
       color: 'white 
       pattern: 'wavy1)
     (define-pattern-paint-style black-wavy ()
       color: 'black 
       pattern: 'wavy1)
     ;;
     (define-stroke-style white-stroke () paint: 'white linewidth: 1)
     (define-stroke-style white-thin-stroke (white-stroke) linewidth: 0.5)
     (define-stroke-style black-stroke () paint: 'black linewidth: 1)
     ;;
     (define-fill-style gray-fill () paint: 'light-gray)
     (define-fill-style black-fill () paint: 'black)
     ;;
     (areashow d (make-rect 310 182 290 290)
               stroke: 'black-stroke
               fill: (if more-patterns?
                         'black-wavy
                         'black-nw))
     (if more-patterns? ; XXX in PDF, this is coming out filled black :-(
         (areashow d (make-rect 300 72 300 100)
                   stroke: 'black-stroke
                   fill: 'gray-fill))
     ;;
     (let* ((f (get-text-font "Minion" "Condensed" 72))
            (fm (font-metrics f))
            (x 310))
       ;;
       (starttext d)
       (setfont d f)
       (moveto d (make-point 310 82))
       (show d "\252b\272, as")
       (endtext d)
       ;;
       (for-each
        (lambda (ch cw dx)
          (areashow 
           d 
           (transform (get-outline f ch)
                      (translate $identity-transform
                                 (make-point x 82)))
           stroke: 'white-thin-stroke
           fill: (if more-patterns?
                     'white-wavy
                     'white-nw))
          (set! x (+ x cw dx)))
        '(#\M-* #\b #\M-: #\, #\space #\a #\s)
        (char-widths fm '(#\M-* #\b #\M-: #\, #\space #\a #\s))
        (append (string-x-deltas fm '(#\M-* #\b #\M-: #\, #\space #\a #\s))
                '(0))))))
            
  ;;
  #|
  (starttext d)
  (setfont d (get-text-font "Times" "Roman" 12))
  (moveto d (make-point 300 100))
  (show d "Case 1")
  (endtext d)
  |#
  ;;
  (endpage d))

(define (t1-bbox-inner dev)
  (moveto dev (make-point 30 50))
  (lineto dev (make-point 35 70))
  (lineto dev (make-point 60 85))
  (setlinewidth dev 5)
  (stroke dev)
  ;;
  (moveto dev (make-point 50 60))
  (setfont dev (get-text-font "Times" "Bold" 16))
  (rotate dev 45)
  (show dev "Fox")
  ;;
  ;(lineto dev (make-point 80 60))
  ;(stroke dev)
  (values))

(define (t1-bbox)
  (let ((dev (open-bbox-device))
        (box #f))
    (t1-bbox-inner dev)
    (set! box (close-graphics-device dev))
    ;;
    (let ((p (open-ps-device "/tmp/bbox-test.ps")))
      (startpage p)
      (with-gstate-saved
       p
       (lambda ()
         (setcolor p (device-color p '(rgb 0.8 0.8 1)))
         (setlinewidth p 0.25)
         (rectfill p box)))
      (t1-bbox-inner p)
      (endpage p)
      (close-graphics-device p))))


(define (t)
  (t1-ps)
  (t1-pdf))
