
(define (render-text-layout dev #key usual-font lines frame)
  (let* ((usual-fm (font-metrics usual-font))
         (line-height (* 14/12 (qty->scaled (font-size usual-font))))
         (ascent (qty->scaled (get-property usual-fm 'Ascender)))
         (descent (qty->scaled (- (get-property usual-fm 'Descender))))
         (leading (- line-height ascent descent)))
    ;;
    #|
    (format #t "line-height ~d  ascent ~d  descent ~d  leading ~d\n"
            line-height
            ascent
            descent
            leading)
    |#
    ;;
    (vector-for-each
     (lambda (p)
       (set-height! p (max (height p) ascent))
       (set-depth! p (max (depth p) descent)))
     lines)
    ;;
    (let ((h (scaled->qty
              (reduce + 
                      (* leading (- (vector-length lines) 1))
                      (map (lambda (p)
                             (+ (height p)
                                (depth p)))
                           (vector->list lines))))))
      (with-gstate-saved
       dev
       (lambda ()
         #|
         (setlinewidth dev 0.25)
         (rectstroke dev (make-rect (origin-x frame)
                                    (- (origin-y frame) h)
                                    (size-width frame)
                                    h))
         |#
         (show-packed-lines dev
                            (origin-x frame)
                            (origin-y frame) 
                            lines
                            (scaled->qty leading))))
      ;;
      (make-rect (origin-x frame)
                 (- (origin-y frame) h)
                 (size-width frame)
                 h))))
