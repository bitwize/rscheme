
(define (get-font-pdf-data (self <font-entry>))
  (or (get-property self 'pdf-data #f)
      (let ((pdf (load-pdf-data self)))
        (set-property! self 'pdf-data pdf)
        pdf)))

(define *eexec-pattern* (reg-expr->proc '(seq "eexec" (+ (or #\cr #\lf)))))

(define (type-1-skip-trailing-zeros str)
  (let loop ((k (- (string-length str) 1))
             (n 0))
    (if (= n 512)
        (+ k 1)
        (if (char=? (string-ref str k) #\0)
            (loop (- k 1) (+ n 1))
            (loop (- k 1) n)))))
        
(define (scan-pfa-font pfa)
  (bind ((str pfa)
         (s e (*eexec-pattern* str)))
    (if s
        (let* ((t (substring str e))
               (z (type-1-skip-trailing-zeros t)))
          (values str e (+ e z)))
        (error "~a: Could not parse PFA font file" pfa))))

(define (load-pdf-data (self <font-entry>))
  (bind ((data brk0 brk1 (scan-pfa-font (get-font-definition self)))
         (metrics (font-metrics self))
         (code-min code-max (char-code-range metrics))
         (widths (list->vector
                  (map (lambda (w)
                         (if w
                             (* w 1000)
                             0))
                       (char-widths
                        metrics
                        (list->string
                         (map 
                          integer->char
                          (list-tail (range (+ code-max 1)) code-min)))))))
         (bbox (get-property metrics 'FontBBox)))
    ;;
    `(pdf family-name: ,(family-name self)
          style-name: ,(member-name self)
          base-font: ,(get-property metrics 'FontName)
          first-char: ,code-min
          last-char: ,code-max
          char-widths: ,widths
          length1: ,brk0
          length2: ,(- brk1 brk0)
          length3: ,(- (string-length data) brk1)
          properties: ,(vector
                        'Flags 0          ; XXX improve this...
                        'FontBBox (vector (origin-x bbox)
                                          (origin-y bbox)
                                          (limit-x bbox)
                                          (limit-y bbox))
                        'ItalicAngle (get-property metrics 'ItalicAngle 0)
                        'Ascent (get-property metrics 'Ascender)
                        'Descent (get-property metrics 'Descender)
                        'CapHeight (get-property metrics 'CapHeight)
                        'StemV (get-property metrics 'UnderlineThickness)
                        'XHeight (get-property metrics 'XHeight))
          content: ,data)))
