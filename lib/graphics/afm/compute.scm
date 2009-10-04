
(define-class <scaled-metrics> (<object>)
  (_properties init-value: #f)
  afm
  font-size)

(define-method properties ((self <scaled-metrics>))
  (or (_properties self)
      (let (((p <vector>) (clone (properties (afm self))))
            (scale (/ (font-size self) 1000)))
        (set-_properties! self p)
        (vector-for-each
         (lambda (needs-scaling-key)
           (let ((j (vassq needs-scaling-key p)))
             (if j
                 (vector-set! p j (* (vector-ref p j) scale)))))
         '#(Ascender 
            Descender
            UnderlineThickness
            CapHeight
            XHeight))
        p)))

(define-class <scaled-char-metrics> (<object>)
  char-metrics
  font-size)

(define-method font-glyph-names ((self <afm>))
  (key-sequence (char-metrics self)))

(define-method font-characters ((self <afm>))
  (key-sequence (code-metrics self)))

(define-method font-characters ((self <scaled-metrics>))
  (key-sequence (code-metrics (afm self))))

(define-method font-glyph-names ((self <scaled-metrics>))
  (key-sequence (char-metrics (afm self))))

(define-method make-scaled-metrics ((self <afm>) size)
  (make <scaled-metrics>
        afm: self
        font-size: size))

(define-method make-scaled-metrics ((self <scaled-metrics>) size)
  (make <scaled-metrics>
        afm: (afm self)
        font-size: (* size (font-size self))))

;;;

;;;
;;;  `seq' is either a <string> (i.e., a sequence of characters)
;;;               or a list of characters or symbols
;;;

(define-method get-char-metrics ((afm <afm>) seq)
  (if (string? seq)
      (let ((cm (code-metrics afm)))
        (map (lambda (ch)
               (table-lookup cm ch))
             (string->list seq)))
      (let ((com (code-metrics afm))
            (chm (char-metrics afm)))
        (map (lambda (ch)
               (if (char? ch)
                   (table-lookup com ch)
                   (table-lookup chm ch)))
             seq))))

(define-method get-char-metrics ((self <scaled-metrics>) seq)
  (map (lambda (cm)
         (make <scaled-char-metrics>
               char-metrics: cm
               font-size: (font-size self)))
       (get-char-metrics (afm self) seq)))

(define-method string-x-deltas ((self <scaled-metrics>) seq)
  (let ((scale (font-size self)))
    (map (lambda (dx)
           (if (eq? dx 0)
               0
               (* dx scale)))
         (string-x-deltas (afm self) seq))))

;;;  Returns the list of x shifts due to kern adjustments

(define-method string-x-deltas ((afm <afm>) seq)
  ;;
  (let ((cm-list (get-char-metrics afm seq)))
    (cond
     ((null? cm-list)
      '())
     ((null? (cdr cm-list))
      '())
     (else
      (let loop ((r '())
                 (prev (car cm-list))
                 (cm (cdr cm-list)))
        (if (null? cm)
            (reverse! r)
            (let ((ki (vassq (car cm) (kern-follows prev))))
              (loop (cons
                     (if ki
                         (/ (vector-ref (kern-follows prev) ki) 1000)
                         0)
                     r)
                    (car cm)
                    (cdr cm)))))))))

(define-method kerning-pairs-after ((self <scaled-metrics>) (first <char>))
  (kerning-pairs-after (afm self) 
                       first
                       (font-size self)))

(define-method kerning-pairs-after ((self <afm>) (first <char>) 
                                    #optional (scale default: 1))
  (let* (((cm <char-metrics>) (car (get-char-metrics self (list first))))
         ((kf <vector>) (kern-follows cm))
         (sf (/ scale 1000)))
    ;;
    (define (char-metric->char cm)
      (integer->char (code cm)))
    ;;
    (let loop ((i (vector-length kf))
               (r '()))
      (if (= i 0)
          r
          (loop (- i 2)
                (cons (cons (char-metric->char (vector-ref kf (- i 2)))
                            (* sf (vector-ref kf (- i 1))))
                      r))))))
  
  
(define-method char-widths ((self <scaled-metrics>) seq)
  (let ((s (/ (font-size self) 1000)))
    (map (lambda (cm)
           (and cm (* s (x-width cm))))
         (get-char-metrics (afm self) seq))))
  
(define-method char-widths ((afm <afm>) (str <string>))
  (map (lambda (cm)
         (and cm (/ (x-width cm) 1000)))
       (get-char-metrics afm str)))

(define-method string-width ((afm <afm>) (font-size <real>) (str <string>))
  (* (/ font-size 1000)
     (reduce + 0 (map x-width (get-char-metrics afm str)))))

(define-method string-width ((self <scaled-metrics>) (str <string>))
  (string-width (afm self) (font-size self) str))
 
;; `xshow-x-list' returns a list of the x shifts to use in
;; the `xshow' postscript operator to implement kerning.

(define-method xshow-x-list ((afm <afm>) (font-size <real>) (str <string>))
  (let ((fscale (/ font-size 1000)))
    (map (lambda (cm dx)
           (+ (* (x-width cm) fscale)
              (* dx font-size)))
         (get-char-metrics afm str)
         (append (string-x-deltas afm str) '(0)))))

;;;

(define-method string-bbox ((self <scaled-metrics>) str)
  (string-bbox (afm self) (font-size self) str))

(define-method string-bbox ((afm <afm>) (font-size <real>) (str <string>))
  (let ((factor (/ font-size 1000.0)))
    (let loop ((m (get-char-metrics afm str))
               (x 0)
               (r #f))
      (if (null? m)
          (or r $zero-rect)
          (loop (cdr m)
                (+ x (* factor (x-width (car m))))
                (let ((b (offset-rect 
                          (scale (bbox (car m)) factor)
                          x 0)))
                  (if r 
                      (union-rect r b)
                      b)))))))

;;;

(define-method char-code-range ((afm <afm>))
  (values (char-metrics-min afm)
          (char-metrics-max afm)))

(define (char-metrics-min (self <afm>))
  (let ((cm (code-metrics self)))
    (let loop (((i <fixnum>) 0))
      (if (table-lookup cm (integer->char i))
          i
          (loop (add1 i))))))

(define (char-metrics-max (self <afm>))
  (let ((cm (code-metrics self)))
    (let loop (((i <fixnum>) 255))
      (if (table-lookup cm (integer->char i))
          i
          (loop (sub1 i))))))

;;;

(define-method char-width ((self <char-metrics>))
  (x-width self))

(define-method char-height ((self <char-metrics>))
  (limit-y (bbox self)))

(define-method char-depth ((self <char-metrics>))
  (- (min 0 (origin-y (bbox self)))))

(define-method char-bbox ((self <char-metrics>))
  (bbox self))

;;;

(define-method char-width ((self <scaled-char-metrics>))
  (* (/ (font-size self) 1000) (char-width (char-metrics self))))

(define-method char-height ((self <scaled-char-metrics>))
  (* (/ (font-size self) 1000) (char-height (char-metrics self))))

(define-method char-depth ((self <scaled-char-metrics>))
  (* (/ (font-size self) 1000) (char-depth (char-metrics self))))

(define-method char-bbox ((self <scaled-char-metrics>))
  (let ((s (/ (font-size self) 1000))
        (cm (char-metrics self)))
    (make-rect (* s (origin-x (bbox cm)))
               (* s (origin-y (bbox cm)))
               (* s (size-width (bbox cm)))
               (* s (size-height (bbox cm))))))
