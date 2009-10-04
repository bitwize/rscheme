(define (compute-break-width ctx l type)
  (if (or (eq? type 'unhyphenated) (null? l))
      (compute-break-width* l (background ctx))
      (compute-break-width-discretionary l (background ctx))))

(define (compute-break-width-discretionary s bg)
  ;; TeX#840 et.al.
  (bind (((disc <disc-node>) (car s))
         (s l (replacement-width (cdr s) (replace-count disc)))
         (nil l1 (list-width (post-break disc)))
         (nil l0 (list-width (pre-break disc))))
    (values (space+ bg (- (+ l0 l1) l)) s)))

(define (list-width s)
  (replacement-width s (length s)))

(define (replacement-width s n)
  (let loop ((n n)
             (s s)
             (w 0))
    (if (> n 0)
        (loop (- n 1) 
              (cdr s)
              (+ w (width (car s))))
        (values s w))))

(define (compute-break-width* l bw)
  (if (null? l)
      (values bw '())
      (dispatch-break-width (car l) bw (cdr l))))

(define-method dispatch-break-width ((self <data-node>) bw rest)
  (values bw (cons self rest)))

(define-method dispatch-break-width ((self <glue-node>) bw rest)
  (if (null? rest)
      (error "XXX dispatch-break-width on <glue-node>: nothing left! (Perhaps excess trailing space in input?)"))
  (compute-break-width* (cdr rest) (space- bw (content self))))

(define-method dispatch-break-width ((self <penalty-node>) bw rest)
  (compute-break-width* (cdr rest) bw))

(define-method dispatch-break-width ((self <math-node>) bw rest)
  (compute-break-width* (cdr rest) (space- bw (width self))))

(define-method dispatch-break-width ((self <kern-node>) bw rest)
  (compute-break-width* (cdr rest) (space- bw (width self))))

(define-method dispatch-break-width ((self <accent-kern-node>) bw rest)
  ;; Kerning nodes for accents are treated specially; they do not
  ;; disappear at a line break.
  (compute-break-width* (cdr rest) bw))
