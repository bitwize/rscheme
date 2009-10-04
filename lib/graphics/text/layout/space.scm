(define-class <space> (<object>)                     ; [index in TeX's array]
  (natural type: <fixnum> init-value: 0)             ; [1] TeX#866: act_width
  (pt-stretch type: <fixnum> init-value: 0)          ; [2]
  (fil-stretch type: <fixnum> init-value: 0)         ; [3]
  (fill-stretch type: <fixnum> init-value: 0)        ; [4]
  (filll-stretch type: <fixnum> init-value: 0)       ; [5]
  (pt-shrink type: <fixnum> init-value: 0))          ; [6]

(define-mm-generic-function space+)
(define-mm-generic-function space-)

(define (glue->space (self <glue>))
  (if (> (shrink-order self) 0)
      (error "Glue ~s contains infinite shrink, which is not allowed here" self))
  ;;
  (let (((s <space>) (make <space>
                           natural: (natural self)
                           pt-shrink: (shrink self)))
        ((x <fixnum>) (stretch self)))
    (case (stretch-order self)
      ((0) (set-pt-stretch! s x))
      ((1) (set-fil-stretch! s x))
      ((2) (set-fill-stretch! s x))
      ((3) (set-filll-stretch! s x)))
    s))
  
(define (space-shrink (self <space>))
  ;; this is trivial because we don't allow infinite shrinkability in <space>
  ;;  XXX but look at TeX#665.  It allows for infinite shrinkability; could it
  ;; be coming from somewhere other than the paragraph?
  (values 0 (pt-shrink self)))

(define (space-stretch (self <space>))
  (cond
   ((not (zero? (filll-stretch self)))
    (values 3 (filll-stretch self)))
   ((not (zero? (fill-stretch self)))
    (values 2 (fill-stretch self)))
   ((not (zero? (fil-stretch self)))
    (values 1 (fil-stretch self)))
   (else
    (values 0 (pt-stretch self)))))

(define-method space+ ((self <space>) (b <glue>))
  (if (> (shrink-order b) 0)
      (error "Glue ~s contains infinite shrink, which is not allowed here" b))
  ;;
  (let (((c <space>) (clone self)))
    (set-natural! c (+ (natural c) (natural b)))
    (set-pt-shrink! c (+ (pt-shrink c) (shrink b)))
    (case (stretch-order b)
      ((0) (set-pt-stretch! c (+ (pt-stretch c) (stretch b))))
      ((1) (set-fil-stretch! c (+ (fil-stretch c) (stretch b))))
      ((2) (set-fill-stretch! c (+ (fill-stretch c) (stretch b))))
      ((3) (set-filll-stretch! c (+ (filll-stretch c) (stretch b)))))
    c))

(define-method space+ ((self <space>) (b <fixnum>))
  (let (((c <space>) (clone self)))
    (set-natural! c (+ (natural c) b))
    c))

(define-method space- ((self <space>) (b <fixnum>))
  (let (((c <space>) (clone self)))
    (set-natural! c (- (natural c) b))
    c))

(define-method space- ((self <space>) (b <glue>))
  (if (> (shrink-order b) 0)
      (error "Glue ~s contains infinite shrink, which is not allowed here" b))
  ;;
  (let (((c <space>) (clone self)))
    (set-natural! c (- (natural c) (natural b)))
    (set-pt-shrink! c (- (pt-shrink c) (shrink b)))
    (case (stretch-order b)
      ((0) (set-pt-stretch! c (- (pt-stretch c) (stretch b))))
      ((1) (set-fil-stretch! c (- (fil-stretch c) (stretch b))))
      ((2) (set-fill-stretch! c (- (fill-stretch c) (stretch b))))
      ((3) (set-filll-stretch! c (- (filll-stretch c) (stretch b)))))
    c))

(define (space+natural! (a <space>) (b <fixnum>))
  (set-natural! a (+ (natural a) b))
  a)

(define (space+! (a <space>) (b <space>))
  (set-natural! a (+ (natural a) (natural b)))
  (set-pt-shrink! a (+ (pt-shrink a) (pt-shrink b)))
  (set-pt-stretch! a (+ (pt-stretch a) (pt-stretch b)))
  (set-fil-stretch! a (+ (fil-stretch a) (fil-stretch b)))
  (set-fill-stretch! a (+ (fill-stretch a) (fill-stretch b)))
  (set-filll-stretch! a (+ (filll-stretch a) (filll-stretch b)))
  a)

(define (space-! (a <space>) (b <space>))
  (set-natural! a (- (natural a) (natural b)))
  (set-pt-shrink! a (- (pt-shrink a) (pt-shrink b)))
  (set-pt-stretch! a (- (pt-stretch a) (pt-stretch b)))
  (set-fil-stretch! a (- (fil-stretch a) (fil-stretch b)))
  (set-fill-stretch! a (- (fill-stretch a) (fill-stretch b)))
  (set-filll-stretch! a (- (filll-stretch a) (filll-stretch b)))
  a)

(define-method space+ ((a <space>) (b <space>))
  (space+! (clone a) b))

(define-method space- ((a <space>) (b <space>))
  (space-! (clone a) b))

(define-method write-object ((self <space>) port)
  (format port "#[<space> ~d" (natural self))
  ;;
  (if (or (not (zero? (pt-stretch self)))
          (not (zero? (fil-stretch self)))
          (not (zero? (fill-stretch self)))
          (not (zero? (filll-stretch self))))
      (begin
        (format port " +")
        (if (not (zero? (pt-stretch self)))
            (format port " ~d" (pt-stretch self)))
        (if (not (zero? (fil-stretch self)))
            (format port " ~dfil" (fil-stretch self)))
        (if (not (zero? (fill-stretch self)))
            (format port " ~dfill" (fill-stretch self)))
        (if (not (zero? (filll-stretch self)))
            (format port " ~dfilll" (filll-stretch self)))))
  ;;
  (if (not (zero? (pt-shrink self)))
      (format port " - ~d" (pt-shrink self)))
  (format port "]"))
