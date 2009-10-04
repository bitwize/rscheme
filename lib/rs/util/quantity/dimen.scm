(define-module-extend iolib ()
  (for-each (lambda (k)
              (if (eq? (string-ref ($id-contd-switch) k) #\1)
                  (string-set! ($num-switch) k #\1)))
            (range 256)))

;;;

(define-class <unit> (<quantity>) :abstract
  (name type: <string>))

(define (unit<? (a <unit>) (b <unit>))
  (string<? (name a) (name b)))
  
(define-class <derived-unit> (<unit>)
  (dimen type: <vector>)                ; vector of base units
  (powers type: <vector>)               ; vector of powers
  (factor type: <real>))

(define-class <base-unit> (<unit>))

(define-method dimen ((self <base-unit>)) (vector self))
(define-method powers ((self <base-unit>)) '#(1))
(define-method factor ((self <base-unit>)) 1)

(define-class <nquantity> (<quantity>)
  (value type: <number>)
  (unit type: <unit>))

(define-method unit ((self <number>))
  $unitless)

(define-method value ((self <number>))
  self)

(if-implements (available rs.db.rstore)
  (with-module rs.db.rstore
    (define (rp n v)
      (let ((k (register-pivot! `(module rs.util.quantity ,n) v)))
        (format #t "Registered pivot[~d] ~s\n" k n)))
    ;;
    (rp '<nquantity> <nquantity>)
    (rp '<base-unit> <base-unit>)
    (rp '<derived-unit> <derived-unit>)))

,(use compiler)

(define-method self-evaluating? ((self <nquantity>)) #t)

(define parse-dimen (reg-expr->proc '(seq (save (seq (? #\-)
                                                     (+ (or #\/ digit #\.))))
                                          (* space)
                                          (save (seq alpha
                                                     (* (or alpha
                                                            #\^ 
                                                            #\/ 
                                                            digit)))))))

(define *units-table* (make-string-table))
(define $unitless (make <derived-unit>
                        name: ""
                        dimen: '#()
                        powers: '#()
                        factor: 1))

(table-insert! *units-table* "" $unitless)

(define-method quantity-unit ((self <number>)) $unitless)
(define-method quantity-value ((self <number>)) self)

(define-method quantity-unit ((self <nquantity>)) (unit self))
(define-method quantity-value ((self <nquantity>)) (value self))

(define (string->unit str)
  (table-lookup *units-table* str))

(define (string->quantity str #optional (base default: 10))
  (bind ((s e num un (parse-dimen str)))
    (if s
        (begin
          ;(format #t "~s => ~s ~s\n" str num un)
          (let ((r (string->number num base))
                (u (table-lookup *units-table* un)))
            (and r u (make <nquantity> value: r unit: u))))
        #f)))

(with-module mathlib
  (set! *number-parsers* (append! *number-parsers* (list string->quantity))))
                                  

(define-macro (define-unit name . equalsl)
  (let ((n (symbol->string name))
        (equals (if (pair? equalsl)
                    (car equalsl)
                    #f)))
    (let ((u (table-lookup *units-table* n))
          (dim (and equals (dimen (unit equals))))
          (pow (and equals (powers (unit equals))))
          (fac (and equals (* (value equals) (factor (unit equals))))))
      (cond
       ((not u)
        (table-insert! *units-table* n
                       (if equals
                           (make <derived-unit>
                                 name: n
                                 dimen: dim
                                 powers: pow
                                 factor: fac)
                           (make <base-unit>
                                 name: n))))
       ((and (instance? u <derived-unit>) equals)
        (set-dimen! u dim)
        (set-powers! u pow)
        (set-factor! u fac))
       ((and (instance? u <base-unit>) (not equals))
        ;; nothing to do
        )
       (equals
        (table-insert! *units-table* n
                       (make <derived-unit>
                             name: n
                             dimen: dim
                             powers: pow
                             factor: fac)))
       (else
        (table-insert! *units-table*
                       n
                       (make <base-unit>
                             name: n))))))
  (list 'quote name))

#|
(table-insert! *units-table* "pt" (make <base-unit> name: "pt"))

(define-unit in 72pt)
(define-unit mm 720/254pt)
(define-unit cm 10mm)
|#

(define-method write-object ((self <base-unit>) port)
  (format port "#[~a ~a]" (class-name (object-class self)) (name self)))

(define-method write-object ((self <derived-unit>) port)
  (format port "#[~a ~a = ~d" 
          (class-name (object-class self)) 
          (name self)
          (factor self))
  (vector-for-each
   (lambda (u p)
     (if (= p 1)
         (format port " ~a" (name u))
         (format port " ~a^~d" (name u) p)))
   (dimen self)
   (powers self))
  (format port "]"))

(define-method write-object ((self <nquantity>) port)
  (format port "~d~a" (value self) (name (unit self))))

(define-method display-object ((self <nquantity>) port)
  (format port "~d~a" (value self) (name (unit self))))

;;;

(define-method dimen-powers ((self <unit>))
  (map cons
       (vector->list (dimen self))
       (vector->list (powers self))))

(define (cancel-powers num den)
  (let loop ((num num)
             (den den)
             (r '()))
    (cond
     ((null? num)
      (append (reverse r) den))
     ((null? den)
      (append (reverse r) num))
     (else
      (let ((n (car num))
            (d (car den)))
        (if (eq? (car n) (car d))
            (loop (cdr num)
                  (cdr den)
                  (if (= (cdr n) (cdr d))
                      r                 ; they cancel completely
                      (cons (cons (car n) (- (cdr n) (cdr d))) r)))
            (if (unit<? (car n) (car d))
                (loop (cdr num) den (cons n r))
                (loop num (cdr den) (cons n r)))))))))
    
;;;

(define (unit/ (a <unit>) (b <unit>))
  (cond
   ((eq? a b)
    1)                                 ; short circuit common case
   ((eq? b $unitless)
    a)
   ((eq? a $unitless)
    (make <derived-unit>
          name: (string-append "1/" (name b))
          dimen: (dimen b)
          powers: (vector-map - (powers b))
          factor: (/ (factor b))))
   (else
    (let ((ratio (/ (factor a) (factor b)))
          (com (cancel-powers (dimen-powers a) (dimen-powers b))))
      (if (null? com)
          ratio
          (make <derived-unit>
                name: (string-append (name a) "/" (name b))
                dimen: (list->vector (map car com))
                powers: (list->vector (map cdr com))
                factor: ratio))))))

(define (unit* a b)
  (cond
   ((eq? b $unitless)
    a)
   ((eq? a $unitless)
    b)
   (else
    ;;
    (begin
      (define (invert l)
        (map (lambda (up)
               (cons (car up) (- (cdr up))))
             l))
      ;;
      (let ((cfactor (* (factor a) (factor b)))
            (com (cancel-powers (dimen-powers a) (invert (dimen-powers b)))))
        (if (null? com)
            cfactor
            (make <derived-unit>
                  name: (string-append (name a) "*" (name b))
                  dimen: (list->vector (map car com))
                  powers: (list->vector (map cdr com))
                  factor: cfactor)))))))

(define (quantity/ (a <quantity>) (b <quantity>))
  (let ((du (unit/ (unit a) (unit b)))
        (v (/ (value a) (value b))))
    (if (number? du)
        (* v du)
        (make <nquantity>
              value: v
              unit: du))))

(define (quantity* (a <quantity>) (b <quantity>))
  (let ((du (unit* (unit a) (unit b)))
        (v (* (value a) (value b))))
    (if (number? du)
        (* v du)
        (make <nquantity>
              value: v
              unit: du))))

#|      

(define (convert-dimen from-dimen #optional (to-units default: '(pt 1)))
  (* (cadr from-dimen) (/ (cadr (car from-dimen))
                          (cadr to-units))))
|#

(define (common-dimen (a <unit>) (b <unit>))
  (if (eq? a b)
      1
      (let ((com (cancel-powers (dimen-powers a) (dimen-powers b))))
        (if (null? com)
            (/ (factor b) (factor a))
            #f))))

(define (quantity+ a b)
  (let ((f (or
            (common-dimen (unit a) (unit b))
            (error "binary+: quantities ~s and ~s are incommensurate" a b))))
    (make <nquantity>
          value: (+ (value a) (* f (value b)))
          unit: (unit a))))

(define (quantity- a b)
  (let ((f (or
            (common-dimen (unit a) (unit b))
            (error "binary-: quantities ~s and ~s are incommensurate" a b))))
    (make <nquantity>
          value: (- (value a) (* f (value b)))
          unit: (unit a))))
  
(define-method binary* ((a <nquantity>) (b <nquantity>)) (quantity* a b))
(define-method binary/ ((a <nquantity>) (b <nquantity>)) (quantity/ a b))
(define-method binary+ ((a <nquantity>) (b <nquantity>)) (quantity+ a b))
(define-method binary- ((a <nquantity>) (b <nquantity>)) (quantity- a b))

(define-method binary* ((a <nquantity>) (b <number>)) (quantity* a b))
(define-method binary/ ((a <nquantity>) (b <number>)) (quantity/ a b))
(define-method binary+ ((a <nquantity>) (b <number>)) (quantity+ a b))
(define-method binary- ((a <nquantity>) (b <number>)) (quantity- a b))

(define-method binary* ((a <number>) (b <nquantity>)) (quantity* a b))
(define-method binary/ ((a <number>) (b <nquantity>)) (quantity/ a b))
(define-method binary+ ((a <number>) (b <nquantity>)) (quantity+ a b))
(define-method binary- ((a <number>) (b <nquantity>)) (quantity- a b))
