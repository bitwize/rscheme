,(use rs.util.bits)

(define-class <segment> (<object>)
  (segment-data type: <byte-vector>)
  (segment-offset type: <fixnum>)
  (segment-length type: <fixnum>))

(define (segment-eval* expr alist)
  (cond
   ((number? expr)
    expr)
   ((symbol? expr)
    (if (assq expr alist)
        (cdr (assq expr alist))
        (error "unpack-segment: ~s undefined" expr)))
   ((pair? expr)
    (case (car expr)
      ((/)
       (/ (segment-eval* (cadr expr) alist)
          (segment-eval* (caddr expr) alist)))
      (else
       (error "unpack-segment: ~s unknown" expr))))
   (else
       (error "unpack-segment: ~s is a bad expr" expr))))
    
;;; Make this faster using compile-time expansion

(define (pack-segment* type value)
  (format #t "pack ~#@*40s with ~#@20s\n" type value)
  (case (car type)
    ((uint)
     (let ((b (bvec-alloc <byte-vector> (cadr type))))
       (set-bit-slice! b 0 (* 8 (cadr type)) value)
       b))
    ((enum)
     (if (symbol? value)
         (let loop ((l (cddr type)))
           (if (null? l)
               (error "Unknown enum symbol: ~s" value)
               (if (eq? value (cadr (car l)))
                   (pack-segment* (cadr type) (car (car l)))
                   (loop (cdr l)))))
         (pack-segment* (cadr type) value)))
    ((array)
     (if (null? (cadr type))
         (pack-segment* (caddr type) value)
         (if (and (equal? (caddr type) '(uint 1))
                  (instance? value <byte-vector>))
             value
             (vector-map
              (lambda (item)
                (pack-segment* (cons* 'array (cdr (cadr type)) (cddr type))
                               item))
              value))))
    ((struct)
     (list->vector
      (map (lambda (field)
             (pack-segment* (cadr (memq 'type: field))
                            (cadr (assq (car field) value))))
           (cdr type))))))

(define (unpack-segment* (data <byte-vector>) 
                         (offset <fixnum>) 
                         (limit <fixnum>)
                         type 
                         alist)
  ;(format #t "unpack: ~s at ~s\n" type offset)
  (case (car type)
    ((struct)
     (let loop ((s (cdr type))
                (i offset)
                (r '())
                (a alist))
       (if (null? s)
           (values (reverse r) i)
           (bind ((e (car s))
                  (t (cadr (memq 'type: e)))
                  (item p (unpack-segment* data i limit t a)))
             (loop (cdr s)
                   p
                   (cons (list (car e) item) r)
                   (cons (cons (car e) item) a))))))
    ((array)
     (if (null? (cadr type))
         (unpack-segment* data offset limit (caddr type) alist)
         (let* ((nbytes (segment-eval* (car (cadr type)) alist))
                (subtype (cons* 'array (cdr (cadr type)) (cddr type))))
           (let loop ((k 0)
                      (i offset)
                      (r '()))
             (if (< i (+ offset nbytes))
                 (bind ((item p (unpack-segment* data i limit subtype alist)))
                   (loop (+ k 1) p (cons item r)))
                 (values (list->vector (reverse! r)) i))))))
    ((enum)
     (bind ((item p (unpack-segment* data offset limit (cadr type) alist))
            (entry (assq item (cddr type))))
       (if entry
           (values (cadr entry) p)
           (if (assq 'else (cddr type))
               (values
                (string->symbol
                 (format #f (cadr (assq 'else (cddr type))) item))
                p)
               (error "unknown value for enum ~s: ~s" type item)))))
    ((uint)
     (let ((n (cadr type)))
       (values (bit-slice data (* offset 8) (* n 8))
               (+ offset n))))
    (else
     (error "unpack-segment: bad type ~s" type))))

(define (unpack-segment (self <segment>) tdesc)
  (bind ((value x (unpack-segment* (segment-data self)
                                   (segment-offset self)
                                   (+ (segment-offset self)
                                      (segment-length self))
                                   tdesc
                                   '())))
    (values value
            (make <segment>
                  segment-data: (segment-data self)
                  segment-offset: x
                  segment-length: (- (+ (segment-offset self)
                                        (segment-length self))
                                     x)))))

(define (t)
  (let ((b (bvec-alloc <byte-vector> 20)))
    (bvec-set! b 3 #x02)
    (bvec-set! b 4 #x01)
    (bvec-set! b 5 #x02)
    (make <segment>
          segment-data: b
          segment-offset: 3
          segment-length: 3)))

          

;;;;


(define-module-extend rs.sys.threads.manager ()

(define-method write-bytes ((self <output-port>) src offset len)
  (let ((str (make-string (bvec-length src))))
    (bvec-copy str 0 src offset len)
    (write-string self str)))
)

