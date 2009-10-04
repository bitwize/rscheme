
(define (pdf-read-number port)
  (bind ((t v (pdf-scan port)))
    (if (eq? t 'number)
        v
        (error "Expected number.  Saw: ~s ~s" t v))))

(define (pdf-read-op port name)
  (bind ((t v (pdf-scan port)))
    (if (eq? t 'op)
        (if (eq? v name)
            (values)
            (error "Expected operator ~s.  Saw: ~s" v))
        (error "Expected operator ~s.  Saw: ~s ~s" t v))))

(define (pdf-read-indirect-obj port pdf)
  (bind ((id (pdf-read-number port))
         (gen (pdf-read-number port))
         (op (pdf-read-op port 'obj)))
    (eval-indirect-obj-defn port pdf)))

(define (pair-list->dict lst)
  (let ((tbl (make-symbol-table)))
    (let loop ((l lst))
      (if (null? l)
          tbl
          (begin
            (table-insert! tbl (cadr l) (car l))
            (loop (cddr l)))))))

(define (pdf-read-eval port ender pdf)
  (let loop ((stack '()))
    (bind ((t v (pdf-scan port)))
      ;(format #t "... ~s ~s    ~s   ~s\n" t v ender stack)
      (case t
        ((number string hexstring name)
         (loop (cons v stack)))
        ((eof)
         (if (eq? ender 'eof)
             (reverse! stack)
             (error "Unexpected 'EOF' ; expecting ~s" ender)))
        ((op)
         (bind ((fn (lookup-operator v)))
           (loop (fn stack ender pdf))))
        ((dict-start)
         (loop (cons (pdf-read-eval port 'dict-end pdf) stack)))
        ((dict-end)
         (if (eq? ender 'dict-end)
             (pair-list->dict stack)
             (error "Unexpected '>>' ; expecting ~s" ender)))
        ((array-start)
         (loop (cons (pdf-read-eval port 'array-end pdf) stack)))
        ((array-end)
         (if (eq? ender 'array-end)
             (list->vector (reverse stack))
             (error "Unexpected ']' ; expecting ~s" ender)))
        (else
         (error "Unexpected token: ~s ~s" t v))))))

(define (eval-indirect-obj-defn port pdf)
  (call-with-current-continuation
   (lambda (exit)
     (pdf-read-eval port exit pdf))))

(define (lookup-operator n)
  (case n
    ((R) make-indirect-objref)
    ((endobj) end-of-indirect-object)
    ((stream) make-stream-object)
    ((endstream) end-of-stream)
    ((true) (lambda (s x p) (cons #t s)))
    ((false) (lambda (s x p) (cons #f s)))
    ((DP) def-marked-content-point-with-properties)
    ((BDC) begin-marked-content-seq-with-properties)
    ((BMC) begin-marked-content-seq)
    ((BT) begin-text)
    ((EMC) end-marked-content-seq)
    ((ET) end-text)
    ((Td) set-text-position)
    ((Tc) set-char-spacing)
    ((Tf) set-text-font-and-size)
    ((Tj) show-text)
    ((Tm) set-text-matrix)
    ((Tr) set-text-rendering)
    ((Tw) set-word-spacing)
    ((gs) set-graphic-state)
    ((i) set-flatness)
    ((k) set-cmyk-color-fill)
    ((K) set-cmyk-color-stroke)
    ((ri) set-color-rendering-intent)
    (else
     (format #t "Unknown operator: ~s\n" n)
     (lambda (stack exit pdf)
       stack))))
;(error "Unknown operator: ~s" n))))

(define (def-marked-content-point-with-properties stack exit pdf) (cddr stack))
(define (begin-marked-content-seq-with-properties stack exit pdf) (cddr stack))
(define (begin-marked-content-seq stack exit pdf) (cdr stack))
(define (end-marked-content-seq stack exit pdf) stack)

(define (begin-text stack exit pdf) stack)
(define (end-text stack exit pdf) stack)
(define (set-text-position stack exit pdf) stack)
(define (set-char-spacing stack exit pdf) (cddr stack))
(define (set-text-font-and-size stack exit pdf) (cddr stack))
(define (show-text stack exit pdf) (cddr stack))
(define (set-text-matrix stack exit pdf) (cddr stack))
(define (set-text-rendering stack exit pdf) (cddr stack))
(define (set-word-spacing stack exit pdf) (cddr stack))
(define (set-graphic-state stack exit pdf) (cddr stack))
(define (set-flatness stack exit pdf) (cddr stack))
(define (set-cmyk-color-fill stack exit pdf) (cddr stack))
(define (set-cmyk-color-stroke stack exit pdf) (cddr stack))
(define (set-color-rendering-intent stack exit pdf) (cddr stack))


(define (end-of-indirect-object stack exit pdf)
  (if (= (length stack) 1)
      (exit (car stack))
      (error "Unbalanced stack at `endobj': ~d objects" (length stack))))

(define-class <pdf-stream> (<object>)
  owner
  dict
  (stream-offset init-value: #f)
  (stream-length init-value: #f)
  (stream-contents init-value: #f))

(define-method contents ((self <pdf-stream>))
  (or (stream-contents self)
      (let* ((s (fstream (owner self)))
             (saved-at (ftell s))
             (b (make-string (stream-length self))))
        (fseek s (stream-offset self) 0)
        (fread-fill s b 0 (stream-length self))
        (set-stream-contents! self b)
        (fseek s saved-at 0)
        b)))

(define (end-of-stream stack exit pdf)
  (if (instance? (car stack) <pdf-stream>)
      stack
      (error "Unexpected `endstream' operator")))

(define (make-stream-object stack exit pdf)
  ;;
  (define (length-value x)
    (if (number? x)
        x
        (value x)))
  ;(format #t "Pre skip posn: ~d\n" (ftell (fstream pdf)))
  (pdf-skip-whitespace pdf)
  ;(format #t "Post skip posn: ~d\n" (ftell (fstream pdf)))
  ;;
  (let* ((dict (car stack))
         (stream (make <pdf-stream>
                       owner: pdf
                       dict: dict
                       stream-offset: (ftell (fstream pdf))
                       stream-length: (length-value
                                       (table-lookup dict 'Length)))))
    ;;
    ;(format #t "Stream is at offset ~d length ~d\n" (stream-offset stream) (stream-length stream))
    ;;
    ;; XXX for some reason, using a relative 
    ;;     seek (fseek ... 1) doesn't work here...
    ;;
    (fseek (fstream pdf) 
           (+ (stream-offset stream) (stream-length stream))
           0)
    ;(format #t "New posn: ~d\n" (ftell (fstream pdf)))
    (cons stream (cdr stack))))
    

(define (make-indirect-objref stack exit pdf)
  (let* ((id (cadr stack))
         (gen (car stack))
         (entry (table-lookup (object-table pdf) id)))
    (cons
     (if (instance? entry <pdf-object>)
         (if (eq? gen (generation entry))
             entry
             (begin
               (format #t "Cross-generation reference: ~s" id)
               'null))
         'null)
     (cddr stack))))
