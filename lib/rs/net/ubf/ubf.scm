

(define $abbrev "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")

(define-class <ubf-output-context> (<object>)
  output-port
  (abbrev init-function: make-table))

;;;

(define (ubf-prescan self tbl)
  (cond
   ((table-lookup tbl self)
    => (lambda ((e <pair>))
         (set-car! e (+ (car e) 1))
         (cdr e)))
   (else
    (let ((cost (ubf-prescan* self tbl)))
      (table-insert! tbl self (cons 1 cost))
      cost))))

;; note that `write-ubf' should write CANONICAL ubf

(define-generic-function write-ubf ((self <object>) (port <output-port>)))

(define-method ubf-prescan* ((self <object>) tbl)
  (string-length
   (call-with-output-string
    (lambda (port)
      (write-ubf self port)))))

(define-method ubf-prescan* ((self <integer>) tbl)
  1)

(define-method ubf-prescan* ((self <byte-vector>) tbl)
  (+ 5 (bvec-length self)))

(define-method ubf-prescan* ((self <string>) tbl)
  (+ 2 (string-length self)))

(define-method ubf-prescan* ((self <symbol>) tbl)
  (+ 3 (string-length (symbol->string self))))

(define-method ubf-prescan* ((self <empty-list>) tbl)
  0)

(define-method ubf-prescan* ((self <pair>) tbl)
  (+ 1
     (ubf-prescan (car self) tbl)
     (ubf-prescan (cdr self) tbl)))
     

(define-method ubf-prescan* ((self <vector>) tbl)
  (let loop ((i 0)
             (sum 3))   ; even an empty vector costs some...
    (if (< i (vector-length self))
        (loop (+ i 1)
              (+ sum (ubf-prescan (vector-ref self i) tbl)))
        sum)))

;;;

(define-method to-ubf ((self <object>) ctx)
  (write-ubf self (output-port ctx)))
   
(define-method to-ubf ((self <byte-vector>) ctx)
  (write-string
   (output-port ctx)
   (string-append (number->string (bvec-length self))
                  " ~"
                  (bvec->string self)
                  "~")))

(define-method to-ubf ((self <integer>) ctx)
  (write-string (output-port ctx) (number->string self))
  (output-port-write-char (output-port ctx) #\space))

(define-method to-ubf ((self <vector>) ctx)
  (write-string (output-port ctx) "{")
  (vector-for-each
   (lambda (item)
     (internal-write-ubf item ctx))
   self)
  (write-string (output-port ctx) "}"))

(define-method to-ubf ((self <empty-list>) ctx)
  (output-port-write-char (output-port ctx) #\#))

(define-method to-ubf ((self <pair>) ctx)
  (internal-write-ubf (cdr self) ctx)
  (internal-write-ubf (car self) ctx)
  (output-port-write-char (output-port ctx) #\&))

(define-method to-ubf ((self <symbol>) ctx)
  (output-port-write-char (output-port ctx) #\')
  (write-string-escaped (output-port ctx) (symbol->string self) #\')
  (output-port-write-char (output-port ctx) #\'))

(define-method to-ubf ((self <string>) ctx)
  (output-port-write-char (output-port ctx) #\")
  (write-string-escaped (output-port ctx) self #\")
  (output-port-write-char (output-port ctx) #\"))

(define (write-string-escaped port str esc)
  (let ((x (string-search str #\\))
        (y (string-search str esc)))
    (if (or x y)
        (for-each
         (lambda (p)
           (if (string=? p "\\")
               (write-string port "\\\\")
               (if (string=? p (string esc))
                   (begin
                     (write-string port "\\")
                     (write-string port (string esc)))
                   (write-string port p))))
           (string-split/including str (reg-expr->proc `(or #\\ ,esc))))
        (write-string port str))))

(define (internal-write-ubf self ctx)
  (cond
   ((table-lookup (abbrev ctx) self)
    => (lambda (item)
         (if (char? item)
             (output-port-write-char (output-port ctx) item)
             (let (((ch <ascii-char>) (car item)))
               (to-ubf self ctx)
               (write-string (output-port ctx) (string #\> ch ch))
               (table-insert! (abbrev ctx) self ch)))))
   (else
    (to-ubf self ctx))))



(define (object->compact-ubf u #optional port)
  (let ((ctx (make <ubf-output-context>
                   output-port: (or port (open-output-string))))
        (pre (make-table)))
    ;;
    (ubf-prescan u pre)
    ;;
    ;(print pre)
    ;;
    (let ((n 0)
          (a (abbrev ctx)))
      ;;
      (table-for-each
       pre
       (lambda (h k v)
         (if (and (> (car v) 1)
                  (not (memq k '(() -1 0 1 2 3 4 5 6 7 8 9))))
             (begin
               (table-insert! a k (cons (string-ref $abbrev n) v))
               (set! n (+ n 1)))))))
    ;;
    ;(print (abbrev ctx))
    ;;
    (internal-write-ubf u ctx)
    (newline (output-port ctx))
    (if port
        (values)
        (get-output-string (output-port ctx)))))
