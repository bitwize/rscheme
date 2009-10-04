;;;
;;;   Take a (parsed) RE and generate a string that will match
;;;

(define (bit-vector-and (vec1 <vector>)
                        (vec2 <vector>))
  (let ((v (make-vector 32 0)))
    (let loop (((i <fixnum>) 0))
      (if (eq? i 32)
	  v
	  (begin
	    (vector-set! v i (bitwise-and (vector-ref vec1 i)
                                          (vector-ref vec2 i)))
	    (loop (add1 i)))))))

;;; try to generate a printable character

(define (generate-char-in-bitvec vec G)
  (or (generate-char-in-bitvec* (bit-vector-and vec *PRINTABLE*) G)
      (generate-char-in-bitvec* vec G)))

(define *PRINTABLE* '#(0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 254
                         0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))

(define (char-in-bitvec-generator vec)
  (let* ((m (bitvec->member-string (bit-vector-and vec *PRINTABLE*))))
    (if (> (string-length m) 1)
        (lambda (R)
          (string (string-ref m (R (string-length m)))))
        (let ((m (bitvec->member-string vec)))
          (if (> (string-length m) 0)
              (lambda (R)
                (string (string-ref m (R (string-length m)))))
              #f)))))
          
(define (bitvec->member-string vec)
  (apply string (map integer->char (bitvec->members vec))))

(define (bitvec->members vec)
  (let loop ((i 255)
             (r '()))
    (if (= i -1)
        r
        (loop (- i 1)
              (if (vector-bit? vec i)
                  (cons i r)
                  r)))))

(define (bound2-count-generator a b)
  (let ((n (- b a)))
    (lambda (R)
      (+ a (R n)))))

(define (bound1-count-generator n)
  (lambda (R)
    (let loop ((i n))
      (if (= (R 5) 0)
          i
          (loop (+ i 1))))))
  
(define (reg-expr->match re #optional R)
  ((reg-expr->match-generator re) (or R random)))

(define (reg-expr->match-generator re)
  (let ((f (reg-expr->match-generator* re)))
    (lambda (#optional R)
      (f (or R random)))))

(define (reg-expr->match-generator* re)
  (cond
   ((pair? re)
    (case (car re)
      ((posix)
       (reg-expr->match-generator* (parse-posix-regex (cadr re))))
      ((seq)
       (let ((seq (map reg-expr->match-generator* (cdr re))))
         (lambda (R)
           (apply string-append (map (lambda (g) (g R)) seq)))))
      ((or)
       (let ((n (length (cdr re)))
             (alternatives (map reg-expr->match-generator* (cdr re))))
         (lambda (R)
           ((list-ref alternatives (R n)) R))))
      ((range)
       (let ((n (- (char->integer (caddr re))
                   (char->integer (cadr re))))
             (first (char->integer (cadr re))))
         (lambda (R)
           (string (integer->char (+ first (R n)))))))
      ((bound + * ?)
       (let ((count (case (car re)
                      ((bound) 
                       (case (length (cadr re))
                         ((1) (bound1-count-generator (caadr re)))
                         ((2) (bound2-count-generator (caadr re)
                                                      (cadadr re)))))
                      ((+) (bound1-count-generator 1))
                      ((*) (bound1-count-generator 0))
                      ((?) (bound2-count-generator 0 1))))
             (sub (reg-expr->match-generator* (case (car re)
                                                ((bound) (caddr re))
                                                (else (cadr re))))))
         (lambda (R)
           (string-join "" (map (lambda (k) (sub R))
                                (range (count R)))))))
      ((not)
       (or (char-in-bitvec-generator (data (compile-reg-expr-not re)))
           (error "reg-expr->match-generator: `not' RE cannot generate anything: ~s" re)))
      (else
       (error "reg-expr->match-generator: unrecognized form: ~s" re))))
   ((string? re)
    (lambda (R) re))
   ((char? re)
    (let ((s (string re)))
      (lambda (R) s)))
   ((symbol? re)
    (let ((ent (assq re *regexp-macros*)))
      (if ent
          (or (char-in-bitvec-generator (cdr ent))
              (error "reg-expr->match-generator: `~s' char class is empty" re))
          (error "reg-expr->match-generator: unrecognized macro ~s" re))))
   (else
    (error "reg-expr->match-generator: unrecognized form: ~s" re))))
