(define (empty-string) "")

(define-method write-object ((self <substring>) port)
  (format port "#[<substring> len=~d]" (string-length self)))

(define-method print ((self <substring>))
  (format #t "substring ~s: offset ~d in parent, length ~d\n" 
          self
          (gvec-ref self 1)
          (gvec-ref self 2))
  (let* ((n (+ (gvec-ref self 1) (gvec-ref self 2)))
         (ixfw (max 4 (string-length (number->string n 16))))
         (x (gvec-ref self 0)))
    (let loop ((i (gvec-ref self 1)))
      (if (< (+ i 16) n)
          (begin
            (print-bvec-line x i 16 ixfw)
            (loop (+ i 16)))
          (print-bvec-line x i (- n i) ixfw)))))

          
(define-method substring* ((self <string>) i j)
  (if (eq? i j)
      (empty-string)
      (let ((n (fixnum- j i)))
        (make-gvec <substring> self i n n))))

(define-method substring* ((self <substring>) i j)
  (if (eq? i j)
      (empty-string)
      (let ((n (fixnum- j i)))
        (make-gvec <substring> 
                   (gvec-ref self 0)
                   (fixnum+ (gvec-ref self 1) i)
                   n
                   n))))

(define-method to-string ((self <substring>))
  (let ((b (bvec-alloc <string> (+ (string-length self) 1))))
    (bvec-copy b
               0
               (gvec-ref self 0)
               (gvec-ref self 1)
               (string-length self))
    b))

(define-method octet-ref ((self <string>) k)
  (bvec-ref self k))

(define-method octet-ref ((self <substring>) k)
  (assert (< k (gvec-ref self 3)))
  (bvec-ref (gvec-ref self 0) (+ (gvec-ref self 1) k)))

