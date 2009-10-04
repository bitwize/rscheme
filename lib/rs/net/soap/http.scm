(define (strip-cr str)
  (if (string? str)
      (let (((n <fixnum>) (string-length str)))
        (if (and (fixnum>? n 0)
                 (char=? (string-ref str (sub1 n)) #\cr))
            (substring str 0 (sub1 n))
            str))
      str))

(define *header-line*
  (reg-expr->proc
   '(prefix
     (seq 
      (save (+ (not (or space #\:))))
      #\:
      (+ space)
      (save (+ any))))))

(define (header-field-symbol str)
  (string->symbol
   (list->string (map char-downcase (string->list str)))))

(define (http-read-headers port #optional initial)
  (let loop ((h (or initial '())))
    (let ((line (strip-cr (read-line port))))
      (if (or (eof-object? line)
              (eq? (string-length line) 0))
          (reverse! h)
          (bind ((s e key value (*header-line* line)))
            (if s
                (loop (cons (cons (header-field-symbol key) value)
                            h))
                (loop (cons (cons 'unknown line) h))))))))
