
(define (pdf-scan port)
  (let ((ch (read-char port)))
    (case ch
      ((#\/)
       (pdf-scan-name port '() 'name))
      ((#\()
       (pdf-scan-string port))
      ((#\%)
       (let loop ()
         (if (memq (read-char port) '(#\cr #\lf))
             (pdf-scan port)
             (loop))))
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\+ #\-)
       (pdf-scan-number port (list ch)))
      ((#\<)
       (if (char=? (peek-char port) #\<)
           (begin
             (read-char port)
             'dict-start)
           (pdf-scan-hex-encoded-string port)))
      ((#\>)
       (if (char=? (peek-char port) #\>)
           (begin
             (read-char port)
             'dict-end)
           (error "Unknown character after '>'")))
      ((#\[) 'array-start)
      ((#\]) 'array-end)
      ((#\{) 'block-start)
      ((#\}) 'block-end)
      ((#\))
       (error "Unexpected close paren"))
      (else
       (cond
        ((eof-object? ch)
         'eof)
        ((char-whitespace? ch)
         (pdf-scan port))
        ((and (> (char->integer ch) 32)
              (< (char->integer ch) 128))
         (pdf-scan-name port (list ch) 'op))
        (else
         (error "Unknown token start character: ~s" ch)))))))

(define (pdf-scan-name port pre type)
  (let loop ((r (reverse pre)))
    (let ((ch (peek-char port)))
      (if (char-namish? ch)
          (loop (cons (read-char port) r))
          (values type
                  (string->symbol (list->string (reverse! r))))))))

(define (pdf-scan-number port pre)
  (let loop ((r (reverse pre)))
    (let ((ch (peek-char port)))
      (if (or (char-numeric? ch)
              (eq? ch #\.))
          (loop (cons (read-char port) r))
          (values 'number
                  (string->number (list->string (reverse! r))))))))

(define (pdf-scan-hex-encoded-string port)
  (let loop ((r '()))
    (let ((ch1 (read-char port)))
      (cond
       ((eq? ch1 #\>)
        (values 'hexstring (list->string (reverse! r))))
       ((char-whitespace? ch1)
        (loop r))
       (else
        (let ((ch2 (read-char port)))
          (loop (cons (integer->char (string->number (string ch1 ch2) 16)) 
                      r))))))))

(define (pdf-skip-whitespace pdf)
  (let ((s (fstream pdf)))
    (let loop ()
      (if (char-whitespace? (fpeekc s))
          (begin
            (fgetc s)
            (loop))))))
        
(define (char-octal? ch)
  (memq ch '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7)))

(define (char-namish? ch)
  (and (> (char->integer ch) 32)
       (< (char->integer ch) 128)
       (not (memq ch '(#\( #\) #\< #\> #\[ #\] #\{ #\} #\/ #\%)))))

(define (pdf-scan-string port)
  (let loop ((r '())
             (k 0))
    (let ((ch (read-char port)))
      (cond
       ((eq? ch #\))
        (if (<= k 0)
            (values 'string (list->string (reverse! r)))
            (loop (cons ch r) (- k 1))))
       ((eq? ch #\()
        (loop (cons ch r) (+ k 1)))
       ((eq? ch #\\)
        (let ((ch2 (read-char port)))
          (case ch2
            ((#\newline) (loop r k))
            ((#\n) (loop (cons #\newline r) k))
            ((#\r) (loop (cons #\cr r) k))
            ((#\t) (loop (cons #\tab r) k))
            ((#\b) (loop (cons #\bs r) k))
            ((#\f) (loop (cons #\ff r) k))
            ((#\( #\) #\\) (loop (cons ch2 r) k))
            ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7)
             (let* ((ch3 (and (char-octal? (peek-char port))
                              (read-char port)))
                    (ch4 (and (char-octal? (peek-char port))
                              (read-char port))))
               (loop (cons (integer->char
                            (string->number 
                             (cond
                              (ch4 (string ch2 ch3 ch4))
                              (ch3 (string ch2 ch3))
                              (else (string ch2)))
                             8))
                           r)
                     k)))
            (else
             (error "Unknown escape character in string: ~s" ch2)))))
       (else
        (loop (cons ch r) k))))))
