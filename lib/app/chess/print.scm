
(define (print-board* proc sol eol)
  (for-each
   (lambda (r)
     (sol r)
     (for-each
      (lambda (f)
        (proc f r))
      '(a b c d e f g h))
     (eol r))
   '(8 7 6 5 4 3 2 1)))

(define (print-board proc)
  (let ((width 1))
    (print-board*
     (lambda (f r)
       (let ((s (proc f r)))
         (set! width (max width (string-length s)))
         (format #t "~a" s)))
     (lambda (r)
       (format #t "~a - " r))
     (lambda (r)
       (newline)))
    (format #t "     ~a\n" (make-string (* width 8) #\-))
    (format #t "     ")
    (for-each (lambda (f)
                (format #t "~a~a" (make-string (- width 1) #\space) f))
              '(a b c d e f g h))
    (newline)))

(define-method print ((self <bit-board>))
  (print-board (lambda (f r)
                 (if (bb-is-set? self f r)
                     " X"
                     " .")))
  self)

(define (print-rtf (self <board>) file)
  (let ((a '())
        (t '()))
    (print-board*
     (lambda (f r)
       (set! t (cons (font-ch self f r) t)))
     (lambda (r) (values))
     (lambda (r) 
       (set! a (cons (apply string-append (reverse t)) a)) 
       (set! t '())))
    (call-with-output-file
        file
      (lambda (port)
        (let ((pieces (apply append
                             (map (lambda (s)
                                    (string-split s "+ + + + "))
                                  (string-split (file->string "board.rtf")
                                                " + + + +")))))
          (print pieces)
          (print a)
          (let loop ((p pieces)
                     (a (reverse a)))
            (if (null? a)
                (write-string port (car p))
                (begin
                  (write-string port (car p))
                  (write-string port (car a))
                  (loop (cdr p) (cdr a))))))))))

(define (print-plain-for-chess-cases-font (self <board>))
  (format #t "1222222223\n")
  (print-board*
   (lambda (f r)
     (format #t "~a" (font-ch self f r)))
   (lambda (r)
     (format #t "4"))
   (lambda (r)
     (format #t "5\n")))
  (format #t "7888888889\n"))

(define (font-ch (self <board>) f r)
  (let* ((w? (white-square? f r))
         (down-if-white (if w?
                            (lambda (ch) 
                              (string (char-downcase (string-ref ch 0))))
                            (lambda (ch) ch))))
    (if (bb-is-set? (occupies (white self)) f r)
        (down-if-white (piece-char (piece-at (white self) f r)))
        (if (bb-is-set? (occupies (black self)) f r)
            (down-if-white 
             (string
              (string-ref
               "OMVTWL"
               (string-search "PNBRQK"
                              (piece-char (piece-at (black self) f r))))))
            (if w? " " "+")))))
