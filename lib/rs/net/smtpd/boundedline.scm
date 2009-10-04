(define cr-or-lf (reg-expr->proc '(or (seq #\cr #\lf)
                                      #\lf)))

(define-class <bounded-line-input-port> (<buffered-input-port>)
  (underlying-input-port type: <input-port>))

(define-method provide-more-input ((self <bounded-line-input-port>))
  (let ((s (input-port-read-max (underlying-input-port self) 1024)))
    (if (string? s)
        s
        #f)))

(define-method more-input-ready? ((self <bounded-line-input-port>))
  (more-input-ready? (underlying-input-port self)))

(define-method input-port-read-line ((self <bounded-line-input-port>))
  (let loop ((try 0))
    (bind ((i (buffered-input-posn self))
           (b (buffered-input-buffer self))
           (s e (cr-or-lf b i)))
      (if s
          (begin
            (set-buffered-input-posn! self e)
            (substring (buffered-input-buffer self) i s))
          (if (= try 0)
              (let ((more (provide-more-input self)))
                (if more
                    (begin
                      (set-buffered-input-posn! self 0)
                      (set-buffered-input-buffer! 
                       self
                       (string-append (substring b i) more))
                      (loop (add1 try)))
                    (begin
                      (set-buffered-input-posn! self 0)
                      (set-buffered-input-buffer! self "")
                      b)))
              (error "Could not read line within message limit"))))))

