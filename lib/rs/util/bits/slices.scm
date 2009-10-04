(define (make-slice-setter bit-offset bit-width)
  (let ((insns '()))
    (define (insn msg . args)
      (set! insns (cons (apply format #f msg args) insns)))
    ;; special case if the start and end octets are the same
    (assert (< bit-offset 8))
    ;; special case if the start and end octets are the same
    (if (<= (+ bit-offset bit-width) 8)
        (let* ((sh (- 8 (+ bit-offset bit-width)))
               (mask (format #f "(((1<<~d)-1)<<~d)" bit-width sh)))
          (insn "data[0] = (data[0] & ~~~a) | ((a << ~d) & ~a)"
                mask
                sh
                mask)
          (values insns 1))
        ;;
        (bind ((p w (if (= (modulo bit-offset 8) 0)
                        (begin
                          (values (quotient bit-offset 8) 
                                  bit-width))
                        (let ((left-bits (- 8 (modulo bit-offset 8))))
                          (insn "data[0] = (data[0] & ~~((1<<~d)-1)) | (a & ((1<<~d)-1))" 
                                left-bits 
                                left-bits)
                          (values (+ (quotient bit-offset 8) 1)
                                  (- bit-width left-bits))))))
          (let loop ((p p)
                     (w w))
            (if (>= w 8)
                (begin
                  (insn "a >>= 8")
                  (insn "data[~d] = a" p)
                  (loop (+ p 1) (- w 8)))
                (begin
                  (if (> w 0)
                      (begin
                        (insn "a >>= ~d" w)
                        (insn "data[~d] = (data[~d] & ((1<<(8-~d))-1)) | (a<<(8-~d))"
                              p p w w)
                        (values insns (+ p 1)))
                      (values insns p)))))))))

(define (make-slicer bit-offset bit-width)
  ;; return the list of C instructions,
  ;; and the number of octets accessed
  (let ((insns '()))
    (define (insn msg . args)
      (set! insns (cons (apply format #f msg args) insns)))
    (assert (< bit-offset 8))
    ;; special case if the start and end octets are the same
    (if (<= (+ bit-offset bit-width) 8)
        (let* ((sh (- 8 (+ bit-offset bit-width))))
          (insn "a = (data[0] >> ~d) & ((1<<~d)-1)" sh bit-width)
          (values insns 1))
        ;;
        (bind ((p w (if (= (modulo bit-offset 8) 0)
                        (begin
                          (insn "a = 0")
                          (values (quotient bit-offset 8) 
                                  bit-width))
                        (let ((left-bits (- 8 (modulo bit-offset 8))))
                          (insn "a = data[0] & ((1<<~d)-1)" left-bits)
                          (values (+ (quotient bit-offset 8) 1)
                                  (- bit-width left-bits))))))
          (let loop ((p p)
                     (w w))
            (if (>= w 8)
                (begin
                  (insn "a = (a << 8) + data[~d]" p)
                  (loop (+ p 1) (- w 8)))
                (begin
                  (if (> w 0)
                      (begin
                        (insn "a = (a << ~d) + (data[~d] >> (8-~d))" w p w)
                        (values (reverse insns) (+ p 1)))
                      (values (reverse insns) p)))))))))

(define (gen-slicer name setter?)
  (let ((p (open-output-string)))
    (format p "    UINT_32 a, byte_len, byte_offset;\n")
    (format p "    unsigned code;\n")
    (format p "    unsigned char *data;\n")
    (format p "    if (!BVEC_P(buf)) {\n")
    (format p "        scheme_error( \"~a: buffer not a bvec: ~~s\",\n" name)
    (format p "                      1, buf );\n")
    (format p "    }\n")
    (format p "    byte_len = SIZEOF_PTR( buf );\n")
    (if setter?
        (format p "    a = datum;\n"))
    (format p "    code = bit_width * 8 + (bit_offset & 7);\n")
    (format p "    byte_offset = bit_offset / 8;\n")
    (format p "    data = PTR_TO_DATAPTR( buf );\n")
    (format p "    data += byte_offset;\n")
    (format p "    if (STRING_P(buf)) {\n")
    (format p "       byte_len--;\n")
    (format p "    }\n")
    (format p "    switch (code) {\n")
    (for-each 
     (lambda (w)
       (for-each
        (lambda (o)
          (format p "    case ~d*8+~d:\n" w o)
          (bind ((insns nb (if setter?
                               (make-slice-setter o w)
                               (make-slicer o w))))
            (format p "        if ((byte_offset + ~d) > byte_len) {\n" nb)
            (format p "            goto wont_fit;\n")
            (format p "        }\n")
            (for-each (lambda (l)
                        (format p "        ~a;\n" l))
                      insns))
          (if (not setter?)
              (if (<= w 28)
                  (format p "        REG0 = int2fx( a );\n")
                  (format p "        goto long_stuff;\n")))
          (if (or setter? (<= w 28))
              (format p "        goto ok;\n")))
        '(0 1 2 3 4 5 6 7)))
     (cdr (range 33)))
    (format p "    }\n")
    (format p "    scheme_error( \"~a: can't handle ~~d bit width\",\n"
            name)
    (format p "                  1, raw_bit_width );\n")
    (format p " wont_fit:\n")
    (format p "    a = 0;\n")
    (format p "    scheme_error( \"~a: buffer too small for ~~d bits starting at ~~d\",\n"
            name)
    (format p "                  2, raw_bit_width, raw_bit_offset );\n")
    (if (not setter?)
        (begin
          (format p " long_stuff:\n")
          (format p "    if (a < (1<<29)) {\n")
          (format p "        REG0 = int2fx( a );\n")
          (format p "    } else {\n")
          (format p "        REG0 = int_64_compact( int_32_to_int_64( a ) );\n")
          (format p "    }\n")))

    (format p " ok:\n")
    (format p "    RETURN~a();\n" (if setter? 0 1))
    (close-output-port p)))

(define-macro (define-slicer-glue (name . args))
  `(define-safe-glue (,name ,@args)
     ,(make <curly-braced>
            text: (if (eq? name 'set-bit-slice!)
                      (gen-slicer name #t)
                      (gen-slicer name #f)))))

(define-slicer-glue (bit-slice buf
                               (bit_offset <raw-int>) 
                               (bit_width <raw-int>)))
(define-slicer-glue (set-bit-slice! buf 
                                    (bit_offset <raw-int>) 
                                    (bit_width <raw-int>) 
                                    (datum <raw-int>)))
                                   