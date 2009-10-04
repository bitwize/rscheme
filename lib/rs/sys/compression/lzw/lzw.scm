,(use tables)

;; see http://www.dogma.net/markn/articles/lzw/lzw.htm

(define-class <compressor> (<object>)
  (dictionary)
  )

(define (lzw-initial-table)
  (let ((t (make-table)))
    (for-each
     (lambda (i)
       (table-insert! t (list i) i))
     (range 256))
    (table-insert! t '(clear) 256)
    (table-insert! t '(end-of-data) 257)
    t))

(define (make-compressor)
  (make <compressor>
    dictionary: (lzw-initial-table)))

(define (lzw-code-width (self <compressor>))
  (let ((n (table-size (dictionary self))))
    (cond
     ((< n 512) 9)
     ((< n 1024) 10)
     ((< n 2048) 11)
     ((< n 4096) 12)
     (else (error "table too big")))))

(define (compress (self <compressor>) (input <list>))
  (if (null? input)
      '()
      (let ((dict (dictionary self)))
        ;;
        (define (encoding current)
          (list (lzw-code-width self) 
                (table-lookup dict current)))
        ;;
        (let loop ((input (cdr input))
                   (current (list (car input)))
                   (output '()))
          (if (null? input)
              (reverse! (cons (encoding current) output))
              (let* ((ch (car input))
                     (key (list (table-lookup dict current) ch))
                     (val (table-lookup dict key)))
                (format #t "key ~s --> ~s\n" key val)
                (if val
                    (loop (cdr input) key output)
                    (let ((enc (encoding current)))
                      (table-insert! dict key (table-size dict))
                      (loop (cdr input) (list ch) (cons enc output))))))))))



(define-method print ((self <compressor>))
  (table-for-each
   (dictionary self)
   (lambda (h k v)
     (if (> (length k) 1)
         (format #t "  ~s => ~s\n" k v)))))

(define (base85-frag str i z-ok?)
  (let ((x (+ (* (char->integer (string-ref str i)) #x1000000)
              (* (char->integer (string-ref str (+ i 1))) #x10000)
              (* (char->integer (string-ref str (+ i 2))) #x100)
              (char->integer (string-ref str (+ i 3))))))
    (if (and z-ok? (= x 0))
        "z"
        (list->string
         (map (lambda (i)
                (integer->char (+ i 33)))
              (list (quotient x (* 85 85 85 85))
                    (modulo (quotient x (* 85 85 85)) 85)
                    (modulo (quotient x (* 85 85)) 85)
                    (modulo (quotient x 85) 85)
                    (modulo x 85)))))))

  
(define (base85-encode str)
  (let ((o (open-output-string)))
    (let loop ((i 0))
      (if (< (+ i 4) (string-length str))
          (begin
            (write-string o (base85-frag str i #t))
            (loop (+ i 4)))
          (begin
            (if (< i (string-length str))
                (write-string o (base85-frag
                                 (string-append (substring str i) "\0\0\0")
                                 0
                                 #f)))
            (get-output-string o))))))

;;;

(define (packbits outport bits)
  (let loop ((accum 0)
             (nbits 0)
             (bits bits))
    (cond
     ((>= nbits 8)
      (let* ((top (logical-shift-right accum (- nbits 8)))
             (low (- accum (logical-shift-left top (- nbits 8)))))
        (output-port-write-char outport (integer->char top))
        (loop low (- nbits 8) bits)))
     ((null? bits)
      (if (> nbits 0)
          (loop accum nbits (list (list (- 8 nbits) 0)))))
     (else
      (let ((n (caar bits))
            (code (cadar bits)))
        (loop (+ (logical-shift-left accum n) code)
              (+ nbits n)
              (cdr bits)))))))

        
            

(define (pack85 bits)
  (let ((p (open-output-string)))
    (packbits p bits)
    (base85-encode (get-output-string p))))

(define (zpack str)
  (let* ((pre '((9 256)))
         (z (make-compressor))
         (mid (compress z (map char->integer (string->list str))))
         (post (list (list (lzw-code-width z) 257))))
    ;;
    (pack85 (append pre mid post))))



#|
(define a (make-compressor))
(print (compress a (map char->integer (string->list "/WED/WE/WEE/WEB/WET"))))
|#

#|

(zpack "-----A---")

/a (...) /ASCII85Decode filter /LZWDecode filter def

a read == ==
...
|#

#|

,(use graphics.image
      graphics.color)

(define (get-image-raw-data image)
  (let ((raw (open-output-string)))
    (for-each-pixel
     image
     (lambda (x y pix)
       (bind ((r g b a (pixel-rgba pix)))
         (write-char (integer->char (logical-shift-right r 8)) raw)
         (write-char (integer->char (logical-shift-right g 8)) raw)
         (write-char (integer->char (logical-shift-right b 8)) raw))))
    (get-output-string raw)))

,(use graphics.png)

(get-image-raw-data (read-png-image "homepage.png"))
|#
