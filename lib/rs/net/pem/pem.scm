;;;  RFC 1421

(define-constant $pem-encoding 
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

(define-syntax (PEM-encode-char num)
  (string-ref $pem-encoding num))

(define-constant $pem-decoding
  (let ((s (bvec-alloc <byte-vector> 256)))
    (for-each (lambda (i)
                (bvec-set! s i #xFF)
                (values))
              (range 256))
    (for-each (lambda (i)
                (bvec-set! s
                           (char->integer (string-ref $pem-encoding i))
                           i)
                (values))
              (range 64))
    s))

(define (PEM-decode-error ch)
  (error "PEM-decode-char: invalid PEM encoding char: ~s" ch))

(define-syntax (PEM-decode-char ch)
  (let (((p <fixnum>) (bvec-ref $pem-decoding (char->integer ch))))
    (if (eq? p #xFF)
        (PEM-decode-error ch)
        p)))

(define (random-pem len #optional (state default: *default-random-state*))
  (if (<= len 0)
      ""
      ;; build the string (with built-in padding), rounding up to
      ;; the next even 4-char boundary (given PEM's 3->4 encoding)
      (bind ((dn pad (case (modulo len 4)
                       ((0) (values 0 0))
                       ((1) (values 1 3))
                       ((2) (values 0 2))
                       ((3) (values 0 1))))
             (s (make-string (+ len pad) #\=))
             (n (+ len dn)))
        (let loop ((i 0))
          (if (< i n)
              (begin
                (string-set! s i (string-ref $pem-encoding 
                                             (next-random state 64)))
                (loop (+ i 1)))
              s)))))

(define (pem-decode-integer (str <string>))
  (bind ((i0 neg? (if (char=? (string-ref str 0) #\-)
                      (values 1 #t)
                      (values 0 #f))))
    (let loop ((i i0)
               (accum 0))
      (if (= i (string-length str))
          (if neg? (- accum) accum)
          (let ((ch (string-ref str i)))
            (if (char-whitespace? ch)
                (loop (+ i 1) accum)
                (if (char=? ch #\=)
                    (if neg? (- accum) accum)
                    (loop (+ i 1) (+ (* accum 64) (PEM-decode-char ch))))))))))

(define-method pem-encode ((self <integer>))
  (if (< self 0)
      (string-append "-" (pem-encode (- self)))
      (let loop ((k self)
	         (r '()))
        (if (zero? k)
            (if (null? r) "A" (list->string r))
            (loop (quotient k 64)
                  (cons (PEM-encode-char (+ 0 (modulo k 64))) r))))))

(define-method pem-encode ((str <string>))
  (let* ((n (* 4 (quotient (+ (string-length str) 2) 3)))
	 (result (make-string n #\=)))
    (let loop ((i 2)
	       (j 0))
      (if (< i (string-length str))
	  ;; full input quantum available
	  (let ((num (+ (logical-shift-left
                         (char->integer (string-ref str (- i 2)))
                         16)
			(logical-shift-left
                         (char->integer (string-ref str (- i 1))) 
                         8)
			(char->integer (string-ref str i)))))
	    (let ((a (logical-shift-right num 18))
		  (b (bitwise-and 63 (logical-shift-right num 12)))
		  (c (bitwise-and 63 (logical-shift-right num 6)))
		  (d (bitwise-and 63 num)))
	      (string-set! result j (PEM-encode-char a))
	      (string-set! result (+ j 1) (PEM-encode-char b))
	      (string-set! result (+ j 2) (PEM-encode-char c))
	      (string-set! result (+ j 3) (PEM-encode-char d))
	      (loop (+ i 3) (+ j 4))))
	  ;; not a full input quantum available
	  (if (< (- i 1) (string-length str))
	      ;; two input bytes
	      (let ((num (+ (logical-shift-left
			     (char->integer (string-ref str (- i 2))) 
			     10)
			    (logical-shift-left
			     (char->integer (string-ref str (- i 1))) 
			     2))))
		(let ((a (bitwise-and 63 (logical-shift-right num 12)))
		      (b (bitwise-and 63 (logical-shift-right num 6)))
		      (c (bitwise-and 63 num)))
		  (string-set! result j (PEM-encode-char a))
		  (string-set! result (+ j 1) (PEM-encode-char b))
		  (string-set! result (+ j 2) (PEM-encode-char c))
		  result))
	      (if (< (- i 2) (string-length str))
		  ;; one input byte
		  (let ((num (char->integer (string-ref str (- i 2)))))
		    (let ((a (bitwise-and 63 (logical-shift-right num 2)))
			  (b (bitwise-and 63 (logical-shift-left num 4))))
		      (string-set! result j (PEM-encode-char a))
		      (string-set! result (+ j 1) (PEM-encode-char b))
		      result))
		  ;; none
		  result))))))

(define (pem-decode (str <string>))
  (if (not (eq? (remainder (string-length str) 4) 0))
      (error "PEM-decode-string: not a valid PEM RFC-1421 encoding: ~s" str))
  (if (string=? str "")
      str
      (let ((n (quotient (string-length str) 4))
	    (skips (if (eq? (string-ref str (- (string-length str) 2)) #\=)
		       2
		       (if (eq? (string-ref str (- (string-length str) 1)) #\=)
			   1
			   0))))
	(let ((result (make-string (- (* n 3) skips) #\space)))
	  (let loop ((i 0)
		     (j 0))
	    ;(format #t "~d ~d : ~s\n" i j (substring str j))
	    (if (< j (string-length str))
		(let ((num (+ (logical-shift-left (PEM-decode-char (string-ref str j))
					  18)
			      (logical-shift-left (PEM-decode-char 
					   (string-ref str (+ j 1))) 
					  12)
			      (if (< (+ i 1) (string-length result))
				  (logical-shift-left
				   (PEM-decode-char (string-ref str (+ j 2))) 
				   6)
				  0)
			      (if (< (+ i 2) (string-length result))
				  (PEM-decode-char (string-ref str (+ j 3)))
				  0))))
		  ;(format #t "   num = ~06x\n" num)
		  (string-set! result 
			       i
			       (integer->char (logical-shift-right num 16)))
		  (if (< (+ i 1) (string-length result))
		      (begin
			(string-set! result 
				     (+ i 1)
				     (integer->char
				      (bitwise-and 
				       255
				       (logical-shift-right num 8))))
			(if (< (+ i 2) (string-length result))
			    (string-set! result
					 (+ i 2)
					 (integer->char
					  (bitwise-and num 255))))))
		  (loop (+ i 3) (+ j 4)))
		result))))))
