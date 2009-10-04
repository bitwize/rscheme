
(define $pem-encoding "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

(define (PEM-encode-char (num <fixnum>))
  (string-ref $pem-encoding num))

(define $pem-decoding (reverse (string->list $pem-encoding)))

(define (PEM-decode-char ch)
  (let ((p (memq ch $pem-decoding)))
    (if p
	(length (cdr p))
	(error "PEM-decode-char: invalid PEM encoding char: ~s" ch))))

(define (PEM-encode-string (str <string>))
  (let* ((n (* 4 (quotient (+ (string-length str) 2) 3)))
	 (result (make-string n #\=)))
    (let loop ((i 2)
	       (j 0))
      (if (< i (string-length str))
	  ;; full input quantum available
	  (let ((num (+ (logical-shift-left (char->integer (string-ref str (- i 2))) 16)
			(logical-shift-left (char->integer (string-ref str (- i 1))) 8)
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
	      (let ((num (+ (logical-shift-left (char->integer (string-ref str (- i 2))) 10)
			    (logical-shift-left (char->integer (string-ref str (- i 1))) 2))))
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

(define (PEM-decode-string (str <string>))
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
		(let ((num (+ (logical-shift-left (PEM-decode-char (string-ref str j)) 18)
			      (logical-shift-left (PEM-decode-char (string-ref str (+ j 1))) 12)
			      (if (< (+ i 1) (string-length result))
				  (logical-shift-left (PEM-decode-char (string-ref str (+ j 2))) 6)
				  0)
			      (if (< (+ i 2) (string-length result))
				  (PEM-decode-char (string-ref str (+ j 3)))
				  0))))
		  ;(format #t "   num = ~06x\n" num)
		  (string-set! result i (integer->char (logical-shift-right num 16)))
		  (if (< (+ i 1) (string-length result))
		      (begin
			(string-set! result 
				     (+ i 1)
				     (integer->char (bitwise-and 255 (logical-shift-right num 8))))
			(if (< (+ i 2) (string-length result))
			    (string-set! result
					 (+ i 2)
					 (integer->char (bitwise-and num 255))))))
		  (loop (+ i 3) (+ j 4)))
		result))))))
