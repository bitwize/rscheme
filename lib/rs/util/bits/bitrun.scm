;;;
;;;  NOTE:  The PowerPC Compiler Writer's Guide has a chapter
;;;         called "Clever Examples" which talks about how
;;;         to efficiently do some of this stuff...

(define (num-low-bits-set n)
  (let loop ((i 0))
    (if (bit-ref n i)
	(loop (+ i 1))
	i)))

(define (num-high-bits-set n)
  (let loop ((i 8))
    (if (and (> i 0) (bit-ref n (- i 1)))
	(loop (- i 1))
	(- 8 i))))

#|
(define (num-low-bits-clear n) (num-low-bits-set (bitwise-xor #xff n)))
(define (num-high-bits-clear n) (num-high-bits-set (bitwise-xor #xff n)))
|#

(define (num-low-bits-clear n)
  (vector-ref '#(8 0 1 0 2 0 1 0 3 0 1 0 2 0 1 0 4 0 1 0 2 0 1 0 3 0 1 0 2 0 1 0 5 0 1 0 2 0 1 0 3 0 1 0 2 0 1 0 4 0 1 0 2 0 1 0 3 0 1 0 2 0 1 0 6 0 1 0 2 0 1 0 3 0 1 0 2 0 1 0 4 0 1 0 2 0 1 0 3 0 1 0 2 0 1 0 5 0 1 0 2 0 1 0 3 0 1 0 2 0 1 0 4 0 1 0 2 0 1 0 3 0 1 0 2 0 1 0 7 0 1 0 2 0 1 0 3 0 1 0 2 0 1 0 4 0 1 0 2 0 1 0 3 0 1 0 2 0 1 0 5 0 1 0 2 0 1 0 3 0 1 0 2 0 1 0 4 0 1 0 2 0 1 0 3 0 1 0 2 0 1 0 6 0 1 0 2 0 1 0 3 0 1 0 2 0 1 0 4 0 1 0 2 0 1 0 3 0 1 0 2 0 1 0 5 0 1 0 2 0 1 0 3 0 1 0 2 0 1 0 4 0 1 0 2 0 1 0 3 0 1 0 2 0 1 0)
	      n))

(define (num-high-bits-clear n)
  (vector-ref '#(8 7 6 6 5 5 5 5 4 4 4 4 4 4 4 4 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	      n))

(define (set-low-bits n)
  (let ((l (num-low-bits-clear n)))
    (bitwise-or n (bit-run-mask 0 l))))

(define (set-high-bits n)
  (let ((r (num-high-bits-clear n)))
    (bitwise-or n (bit-run-mask (- 8 r) r))))

(define (bit-run-mask bit len)
  (logical-shift-left (- (expt 2 len) 1) bit))

(define (find-clear-bit n)
  (let loop ((i 0))
    (if (bit-ref n i)
	(loop (+ i 1))
	i)))

(define (find-bit-run n)
  (let ((at (find-clear-bit n)))
    (let loop ((l 1))
      (let ((m (bit-run-mask at (+ l 1))))
	(if (= m (bitwise-and (bitwise-not n) m))
	    (loop (+ l 1))
	    (values at l))))))


(define (bit-runs n)
  (let loop ((n (set-high-bits (set-low-bits n)))
	     (r '()))
    (if (eq? n #xFF)
	(reverse r)
	(bind ((bit len (find-bit-run n)))
	  (loop (bitwise-or n (bit-run-mask bit len))
		(cons (list len bit) r))))))

(define *middle-bits* '#(#(#f #f #f #f #f #f #f #f #f 1 #f #f #f #f #f #f #f 1 2 2 #f #f #f #f #f 1 #f #f #f #f #f #f #f 1 2 2 3 3 3 3 #f 1 #f #f #f #f #f #f #f 1 2 2 #f #f #f #f #f 1 #f #f #f #f #f #f #f 1 2 2 3 3 3 3 4 1 4 4 4 4 4 4 #f 1 2 2 #f #f #f #f #f 1 #f #f #f #f #f #f #f 1 2 2 3 3 3 3 #f 1 #f #f #f #f #f #f #f 1 2 2 #f #f #f #f #f 1 #f #f #f #f #f #f #f 1 2 2 3 3 3 3 4 1 4 4 4 4 4 4 5 5 2 2 5 5 5 5 5 1 5 5 5 5 5 5 #f 1 2 2 3 3 3 3 #f 1 #f #f #f #f #f #f #f 1 2 2 #f #f #f #f #f 1 #f #f #f #f #f #f #f 1 2 2 3 3 3 3 4 1 4 4 4 4 4 4 #f 1 2 2 #f #f #f #f #f 1 #f #f #f #f #f #f #f 1 2 2 3 3 3 3 #f 1 #f #f #f #f #f #f #f 1 2 2 #f #f #f #f #f 1 #f #f #f #f #f #f) 
			 #(#f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f 1 #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f 1 2 2 #f #f #f #f #f #f #f #f #f #f #f #f #f 1 #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f 1 2 2 3 3 3 3 #f #f #f #f #f #f #f #f #f 1 #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f 1 2 2 #f #f #f #f #f #f #f #f #f #f #f #f #f 1 #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f 1 2 2 3 3 3 3 4 4 4 4 4 4 4 4 #f 1 #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f 1 2 2 #f #f #f #f #f #f #f #f #f #f #f #f #f 1 #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f 1 2 2 3 3 3 3 #f #f #f #f #f #f #f #f #f 1 #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f 1 2 2 #f #f #f #f #f #f #f #f #f #f #f #f #f 1 #f #f #f #f #f #f #f #f #f #f #f #f #f #f) 
			 #(#f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f 1 #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f 1 2 2 #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f 1 #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f 1 2 2 3 3 3 3 #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f 1 #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f 1 2 2 #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f 1 #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f) 
			 #(#f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f 1 #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f 1 2 2 #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f 1 #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f) 
			 #(#f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f 1 #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f)))

(define (find-middle-bits n w)
  (vector-ref (vector-ref *middle-bits* (- w 2)) n))

#|
   This is how we compute it...

(define (find-middle-bits n w)
  (let ((r (bit-runs n)))
    (let loop ((w w))
      (if (< w 8)
	  (let ((a (assq w r)))
	    (if a
		(cadr a)
		(loop (+ w 1))))
	  #f))))

(define (vector-range k) (list->vector (range k)))

(define *middle-bits*
  (vector-map
   (lambda (w)
     (vector-map (lambda (n)
		   (find-middle-bits n w))
		 (vector-range 256)))
   (subvector (vector-range 7) 2)))

|#

;;;
;;; now, we can write the bit searcher procedure
;;;
;;; there are specialization partitions:
;;;   len == 1  (most things fold out) 
;;;   len < 6
;;;   len == 6  (find-middle-bits only returns 1 when n=#b10000001)
;;;   len >= 7  (find-middle-bits always returns #f)

(define (find-long-bit-run bitmap len)
  (case len
    ((1) (find-long-bit-run/1 bitmap))
    ((6) (find-long-bit-run/6 bitmap))
    (else
     (if (< len 6)
	 (find-long-bit-run/small bitmap len)
	 (find-long-bit-run/big bitmap len)))))

(define-syntax (find-long-bit-run-loop bitmap len fmb)
  (let loop ((bits-in-run 0)
	     (i 0))
    (if (< i (vector-length bitmap))
	(let ((b (vector-ref bitmap i)))
	  (let ((bits-in-new-run (+ bits-in-run (num-low-bits-clear b))))
	    (if (< bits-in-new-run len)
		(if (= b 0)
		    (loop bits-in-new-run (+ i 1))
		    (if (and (< len 8) (fmb b))
			(+ (* 8 i) (fmb b))
			(loop (num-high-bits-clear b) (+ i 1))))
		(- (+ (* 8 i) (- len bits-in-run)) len))))
	#f)))

(define (find-long-bit-run/1 bitmap)
  (let loop ((i 0))
    (if (< i (vector-length bitmap))
	(let ((b (vector-ref bitmap i)))
	  (if (eq? b #xFF)
	      (loop (+ i 1))
	      (+ (* 8 i) (find-clear-bit b))))
	#f)))

(define (find-long-bit-run/small bitmap len)
  (let ((v (vector-ref *middle-bits* (- len 2))))
    (find-long-bit-run-loop bitmap len 
			    (lambda (n) 
			      (vector-ref v n)))))

(define (find-long-bit-run/6 bitmap)
  (find-long-bit-run-loop bitmap 6 
			  (lambda (n)
			    (if (eq? n #b10000001) 1 #f))))

(define (find-long-bit-run/big bitmap len)
  (find-long-bit-run-loop bitmap len 
			  (lambda (n) 
			    #f)))

#|

(define *test-cases*
  '((#(#b11111100 #b11111111 #b11100011 #b11111111 #b11111111) 2    0)
    (#(#b11111111 #b11111111 #b11100011 #b11111111 #b11111111) 3   18)
    (#(#b11110011 #b01111111 #b11111000 #b11111111 #b11111111) 4   15)
    (#(#b10111011 #b01110000 #b11110000 #b11111111 #b11111111) 5   15)
    (#(#b10111011 #b00110000 #b11110000 #b11111111 #b11111111) 6   14)
    (#(#b10111011 #b00110000 #b10000001 #b11111111 #b11111111) 6   17)
    (#(#b10111011 #b00110000 #b10000001 #b11111111 #b11111111) 1    2)
    (#(#b11111111 #b11111111 #b10000001 #b11111111 #b11111111) 1   17)
    (#(#b10111011 #b00110000 #b00000001 #b11111110 #b11111111) 8   17)
    (#(#b10111011 #b10110000 #b00000000 #b11111110 #b11111111) 8   16)
    (#(#b10111011 #b10110000 #b00000000 #b11111111 #b11111111) 8   16)
    (#(#b10111011 #b00110000 #b01110000 #b00000000 #b10000000) 16  23)
    (#(#b10111011 #b00110000 #b01110000 #b00000000 #b10000000) 4    8)
    (#(#b10111011 #b00001001 #b01110000 #b00000000 #b10000000) 4   12)
    ))

(define (t case)
  (format #t "run(~d) => ~s (expected ~d)\n"
	  (cadr case)
	  (find-long-bit-run (car case) (cadr case))
	  (caddr case)))
|#
