#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/iolib/print.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.8
 | File mod date:    1997-11-29 23:10:41
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  iolib
 |
 | Purpose:          'print' generic function and implementation
 `------------------------------------------------------------------------|#

;;

(define (print.print1 name index item)
    (let ((s (if name
		 (format #f "    [~d] ~s = " index name)
		 (format #f "    [~d] = " index))))
	(display s)
	(display (object->bounded-string (- 76 (string-length s))
					    item))
	(newline)))

(define (print.print* name index)
    (if name
	(format #t "    [~d]? ~s\n" index name)
	(format #t "    [~d]?\n" index)))

(define-method print ((l <pair>))
    (print-summary l)
    (let loop ((i 0) (x l))
	(if (pair? x)
	    (begin
		(print.print1 #f i (car x))
		(loop (add1 i) (cdr x)))
	    (begin
		(if (not (null? x))
		    (print.print1 'rest i x))
		l))))

(define-method print ((l <empty-list>))
    (print-summary l)
    (display "    the empty list\n")
    '())

(define-method print ((v <vector>))
    (print-summary v)
    (let loop ((i 0))
	(if (< i (vector-length v))
	    (begin
		(print.print1 #f i (vector-ref v i))
		(loop (add1 i)))
	    v)))

(define (print-summary x)
    (display (object->bounded-string 70 x))
    (newline))
    
(define (print-gvec x)
  (for-each
   (lambda (index)
     (let* ((sd (slot-descriptor-for-class-slot (object-class x) index))
	    (slot-name (if sd
			   (name sd)
			   #f)))
       (print.print1 slot-name index (gvec-ref x index))))
   (range (gvec-length x)))
  (if (< (gvec-length x)
	 (instance-size (object-class x)))
      (begin
	(format #t "   WARNING: object has only ~d slots,\n" (gvec-length x))
	(format #t "            instance size of class ~s is ~d\n"
		(class-name (object-class x))
		(instance-size (object-class x)))))
  (values))

;; XXXX: XXXX XXXX XXXX XXXX  XXXX XXXX XXXX XXXX  | ................

(define $post-blk '#("" " " "" " " "" " " "" "  "
		     "" " " "" " " "" " " "" "  | "))

(define (print-bvec-line bvec index len ixfw)
    (let ((s (number->string index 16)))
	(format #t "~a~a: " (make-string (- ixfw (string-length s)) #\0)
			    s)
	(let loop ((i index) (n len))
	    (if (eq? n 0)
		(let loop ((i i) (n len))
		    (if (< n 16)
			(begin
			    (display "  ")
			    (display (vector-ref $post-blk (bitwise-and i 15)))
			    (loop (+ i 1) (+ n 1)))))
		(let ((byte (bvec-ref bvec i)))
		    (display (substring (number->string (+ byte 256) 16) 1))
		    (display (vector-ref $post-blk (bitwise-and i 15)))
		    (loop (+ i 1) (- n 1)))))
	(let loop ((i index) (n len))
	    (if (not (eq? n 0))
		(let ((byte (bvec-ref bvec i)))
		    (if (and (>= byte 32) (< byte 127))
			(write-char (integer->char byte))
			(write-char #\.))
		    (loop (+ i 1) (- n 1)))))
	(newline)))	    
    
(define (print-bvec x)
    (format #t "bvec ~s: ~d bytes\n" 
           (class-name (object-class x))
    	   (bvec-length x))
    (let* ((n (bvec-length x))
	  (ixfw (string-length (number->string n 16))))
	(if (< ixfw 4)
	    (set! ixfw 4))
	(let loop ((i 0))
	    (if (< (+ i 16) n)
		(begin
		    (print-bvec-line x i 16 ixfw)
		    (loop (+ i 16)))
		(print-bvec-line x i (- n i) ixfw)))))

(define-method print ((x <object>))
    (print-summary x)
    (cond
	((gvec? x)
	    (print-gvec x))
	((bvec? x)
	    (print-bvec x)))
    x)

(define-method print ((self <hash-table>))
    (print-summary self)
    ;; print the bucket occupation levels
    (let (((stats <vector>) (table-stats self)))
	(format #t "implementation: ~s, " (class-name (object-class self)))
	(format #t "directory: ~d bits\n" (directory-bits self))
	(format #t "~d chains, sizes: ~s\noccupancy: "
		(vector-ref stats 1)
		(map cdr (vector-ref stats 2)))
	(let (((n <fixnum>) 0))
	    (vector-for-each
		(lambda (e)
		    (if (eq? n 0)
			(display (cdr e))
			(if (eq? n 32)
			    (begin
				(format #t "\n           ~d" (cdr e))
				(set! n 0))
			    (format #t " ~d" (cdr e))))
		    (set! n (add1 n)))
		(vector-ref stats 0))
	    (newline)))
    ;; print the contents
    (table-for-each
	self
	(lambda (h k v)
	    (format #t "   ~s => ~s\n" k v)))
    self)
