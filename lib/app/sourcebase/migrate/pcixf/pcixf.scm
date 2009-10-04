
;;  448 ==> fixed text
;;  456 ==> long text
;;  496 ==> integer


(define s "")

#|
(define s (file->string "/u/os2/tmp/files.x"))

(define (yank offset stride . n)
  (for-each
   (lambda (i)
     (let ((x (+ offset (* i stride))))
       (format #t "=============== [~d] (+~05x)\n" i x)
       (print (substring s x (+ x stride)))))
   (range (if (null? n)
	      5
	      (car n)))))
|#

;;("defectid" "userid" "action" "adddate" "remarks")

(define (get-field offset len)
  (substring s offset (+ offset len)))

(define (get-num offset len)
  (let ((n (string->number (get-field offset len))))
    (if n
	n
	(error "at +~d: ~s not a number" offset (get-field offset len)))))

;; (define $columns-meta-stride #x5a)
(define $columns-meta-offset #x95)

(define *next-row-offset* 0)

(define (fixed-width-fields x . parts)
  (let loop ((r '())
	     (x x)
	     (p parts))
    (if (null? p)
	(reverse r)
	(loop (cons (get-field x (car p)) r)
	      (+ x (car p))
	      (cdr p)))))

(define (columns)
  (let loop ((r '())
	     (x $columns-meta-offset)
	     (n (get-num #x70 5)))
    ;(format #t "~03x: ~s\n" x (get-field x 60))
    (if (eq? n 0)
	(reverse r)
	(loop (cons (fixed-width-fields x 6 1 2 18 1 1 1 1 3 5 5 5 3 6 29) r)
	      (+ x 90)
	      (- n 1)))))
#|
    (map (lambda (i)
	   (let* ((x (+ (* i $columns-meta-stride) $columns-meta-offset))
		  (len (get-num x 2)))
	     (set! *next-row-offset* (+ x $columns-meta-stride))
	     (list->string
	      (map char-downcase 
		   (string->list
		    (get-field (+ x 2) len))))))
	 (range n))))
|#
(define (get-bin-16 offset)
  (let ((v (get-field offset 2)))
    (values (+ (* (char->integer (string-ref v 1)) 256)
	       (* (char->integer (string-ref v 0)) 1))
	    (+ offset 2))))

(define (get-bin-32 offset)
  (let ((v (get-field offset 4)))
    (values (+ (* (char->integer (string-ref v 3)) 16777216)
	       (* (char->integer (string-ref v 2)) 65536)
	       (* (char->integer (string-ref v 1)) 256)
	       (* (char->integer (string-ref v 0)) 1))
	    (+ offset 4))))

(define (get-valid-flag offset)
  (let ((f (get-field offset 2)))
    (if (string=? f "\0\0")
	#t
	(if (string=? f "\377\377")
	    #f
	    (error "invalid null? flag: ~s" f)))))

(define (make-get-int col)
  (lambda '_int (cardx)
     ;(format #t "~04x+~02x: ~s\n" cardx col (get-field (+ cardx col) 20))
     (values (and (get-valid-flag (+ cardx col))
		  (get-bin-32 (+ cardx col 2)))
	     cardx)))

(define (make-get-var-field col)
  (lambda '_var-field (cardx)
     ;(format #t "~04x+~02x: ~s\n" cardx col (get-field (+ cardx col) 20))
    (let ((len (get-bin-16 (+ cardx col 2))))
      (values (and (get-valid-flag (+ cardx col))
		   (get-field (+ cardx col 4) len))
	      cardx))))

#|
(define (make-get-fixed-field width)
  (lambda 'get-fixed-field (offset)
    (let ((len (get-bin-16 (+ 2 offset))))
      (values (cons (get-valid-flag offset)
		    (get-field (+ 4 offset) len))
	      (+ offset len 4)))))
|#

(define (get-card-header offset)
  (values (fixed-width-fields offset 6 1 3 4)
	  (+ offset 14)))

(define (get-card-set offset num-cards)
  (let loop ((x offset)
	     (r '())
	     (n num-cards))
    (if (eq? n 0)
	(values (reverse r) x)
	(let ((len (get-num x 6)))
	  (loop (+ x len 6) (cons (get-field (+ x 6) len) r) (- n 1))))))

(define (get-all-cards)
  (let loop ((x 0)
	     (r '()))
    (if (< x (string-length s))
	(let ((len (get-num x 6)))
	  (loop (+ x len 6) (cons (get-field (+ x 6) len) r)))
	(reverse r))))

(define *current-card-offset* 0)
(define *current-card-length* 0)

(define (reset-card-iterator)
  (set! *current-card-offset* 0)
  (set! *current-card-length* (get-num 0 6)))

(define (current-card-type)
  (string-ref s (+ *current-card-offset* 6)))

(define (current-card-data)
  (+ *current-card-offset* 6))

(define (get-next-cardx)
  (set! *current-card-offset* (+ *current-card-offset* 
				 *current-card-length*
				 6))
  (if (< *current-card-offset* (string-length s))
      (begin
	(set! *current-card-length* (get-num *current-card-offset* 6))
	(+ *current-card-offset* 6))
      #f))

(define (find-first-card-type t)
  (reset-card-iterator)
  (let loop ()
    (if (eq? (current-card-type) t)
	(format #t "first ~s card at: +~x\n" t *current-card-offset*)
	(begin
	  (if (get-next-cardx)
	      (loop)
	      (begin
		(format #t "no ~s cards\n" t)
		#f))))))

(define (get-row first-card-x rdrs)
  (let loop ((m rdrs)
	     (r '())
	     (cardx first-card-x))
    (if (null? m)
	(values (reverse r) cardx)
	(if (car m)
	    ;; extract a field from the current card
	    (begin
	      ;(format #t "+~04x: ~s: ~s\n" cardx (car m) (get-field cardx 40))
	      (let ((v ((car m) cardx)))
		;(format #t " ==> ~s\n" v)
		(loop (cdr m) (cons v r) cardx)))
	    ;; get the next card
	    (loop (cdr m) r (get-next-cardx))))))


(define (make-meta columns)
  (let loop ((rdrs '())
	     (cl columns)
	     (prev-card 1))
    (if (null? cl)
	(reverse (cons #f rdrs))
	(let ((c (car cl)))
	  (let ((type (string->number (list-ref c 8)))
		(cardno (string->number (list-ref c 12)))
		(cardcol (+ 7 (string->number (list-ref c 13))))
		(width (string->number (list-ref c 11))))
	    ;;
	    (if (not (eq? cardno prev-card))
		;; install "read next card" marker
		(set! rdrs (cons #f rdrs)))
	    ;;
	    (loop (cons (case type
			  ((496) (make-get-int cardcol))
			  ((448) (make-get-var-field cardcol))
			  ((456) (make-get-var-field cardcol))
			  (else (error "unknown field type: ~s"
				       (list-ref c 8))))
		      rdrs)
		(cdr cl)
		cardno))))))

; 17,1,"open","95/08/14 16:46:11","incorporate files from regular file system"
; 4199,1,"open","95/08/14 20:42:16","CMVC does not preserve empty directories in a release...
#|
(define (row)
  (let* ((i *next-row-offset*)
	 (r (map (lambda (meta)
		   (let ((type (car meta))
			 (info (cadr meta)))
		     (case type
		       ((integer) 
			(let ((v (get-num i info)))
			  (set! i (+ i info))
			  v))
		       ((varchar)
			(let ((n (get-bin i)))
			  (set! i (+ i 4 info))
			  (get-field i n)))
		       ((longvar)
			(let ((len (get-bin 
			  (set! i 
|#
;"DEFECTID","NOTES","RSFAM   ",,"INTEGER ","Y",0,0,4,0,0,-1,"","",-1,,"INTEGER","SYSIBM  ",,4," "," ",-1,-1,,0
;"USERID","NOTES","RSFAM   ",,"INTEGER ","Y",0,0,4,0,1,-1,"","",-1,,"INTEGER","SYSIBM  ",,4," "," ",-1,-1,,0
;"ACTION","NOTES","RSFAM   ",,"VARCHAR ","Y",850,0,15,0,2,-1,"","",-1,,"VARCHAR","SYSIBM  ",,15," "," ",-1,-1,,850
;"ADDDATE","NOTES","RSFAM   ",,"VARCHAR ","Y",850,0,25,0,3,-1,"","",-1,,"VARCHAR","SYSIBM  ",,25," "," ",-1,-1,,850
;"REMARKS","NOTES","RSFAM   ",,"LONGVAR ","Y",850,0,32700,0,4,-1,"","",-1,,"LONG VARCHAR","SYSIBM  ",,32700," "," ",-1,-1,,850


