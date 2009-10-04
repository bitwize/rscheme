(define *vec-tree-branching* 11)

(define (make-new-vec-tree (old-vec-tree <vector>) new-lines)
   (let ((tbl (make-table string=? string->hash)))
    ;;
    ;(format #t "============= from ===========\n")
    ;(dump old-vec-tree 0)
    ;;
    (let loop ((v old-vec-tree))
	(if (string? v)
	    (table-insert! tbl v v)
	    (vector-for-each loop v)))
    ;;
    (let ((new-lines (map (lambda (line)
    			     (or (table-lookup tbl line) line))
			  new-lines))
	  (starts-table (make-object-table)))
	;;
	(define (find-commonality node)
	    (if (vector? node)
		(begin
		    ;; find finer-grain commonality first
		    (vector-for-each find-commonality node)
		    (replace-common! new-lines node starts-table))))
	;;
	;(format #t "============= to ===========\n")
	;(dump (list->vector new-lines) 0)
	;;
	(fill-starts-table starts-table new-lines)
	;;
	;; find common subnodes
	;;
	(find-commonality old-vec-tree)
	;;
	;; break into little groups
	;;
	(make-vec-tree! new-lines))))

(define (fill-starts-table starts-table new-lines)
  (if (pair? new-lines)
      (begin
        (table-insert! starts-table 
		       (car new-lines) 
		       (cons new-lines
		             (or (table-lookup starts-table (car new-lines))
			         '())))
	(fill-starts-table starts-table (cdr new-lines)))))

(define (make-vec-tree! lst)
   (if (< (length lst) *vec-tree-branching*)
       (list->vector lst)
       (let ((r (list->vec-tree! lst *vec-tree-branching*)))
	   (make-vec-tree! (cons (list->vector lst) r)))))
	  
(define (list->vec-tree! lst left)
  (if (null? lst)
      '()
      (if (eq? left 0)
          (let ((r (cdr lst)))
	    (set-cdr! lst '())
	    (let ((more (list->vec-tree! r *vec-tree-branching*)))
	      (cons (list->vector r) more)))
	  (list->vec-tree! (cdr lst) (- left 1)))))

(define (dump node depth)
    (format #t "~a~04x_~04x: " 
	(make-string (* depth 3))
	(obj-high-bits node) 
	(obj-low-bits node))
   (if (vector? node)
       (begin
         (format #t "[~d elements]\n" (vector-length node))
	 (vector-for-each (lambda (sub) (dump sub (+ depth 1))) node))
       (format #t "~#@*40s\n" node)))

(define (unique-lines lines)
  (let ((tbl (make-table string=? string->hash)))
    (map (lambda (l)
           (or (table-lookup tbl l)
	       (begin
	         (table-insert! tbl l l)
		 l)))
	 lines)))

#|
(define (file->lines p)
    (string-split (file->string p) #\newline))

(define (time-it)
  (let* ((f0 (unique-lines (file->lines "file1.test")))
	 (f0n (length f0))
	 (f0t (make-vec-tree! f0))
         (f1 (file->lines "file2.test")))
    (let ((t1 (time)))
      (make-new-vec-tree f0t f1)
      (let ((dt (time-time (time) t1)))
        (format #t "~d/~d lines took ~a\n" f0n (length f1) dt)
	(/ (length f1) (interval->seconds dt))))))

(define (another-time)
  (let ((p (open-output-append-file "file1.test"))
        (t (time)))
    (format p "this is a test\n")
    (format p "what do you think of it?\n\n")
    (format p "the time is now... ~a\n" t)
    (for-each (lambda (i) (format p "      [~d] ==>\n" i)) (range 20))
    (close-output-port p))
  (let ((p (open-output-append-file "file2.test"))
        (t (time)))
    (format p "this is a test\n")
    (format p "what do you think of it?\n\n")
    (format p "the time is now... ~a\n" t)
    (for-each (lambda (i) (format p "      [~d] ==>\n" i)) (range 20))
    (close-output-port p)))
|#
    
(define (replace-common! lst vec starts-table)
  (if (eq? (vector-length vec) 0)
      (values)
      (let ((f (table-lookup starts-table (vector-ref vec 0))))
        (if f
	    (table-insert! starts-table vec (compute-new-starts f vec))))))

(define (compute-new-starts old-starts vec)
  (let loop ((s old-starts)
	     (r '()))
    (if (pair? s)
	(let* ((start (car s))
	       (r (if (pair? start)
		      (rest-being-eq vec start) 
		      #f)))
	  (if r
	      (begin
		(set-car! start vec)
		(zap-cars (cdr start) r)
		(set-cdr! start r)
		(loop (cdr s) (cons start r)))
	      (loop (cdr s) r)))
	r)))

(define (zap-cars p end)
  (if (not (eq? p end))
      (begin
        (set-car! p #f)
	(zap-cars (cdr p) end))))
	
(define (rest-being-eq (vec <vector>) (lst <pair>))
  (let ((n (vector-length vec)))
    (let loop (((i <fixnum>) 0)
	       ((l <pair>) lst))
      (if (eq? i n)
	  l
	  (begin
	    ;(format #t " at +~d: ~s vs ~s: " i (car l) (vector-ref vec i))
	    (if (eq? (car l) (vector-ref vec i))
		(let ((r (cdr l)))
		  ;(format #t "ok...\n")
		  (if (pair? r)
		      (loop (add1 i) r)
		      (if (eq? (add1 i) n)
			  '()
			  #f)))
		(begin
		  ;(format #t "nope\n")
		  #f)))))))
