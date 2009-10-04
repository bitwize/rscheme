
;; reads a level map
;; input is the file name
;; output is a NODE
;;  NODE ::= (name NODE...)
;;        |  (name id1 id2 type)

(define (parse-level-map file)
  (let ((root (list "")))
    (with-lines-from-file
     file
     (lambda (line)
       (bind ((path id1 id2 type (list->values (string-split line #\space))))
	 (let loop ((n root)
		    (s (string-split path #\/)))
	   (if (null? (cdr s))
	       (begin
		 (set-cdr! n (cons (list (car s)
					 (string->number id1)
					 (string->number id2)
					 (string->symbol type))
				   (cdr n)))
		 (values))
	       (let ((a (assoc (car s) (cdr n))))
		 (if a
		     (loop a (cdr s))
		     (let ((c (list (car s))))
		       (set-cdr! n (cons c (cdr n)))
		       (loop c (cdr s))))))))))
    root))

(define (level-map-dir? l)
  (not (fixnum? (cadr l))))
