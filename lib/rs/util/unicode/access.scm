;;;
;;;  compile `unicode.txt' into a char-table
;;;

,(use tables rs.util.charset)

(define (load-unicode-database file)
  (let* ((tbl (make-char-table))
	 (lines (select (lambda (l)
			  (> (string-length l) 4))
			(string-split (file->string file) #\newline))))
    (members->char-table
     (map (lambda (l)
	    (cons (integer->char (string->number (substring l 0 4) 16)) l))
	  lines))))

(define *database* (load-unicode-database "unicode.txt"))

(define (get-unicode-database)
  (values))

(define (get-unicode-info ch)
  (let ((e (table-lookup (or *database* (get-unicode-database)) ch)))
    (if e
	(let ((fields (string-split e #\;)))
	  (values (cadr fields) (cddr fields)))
	(values))))

