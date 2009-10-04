;;;

(define-class <pg-table> (<table>)
  db-connection ;; can be a `delay' as well
  pg-table-name
  pg-key-field   ;; a field name or a list of field names
  pg-value-field ;; a field name or a list of field names
  (pg-cardinality init-value: 1)
  (pg-value-type init-value: #f))

(define-class <caching-pg-table> (<pg-table>)
  pg-cache
  (pg-hit-expiry type: <interval>)
  (pg-miss-expiry type: <interval>))

;;;

(define *default-hit-expiry* (seconds->interval 3600))
(define *default-miss-expiry* (seconds->interval 30))

(define (make-pg-table cnxn
		       (table <string>)
		       #key (key-field default: #f)
		            (key-fields default: #f)
		            (value-field default: #f)
			    (value-fields default: #f)
			    (value-type default: #f)
			    (cardinality default: 1)
			    (cache? default: #f)
			    (hit-expiry default: #f)
			    (miss-expiry default: #f))
  (let ((vf (or value-field
		value-fields
		(if value-type
		    (class-slot-names value-type)
		    (select
		     (lambda (col)
		       (not (string=? col key-field)))
		     (db-table-column-names cnxn table))))))
    (if (and (pair? vf)
	     (= (length vf) 1))
	(set! vf (car vf)))
    (if key-field (assert (string? key-field)))
    (if key-fields (assert (list? key-fields)))
    (if cache?
	(make <caching-pg-table>
	      db-connection: cnxn
	      pg-table-name: table
	      pg-key-field: (or key-field key-fields)
	      pg-value-type: value-type
	      pg-value-field: vf
	      pg-cardinality: cardinality
	      pg-cache: (make-table)
	      pg-hit-expiry: (or hit-expiry *default-hit-expiry*)
	      pg-miss-expiry: (or miss-expiry *default-miss-expiry*))
	(make <pg-table>
	      db-connection: cnxn
	      pg-table-name: table
	      pg-key-field: (or key-field key-fields)
	      pg-value-type: value-type
	      pg-value-field: vf
	      pg-cardinality: cardinality))))

;;;

(define (db-table-column-names (cnxn <pg-connection>) (table <string>))
  (pg-with-tuples
   cnxn
   (format #f "select * from ~a where 1=0" table)
   (lambda (result num-tuples num-fields)
     (map symbol->string
	  (map car
	       (result-metadata cnxn result num-fields))))))

(define (class-slot-names (c <<class>>))
  (map symbol->string (map name (slot-descriptors c))))

;;;

(define-method pg-marshall ((self <boolean>))
  (if self "true" "false"))

(define-method pg-marshall ((self <number>))
  self)

(define needs-esc
  (reg-expr->proc '(or #\\ #\' #\" (not (range #\space #\~)))))

(define-method pg-marshall ((self <string>))
  (let ((o (open-output-string)))
    (write-char #\' o)
    (let loop ((i 0))
      (if (= i (string-length self))
	  (begin
	    (write-char #\' o)
	    (close-output-port o))
	  (let ((s (needs-esc self i)))
	    (if s
		(begin
		  (write-string o (substring self i s))
		  (format o "\\~03o" (char->integer (string-ref self s)))
		  (loop (+ s 1)))
		(begin
		  (write-string o (substring self i))
		  (loop (string-length self)))))))))

(define-method pg-marshall ((self <symbol>))
  (pg-marshall (symbol->string self)))

;;; insert is kind of tricky...  for now, we only support
;;; the simplest case.  The tricky cases involve object values,
;;; partial columns, and cardinality != 1
;;;
;;; we also don't handle removing any old keys, so insert-as-update
;;; doesn't work quite right either

(define-method table-insert! ((self <pg-table>) key value)
  (let* ((fields (if (pair? (pg-value-field self))
		     (string-join "," (pg-value-field self))
		     (pg-value-field self)))
	  (q (format #f "insert into ~a (~a,~a) values(~a,~a)"
		     (pg-table-name self)
		     (pg-key-field self)
		     fields
		     (pg-marshall key)
		     (if (string? (pg-value-field self))
			 (pg-marshall value)
			 (string-join
			  ","
			  (map pg-marshall (vector->list value)))))))
    (pg-exec-command (force (db-connection self)) q)
    (values)))

(define (make-where-clause key-field key)
  (if (string? key-field)
      (format #f "~a = ~a" key-field (pg-marshall key))
      (string-join " and " (map make-where-clause key-field key))))

(define-method table-lookup ((self <pg-table>) key)
  (let* ((fields (if (pair? (pg-value-field self))
		     (string-join "," (pg-value-field self))
		     (pg-value-field self)))
	  (q (format #f "select ~a from ~a where ~a"
		     fields
		     (pg-table-name self)
		     (make-where-clause (pg-key-field self) key)))
	  (r (query->list (force (db-connection self))
			  q
			  class: (pg-value-type self))))
    (if (eq? (pg-cardinality self) 'n)
	r
	(if (pair? r)
	    (car r)
	    (values)))))

(define-method table-lookup ((self <caching-pg-table>) key)
  ;;
  (define (fill)
    (let ((r (next-method)))
      (table-insert! (pg-cache self) key (cons (time) r))
      r))
  ;;
  (let ((c (table-lookup (pg-cache self) key)))
    (if c
	(if (interval<? (time-time (time) (car c))
			(if (cdr c)
			    (pg-hit-expiry self)
			    (pg-miss-expiry self)))
	    (cdr c)
	    (fill))
	(fill))))

(define-method table-insert! ((self <caching-pg-table>) key value)
  (table-remove! (pg-cache self) key)
  (next-method))

(define-method clear-cache ((self <caching-pg-table>))
  (set-pg-cache! self (make-table))
  (values))

(define-method clear-cache ((self <pg-table>))
  (values))
