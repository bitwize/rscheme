
(define (atom->symbol (atom <fixnum>))
  (let ((tbl (atom-symbol-table (current-client))))
    (or (table-lookup tbl atom)
	(let ((n (symbol->string (atom-name (on-display (current-client))))))
	  (table-insert! tbl atom n)
	  n))))
