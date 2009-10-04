;;;

(define *default-scan-table* (make-scanner-table <input-port>))
(define *default-sharp-table* (make-sharp-table <input-port>))
(define *default-sharp-id-table* (make-table string-ci=? string-ci->hash))

(table-insert! *default-sharp-id-table* "t" '(#t))
(table-insert! *default-sharp-id-table* "f" '(#f))

(define-method scan-table ((self <input-port>))
  *default-scan-table*)

(define-method sharp-table ((self <input-port>))
  *default-sharp-table*)

(define-method sharp-id-table ((self <input-port>))
  *default-sharp-id-table*)

;; indirecting through `callit' is due to a bug in 0.7.1's codegen [CR 593]

(define (callit ent self ch loc)
  (ent self ch loc))

(define-method scan ((self <input-port>))
  (let* ((loc (location self))
	 (ch (input-port-read-char self)))
    (if (char? ch)
	(let ((ent (table-lookup (scan-table self) ch)))
	  (if ent
	      (callit ent self ch loc)
	      (signal (make <invalid-character>
			    source: self
			    character: ch
			    position: loc))))
	ch)))

