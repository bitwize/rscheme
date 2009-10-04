
(define-odbc-glue (apply-plan (stmt <hstmt>)
			      (plan <extraction-plan>))
  properties: ((other-c-files "extract.c")
	       (other-h-files "extract.h"))
{
  REG0 = run_odbc_extraction_plan( stmt, plan );
  RETURN1();
})

(define (result-metadata (stmt <hstmt>))
  (vector-map
   (lambda (i)
     (sql-describe-col stmt (+ i 1)))
   (list->vector (range (sql-num-result-cols stmt)))))

(define (bind-extraction-plan stmt (schema <extraction-plan-schema>))
  (let (((buf <byte-vector>) (clone (tuple-buffer schema)))
	(num-slots (bvec-length (instructions schema)))
	(tvec (vector #f
		      (sql-c-type 'default) ;; insn 1
		      (sql-c-type 'long)))) ;; insn 2
    (let loop ((slot 0)
	       (info-ix 0)
	       (cols (vector->list (result-columns schema))))
      ;(break-point)
      (if (< slot num-slots)
	  (let ((op (bvec-ref (instructions schema) slot)))
	    (if (eq? op 0)
		(loop (+ slot 1) info-ix cols)
		(let ((t (vector-ref tvec op))
		      (data-at (bvec-read-signed-32 buf info-ix))
		      (max-w (bvec-read-signed-32 buf (+ info-ix 4))))
		  (sql-bind-col stmt
				(car cols)
				t
				buf
				data-at
				max-w
				(+ info-ix 4))
		     (loop (+ slot 1)
			   (+ info-ix 8)
			   (cdr cols)))))
	  (make-gvec* <extraction-plan>
		      (properties schema)
		      (result-class schema)
		      buf
		      (instructions schema)
		      stmt
		      (vector->list (plan-literals schema)))))))

(define (query->iterator (dbc <hdbc>) str)
  (let ((stmt (sql-alloc-stmt dbc)))
    (sql-prepare stmt str)
       (sql-execute stmt)
       (let ((plan (bind-extraction-plan
		    stmt
		    (build-vector-plan
		     (result-metadata stmt))))
	     (n (sql-row-count stmt))
	     (i 0))
	 (values
	  (lambda ()
	    (if (< i n)
		(begin
		  (sql-fetch stmt)
		  (set! i (+ i 1))
		  (apply-plan stmt plan))
		#f))
	  n))))


(define (column-buffer-width (col <column>))
  (case (column-type col)
    ((unknown) (column-precision col))
    ((integer) 4)
    (else (error "not impl: ~s" (column-type col)))))

(define (make-schema-buffer (cols <vector>))
  (let* ((n (vector-length cols))
	 (plan-widths (vector-map column-buffer-width cols))
	 (data-len (reduce + 0 (vector->list plan-widths)))
	 (buf (bvec-alloc <byte-vector> (+ (* n 8) data-len))))
    ;;
    (do ((i 0 (+ i 1))
	 (info-ix 0 (+ info-ix 8))
	 (data-ix (* n 8) (+ data-ix (vector-ref plan-widths i))))
	((eq? i n))
      ;; offset
      (bvec-write-signed-32 buf info-ix data-ix) ;; offset
      ;; max width
      (bvec-write-signed-32 buf (+ info-ix 4) (vector-ref plan-widths i)))
    ;;
    buf))

(define (build-vector-plan (cols <vector>))
  (make <extraction-plan-schema>
    result-class: <vector>
    tuple-buffer: (make-schema-buffer cols)
    instructions: (list->u8vector
		   (vector->list
		    (vector-map
		     (lambda (col)
		       (case (column-type col)
			 ((unknown) 1) ;; use `default' C type
			 ((integer) 2) ;; use `long' C type
			 (else (error "234-oops: ~s" col))))
		     cols)))
    plan-literals: '#()
    result-columns: (vector-map column-number cols)))
