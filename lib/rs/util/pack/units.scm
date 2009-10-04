
(define-message-table rs.util.pack 416)

(define *packing-types* 
  (apply append
	 (map (lambda (z)
		(let ((k (car z)))
		  (list z
			(list (symbol->keyword
			       (symbol-append (keyword->symbol k) "/b"))
			      (cadr z)
			      (+ (caddr z) 2))
			(list (symbol->keyword
			       (symbol-append (keyword->symbol k) "/l"))
			      (cadr z)
			      (+ (caddr z) 4)))))
	      '((u8: 1 0)
		(s8: 1 1)
		(u16: 2 6)
		(s16: 2 7)
		(u32: 4 12)
		(s32: 4 13)
		(u64: 8 18)
		(s64: 8 19)
		(f32: 4 24)
		(f64: 8 30)))))


(define (get-packing-type kwd)
  (let ((a (assq kwd *packing-types*)))
    (if a
	a
	(em 401 "packing type `~s' not recognized" kwd))))

(define (packing-type-length t)
  (cadr t))

(define (packing-type-id t)
  (caddr t))

(define (compute-buffer-length pt-list)
  (reduce + 0 (map packing-type-length pt-list)))
