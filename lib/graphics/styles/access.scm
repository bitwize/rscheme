(define (style-apply dev s #rest rest)
  ((apply style-compile s rest) dev))

;;;

(define (flatten-style-attributes (s <style>))
  (let ((tbl (if (basis s)
                 (flatten-style-attributes (get-style (basis s)))
                 (make-symbol-table)))
        ((p <vector>) (overrides s))
        ((n <fixnum>) (vector-length (overrides s))))
    (let loop (((i <fixnum>) 0))
      (if (eq? i n)
          tbl
          (begin
            (table-insert! tbl (vector-ref p i) (vector-ref p (add1 i)))
            (loop (fixnum+ i 2)))))))

(define-method get-style-attribute ((self <symbol>) attr)
  (get-style-attribute (get-style self) attr))

(define-method get-style-attribute ((self <style>) attr)
  (let ((k (vassq attr (overrides self))))
    (if k
        (vector-ref (overrides self) k)
        (if (basis self)
            (get-style-attribute (basis self) attr)
            (error "~s: no style attribute ~s" self attr)))))
      
  
(define (get-style-attributes style attr-list)
  (let ((tbl (flatten-style-attributes (get-style style))))
    (map (lambda (n)
           (if (table-key-present? tbl n)
               (table-lookup tbl n)
               (error "~s: no attribute ~s" style n)))
         attr-list)))

(define (get-all-style-attributes style)
  (let ((tbl (flatten-style-attributes (get-style style))))
    (map cons
         (key-sequence tbl)
         (value-sequence tbl))))
