,(use regex)

(define parse-dimen (reg-expr->proc '(seq (save (seq (? #\-)
                                                     (+ (or #\/ digit #\.))))
                                          (* space)
                                          (save (* alpha)))))

(define *units-table* (make-string-table))
(table-insert! *units-table* "pt" '(pt 1))
(table-insert! *units-table* "in" '(in 72))
(table-insert! *units-table* "mm" '(mm 720/254))
(table-insert! *units-table* "cm" '(mm 7200/254))

(define (string->dimen str)
  (bind ((s e num un (parse-dimen str)))
    (if s
        (list (or (table-lookup *units-table* un)
                  (error "Unknown unit measure: ~s" un))
              (string->number num))
        (list (table-lookup *units-table* "pt")
              (or (string->number str)
                  (error "Cannot parse plain number: ~s" str))))))

(define (convert-dimen from-dimen #optional (to-units default: '(pt 1)))
  (* (cadr from-dimen) (/ (cadr (car from-dimen))
                          (cadr to-units))))
