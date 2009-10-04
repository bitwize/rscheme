
(define-module-extend mathlib ()

(define *rational* (with-module regex
                     (reg-expr->proc '(entire
                                       (seq (save (seq (? #\-) (+ digit)))
                                            #\/
                                            (save (+ digit)))))))

(define *rational-plus* (with-module regex
                          (reg-expr->proc '(entire
                                            (seq (save (seq (? #\-) (+ digit)))
                                                 #\+
                                                 (save (+ digit))
                                                 #\/
                                                 (save (+ digit)))))))


(define (poor-mans-rational str base)
  (if (= base 10)
      (bind ((s e whole num den (*rational-plus* str)))
        (if s
            (let ((whole-part (string->number whole))
                  (frac-part (/ (string->number num) (string->number den))))
              (if (< whole-part 0)
                  (- whole-part frac-part)
                  (+ whole-part frac-part)))
            (bind ((s e n d (*rational* str)))
              (and s
                   (/ (string->number n) (string->number d))))))
      #f))

(set! *number-parsers* 
      (append! *number-parsers* 
               (list poor-mans-rational))))
