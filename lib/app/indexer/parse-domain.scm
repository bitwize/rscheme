
(define domain-name-pat 
  (reg-expr->proc
   '(save (seq (+ (seq (+ (or alpha digit #\-)) #\.))
	       ;; all TLDs have only letters, and at least 2
	       (seq alpha (+ alpha))))))

(define (parse-domain-names str)
  (matches-ci str domain-name-pat))
