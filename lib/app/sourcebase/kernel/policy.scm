;;
;;   kernel functions for computing policies
;;
;; POLICY
;; 2a: a definite course or method of action selected from among 
;;     alternatives and in light of given conditions to guide and 
;;     determine present and future decisions

;;  does the policy for the given FS require reasons when
;;  making changes relative to the given GROUP?

(define (policy-require-reasons? (group <group>) (fs <file-system>))
   (let ((p (assq 'policy (properties fs))))
      (if p
          (if (assq 'require-reasons (cdr p))
	      #t
	      #f)
	  #f)))

(define (policy-want-snapshot? (group <group>) (fs <file-system>))
   (let ((p (assq 'policy (properties fs))))
      (if p
          (if (assq 'want-snapshot (cdr p))
	      #t
	      #f)
	  #f)))

(define (policy-check-off? (group <group>))
   (let ((p (assq 'policy (properties group))))
      (if p
          (if (assq 'defect-validate (cdr p))
	      #t
	      #f)
	  #f)))
