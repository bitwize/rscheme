
;; c.f. <http://www.webcom.com/haahr/dylan/linearization-oopsla96.html#appendix-b>


(define (signature-cmp s1 s2)
  (let loop ((s1 s1)
	     (s2 s2))
    (cond
     ;;
     ((null? s1)
      (if (null? s2)
          0
          -1))                  ; (A) < (A B)
     ;;
     ((null? s2)
      1)                        ; (A B) > (A)
     ;;
     ((type=? (car s1) (car s2))
      (loop (cdr s1) (cdr s2)))
     ;;
     (else
      (if (type<=? (car s1) (car s2))
          -1
          1)))))

(define (insert-method method-list method)
  (let loop ((l method-list)
             (r '()))
    (if (null? l)
        (append method-list (list method))
        (case (signature-cmp (function-specializers method)
                             (function-specializers (car l)))
          ((0) 
           (append (reverse r) (list method) (cdr l)))
          ((-1)
           (append (reverse r) (list method) l))
          ((1)
           (loop (cdr l) (cons (car l) r)))))))

(define-method target-add-method ((self <multimethod-generic>)
				  (method <method>))
  (set-generic-function-methods! self (insert-method
                                       (generic-function-methods self)
                                       method)))

(define-method install-next-method-syntax ((self <multimethod-generic>) form)
  ;; we don't implement `next-method' for multimethods yet
  form)
