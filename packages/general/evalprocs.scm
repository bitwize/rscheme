;;
;;  Fix ourself up to create properly bound versions
;;  of `load', `*self*', and `eval' whenever this module
;;  (rs.lang.eval) is imported into another module, thereby
;;  completing the proper setup of a new module.
(define (make-eval-bdg tle)
  (make <top-level-var>
	name: 'eval
	value: (lambda 'eval (s-expr ; #optional (envt default: tle)
			      . opt)
		       (eval-in-envt s-expr (if (null? opt) tle (car opt))))))
;;
(define (make-self-bdg tle)
  (make <top-level-var>
	name: '*self*
	value: tle))
;; `make-load-bdg' is already in repl

;;
;;  --NOTE-- since this code is executed at bootstrap time,
;;  modules built using the offline compiler cannot
;;  use *self*, load, and eval from `rs.lang.eval'.
;;
;;  they can still use the two-arg form, however
;;

(define (eval s-expr envt)
  (eval-in-envt s-expr envt))

;;
(%early-once-only
 ;
 (display "Initializing rs.lang.eval...\n")
 ;
 (let ((m (get-module 'rs.lang.eval)))
   (set-usage-hooks!
    m
    (list 
     (lambda (tle)
       (bind! tle (make-self-bdg tle))
       (bind! tle (make-load-bdg tle))
       (bind! tle (make-eval-bdg tle)))))))


