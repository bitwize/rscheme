
;;
;;  map the way we want to do things onto the way the corelib
;;  provides things
;;

(define (modules-list)
  (map (lambda (c-module-ptr)
	 (cons (get-c-module-descr c-module-ptr)
	       c-module-ptr))
       (c-module-list)))

(define (module->parts-list mp)
  (bind ((name parts (get-c-module-descr mp)))
    (map (lambda (part)
	   (bind ((part-name tag (get-c-part-descr part)))
	     (cons tag part)))
	 parts)))

(define (part->functions-list pp)
  (bind ((part-name tag fns (get-c-part-descr pp)))
    fns))

(define (function->monotones-list fp)
  (bind ((fn-name monotones (get-c-function-descr fp)))
    monotones))

