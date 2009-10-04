
;;
;; returns a thunk which is an iterator over the procedure calling
;; it's argument
;;
#|
  e.g.,

    (call-to-make-iterator (lambda (item) (map item '(1 2 3))))
          ==> #[<procedure>]  (`iter')

    (iter)  ==> 1
    (iter)  ==> 2
    (iter)  ==> 3
    (iter)  ==> #f
    (iter)  ==> #f
                ...
|#

(define call/cc call-with-current-continuation)

(define (proc->iterator proc)
  (let ((proc-routine (call/cc
		       (lambda (exit)
			 (let ((mainline (call/cc
					  (lambda (state)
					    (exit state)))))
			   ;;------------
			   ;; co-routine
			   ;;------------
			   (proc (lambda (item)
				   (set! mainline 
					 (call/cc 
					  (lambda (here)
					    (mainline here item))))))
			   (mainline #f #f))))))
    (lambda ()
      (if proc-routine
	  (bind ((nxt val (call/cc proc-routine)))
	    (set! proc-routine nxt)
	    val)
	  #f))))

