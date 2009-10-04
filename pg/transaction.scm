
(define-thread-var *in-transactions* '())

(define (with-transaction (db <pg-connection>) thunk)
  (let ((t *in-transactions*))
    (if (memq db t)
	(thunk)
	(thread-let ((*in-transactions* (cons db t)))
	  (pg-exec-command db "BEGIN;")
	  (handler-case
	   (bind ((#rest r (thunk)))
	     (pg-exec-command db "END;")
	     (list->values r))
	   ((<condition> condition: c)
	    (pg-exec-command db "ABORT;")
	    ;; in case they want to start another transaction
	    ;; within the signal stack
	    (thread-let ((*in-transactions* t))
	      (signal c))))))))
