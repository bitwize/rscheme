
;; insert the macro expander into the framework

(with-module repl
  (with-module compiler
    (with-module tables
      (with-module mlink
	(let ((target (top-level-envt (get-module 'user))))
	  (for-each (lambda (new-name old-name)
		      (table-insert! (table target) new-name 
				     (table-lookup (table target) old-name)))
		    (list begin1 define1 quote1 lambda1 if1 set!1)
		    (list 'begin 'define 'quote 'lambda 'if 'set!))
	  (set-value!
	   (& expr->thunk)
	   (let ((e->t expr->thunk))
	     (lambda (expr envt)
	       (e->t (macro-expand expr) envt)))))))))
