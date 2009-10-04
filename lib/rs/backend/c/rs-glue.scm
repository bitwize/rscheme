
;;;
;;;  build the `rs.glue' module and put our `define-glue' compiler
;;;  in it
;;;

(define *rs.glue* (module ()))
(set-name! *rs.glue* 'rs.glue)
(define *define-glue* (with-module repl
			(make <definer>
			      name: 'define-glue
			      compiler-proc: compile-tl-define-glue
			      compiler-description: 'define-glue)))

(bind! (top-level-envt *rs.glue*) *define-glue*)
(bind! (top-level-envt *rs.glue*) *pragma*)

(table-insert! (module-exports *rs.glue*) 'define-glue *define-glue*)
(table-insert! (module-exports *rs.glue*) 'pragma *pragma*)
(install-module! (name *rs.glue*) *rs.glue*)
