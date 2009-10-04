(define-module rs.util.subprocess ()
  (&module
   ;
   (import usual-inlines)
   (import tables sort unixm syscalls)
   (import rs.sys.threads.manager)
   ;
   (load "run.scm")
   (load "inpath.scm")
   ;
   (export port->run->port ;; fork subprocess with input & output
	   run/collecting*
	   )))
