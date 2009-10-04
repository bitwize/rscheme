(define-module rs.sys.threads.shell ()

(&module
 ;(import usual-inlines)
 (import primops *scheme* low-scheme objsys iolib mathlib tables high-scheme)
 (import paths mlink start)
 (import corelib syscalls repl editinp sort debugger)
 ;
 (import rs.sys.threads.manager syscalls tables))

(&module
 (load "pool.scm")
 (load "run.scm")
 (load "taskmgr.scm")
 (load "bg.scm") 
 (export bg)
 (load "thread-list.scm")
 (load "thread-debug.scm")
)

(&module
 (export start-with-thread
         synchronize-with-finalization
         display-thread-list)))
