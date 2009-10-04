(define-module rs.sys.linker ()
  (&module
   (import primops *scheme* iolib low-scheme objsys mathlib tables high-scheme)
   (import compiler codegen editinp paths mlink start sort)
   (import corelib)
   ;; these are pretty unsafe procedures as it currently stands
   ;; expect SEGVs until your code is debugged!
   (export find-linked-module
	   find-part-in-linked-module
	   find-code-ptr-in-part)))
	   