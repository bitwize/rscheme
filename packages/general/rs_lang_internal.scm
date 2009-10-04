(define-module rs.lang.internal ()
  (&module
   (import primops *scheme* iolib low-scheme objsys mathlib tables high-scheme)
   (import compiler codegen editinp paths mlink start sort)
   (import corelib)
   ;
   (export clone
	   clone2
	   <template>
	   <closure>
	   <allocation-area>
	   <byte-coded>
	   make-object-table
	   <hash-integer-table>
	   <integer-table>
	   <eq-table>
	   <generic-table>
	   <table-bucket>
	   <root-dir>)
   (export <slot-descriptor>
	   class-compute-slots
	   instance-size
	   init-value
	   initialization-mode
	   <initializer-missing>)))

