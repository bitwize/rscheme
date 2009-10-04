(define-module rs.util.bits ()
  (&module
   (import usual-inlines)
   (load "bitrun.scm")
   ;
   (export num-low-bits-set num-low-bits-clear
	   num-high-bits-set num-high-bits-clear
	   set-low-bits
	   set-high-bits
	   bit-run-mask
	   find-clear-bit
	   bit-runs
	   find-middle-bits
	   find-long-bit-run)
   ;
   (load "slices.scm")
   (export bit-slice set-bit-slice!)))

