(define-module rs.sys.reflect ()
  (&module
   (import primops *scheme* iolib low-scheme objsys mathlib tables high-scheme)
   (import compiler codegen editinp paths mlink start sort)
   (import corelib)
   ;
   (export bvec?
	   bvec-length
	   bvec-ref
	   bvec-set!
	   bvec-alloc
	   bvec-copy
	   bvec-read-signed-8
	   bvec-read-signed-16
	   bvec-read-unsigned-8
	   bvec-read-unsigned-16
	   bvec-read-signed-32
	   bvec-read-float-32
	   bvec-read-float-64

	   bvec-write-signed-8
	   bvec-write-signed-16
	   bvec-write-unsigned-8
	   bvec-write-unsigned-16
	   bvec-write-signed-32
	   bvec-write-float-32
	   bvec-write-float-64)
   ;
   (export gvec?
	   gvec-length
	   gvec-ref
	   gvec-set!
	   gvec-alloc)))
