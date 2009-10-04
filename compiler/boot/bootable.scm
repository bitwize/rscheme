#|------------------------------------------------------------*-Scheme-*--|
 | File:    compiler/boot/bootable.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.5
 | File mod date:    2002-11-05 21:34:51
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  (rsc)
 |
 `------------------------------------------------------------------------|#

;;
;;  build the bootable image
;;
(mifio-class "<part-descr>" <part-descr>)

(load "bindboot.scm")

#| image modes...

  { /* 0: gvec */ 	LOAD1_ARRAY32,	LOAD2_GVEC,	  write_gvec },
  { /* 1: bvec */	LOAD1_ARRAY8,	LOAD2_CLASS_ONLY, write_array_8 },
  { /* 2: symbol */	LOAD1_SYMBOL,	LOAD2_NOP,	  write_symbol },
  { /* 3: part */	LOAD1_PART,	LOAD2_NOP,	  write_part },
  { /* 4: template */	LOAD1_ARRAY32,	LOAD2_TEMPLATE,	  write_template },
  { /* 5: part-cont */	LOAD1_ARRAY32,	LOAD2_PARTCONT,	  write_gvec },
  { /* 6: longfloat */	LOAD1_ARRAY64,	LOAD2_CLASS_ONLY, write_array_64 },
  { /* 7: uint32*n */	LOAD1_ARRAY32,	LOAD2_CLASS_ONLY, write_array_32 } };
|#

(define $hierarchy
  '(<object> :gvec 
	     (<vector> :gvec)
	     (<string> :bvec)
	                ;; <symbol>'s have image-mode: 0 during
	                ;; write-out to keep them from getting wrtn as sym's
	     (<symbol> :gvec)  #| image-mode: 2 |#
	     (<closure> :gvec)
	     (<<class>> :gvec)

	     (<char> :abstract
		     (<ascii-char> :immob)
		     (<unicode-char> :immob))
	     (<unique-obj> :immob)
	     (<fixnum> :immob)
	     (<double-float> :bvec image-mode: 6)
	     (<boolean> :immob)
	     (<byte-vector> :bvec)

	     (<binding-envt> :gvec)

	     (<template> :gvec image-mode: 4)
	     (<partial-continuation> :gvec image-mode: 5)
	     
	     (<top-level-var> :gvec)
	     (<top-level-contour> :gvec)
	     (<imported-binding> :gvec)

	     (<patch> :gvec)
	     (<part-descr> :gvec image-mode: 3)

	     (<module> :gvec)
	     (<binding> :abstract
			(<primop> :gvec)
			(<top-level-var> :gvec)
			(<definer> :gvec))
	     (<byte-coded> :bvec)

	     (<link-cmd> :abstract
			 (<link-xform> :gvec)
			 (<link-value> :gvec)
			 (<link-method> :gvec)
			 (<link-bdgs> :gvec))

	     (<imported-module> :gvec)

	     (<table-bucket> :gvec)
	     (<table> :abstract
		      (<symbol-table> :gvec)
		      (<string-table> :gvec)
		      (<string-ci-table> :gvec)
		      (<object-table> :gvec)
		      (<eq-table> :gvec))

	     (<list> :abstract 
		     (<pair> :gvec)
		     (<empty-list> :immob))))

;;

(define (write-bootable-image path)
  (display "building boot module...\n")
  (let ((m (build-boot-module *modules*)))
    (write-bootable-image-from-module m path)))

(define (write-bootable-image-from-module boot-module path)
  (display "building class hierachy...\n")
  (craft-classes (module-exports boot-module)
		 $hierarchy
		 #f)
  (display "building boot vector...\n")
  (let ((boot-vec (build-boot-vector boot-module)))
  
    (display "computing *builtin-modules*...\n")
    (build-builtin-modules! boot-vec boot-module)

    ;; reload the value of *version* into the boot vector
    (vector-set! boot-vec 1 (value (exported-binding boot-module '*version*)))

    (display "computing replacement bindings...\n")
    (let ((replace (compute-closers boot-module boot-vec)))

      (display "rebuilding symbol table...\n")
      (build-symbol-table! boot-vec replace)
      
      (display "saving image... ")
      (write path)
      (newline)
      (if (eq? *save-image-signature* 'new)
	  (save-image (pathname->string path)
		      boot-vec
		      '#()
		      '#()
		      replace)
	  (save-image (pathname->string path)
		      boot-vec
		      '#()
		      replace))
      (display "done saving image.\n")
      boot-vec)))
