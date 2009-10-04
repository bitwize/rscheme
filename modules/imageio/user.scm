#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/imageio/user.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.7
 | File mod date:    2007-01-28 10:02:16
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  imageio
 |
 `------------------------------------------------------------------------|#

;;
;;  high-level user interface
;;

(define (object->image* root (schema <image-schema>))
  (bind ((sections rewrite (pickle root))
	 (refstr sect0 (optimize-refs (vector-ref sections 0)
				      (symbol-dict schema)
				      (class-dict schema)
				      (ref-proc schema)
                                      (class-name-table schema)))
	 (info (image->compressed-string
		(vector sect0
			(vector-ref sections 1)         ; gvecs
			(vector-ref sections 2))        ; bvecs
		rewrite
		root))
	 (result (bvec-alloc <string> (+ 2  ;; 1 for version byte, 1 for NUL
					 (vector-ref info 0)
					 (string-length refstr)))))
    (bvec-set! result 0 #x9e)
    (bvec-copy result 1 
	       refstr 0 
	       (string-length refstr))
    (compact-buffers result
		     (+ (string-length refstr) 1)
		     (vector-ref info 1))
    result))

(define (check-image-version str)
  (let ((v (bvec-ref str 0)))
    (if (not (eq? v #x9e))
	(error 
	 "image->object: string does not appear to be an image\n==> ~#@*50s" 
	 str)))
  1)

;;
;;  `link-in?' determines whether or not function descriptors
;;  and code pointers will be bound into the currently executing
;;  program, or left as <anchor> instances
;;

(define (image->object* (str <string>) (schema <image-schema>))
  (bind ((refvec x (parse-refs str 
			       (check-image-version str)
			       (class-table schema)
			       (link-in? schema)
			       (class-dict schema)
			       (symbol-dict schema))))
    (unpickle (substring str x) refvec)))

;;
;;

(define (object->image root)
  (object->image* root (default-image-schema)))

(define (image->object str)
  (image->object* str (default-image-schema)))
