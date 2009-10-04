#|
,(use util.patterns)

(define-patterns 


(define-patterns packers
  ((pack (?type ?end ?chex) ?bvec ?offset ?value)
   (store ?type (type->host ?end (extract value))))
|#

(define (gen end chex type ctype)
  (let* ((endch (case end
		  ((native) #\n)
		  ((big-endian) #\b)
		  ((little-endian) #\l)))
	 (packer (format #f "bvec_pack~a_~a" endch type))
	 (unpacker (format #f "bvec_unpack~a_~a" endch type)))
    (format #t "void ~a( obj bvec, UINT_32 offset, obj value )\n" packer)
    (format #t "{\n")
    (format #t "  ~a v = basic_to_~a( value );\n" ctype type)
    (format #t "  BVEC_SLOT( bvec, offset, ~a ) = " ctype)
    (if (eq? end 'native)
	(format #t "v")
	(format #t "nto~a_~a( v )" endch type))
    (format #t ";\n")
    (format #t "}\n")
    ;;
    (format #t "obj ~a( obj bvec, UINT_32 offset )\n" unpacker)
    (format #t "{\n")
    (format #t "  ~a v = BVEC_SLOT( bvec, offset, ~a );\n" ctype ctype)
    (format #t "  return ~a_to_basic( " type)
    (if (eq? end 'native)
	(format #t "v")
	(format #t "~aton_~a( v )" endch type))
    (format #t " );\n" )
    (format #t "}\n")
    ;;
    (values)))

(define (run)
  (with-output-to-file
      "packers.ci"
    (lambda ()
      (for-each
       (lambda (t)
	 (let ((type-symbol (car t))
	       (ctype-name (cadr t)))
	   (gen 'native 'value type-symbol ctype-name)
	   (gen 'big-endian 'value type-symbol ctype-name)
	   (gen 'little-endian 'value type-symbol ctype-name)))
       '((u16 "UINT_16")
	 (s16 "INT_16")
	 (u32 "UINT_32")
	 (s32 "INT_32")
	 (u64 "UINT_64")
	 (s64 "INT_64")
	 (f32 "IEEE_32")
	 (f64 "IEEE_64"))))))



