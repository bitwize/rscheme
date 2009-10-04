;;;
;;;   Structure packing and unpacking
;;;
;;;   (pack-string type: value ...)                     ==> <string>
;;;   (pack type: value ...)                            ==> <byte-vector>
;;;
;;;   (make-packing-list '(type: ...))                  ==> <structure-descriptor>
;;;   (pack-using <structure-descriptor> <vector>)      ==> <byte-vector>
;;;
;;;   (with-unpack <expr> ([skip: <expr>] type: var ...) body...)      ==> body's value
;;;
;;;   where `type' is one of:
;;;        {u,s}{8,16}[/{b,l}]
;;;        {u,s,f}{32,64}[/{b,l}]
;;;
;;;     /b => big endian
;;;     /l => little endian
;;;
;;;     default is native byte order

(define-module rs.util.pack ()
  (&module
   (import usual-inlines)
   ;(import rs.lang)
   ;(import rs.glue)
   ;(import rs.sys.reflect)
   (import rs.util.msgs)
   ;
   (load "units.scm")
   (load "compile.scm")
   (load "pack.scm")
   (load "unpack.scm")
   (load "macros.scm")
   ;
   (export pack-using
	   unpack-using
	   pack pack-string
	   with-unpack
	   make-packing-list)))

