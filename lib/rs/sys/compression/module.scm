(define-module rs.sys.compression ()
  (&module
   (import rs.lang)
   (import rs.glue)
   (import rs.sys.reflect)
   ;;
   (load "glue.scm")
   (load "zlib.scm")
   ;;
   (export uncompress
	   compress)))
