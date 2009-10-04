(define-module rs.io.scanner (&module)
  (&module
   (import usual-inlines)
   (import rs.util.charset)
   (import rs.io.textport)
   (import tables)
   ;;
   (load "patch.scm")

   (load "tokclasses.scm")

   (load "util.scm")
   (load "str2num.scm")

   (load "token.scm")
   (load "errors.scm")

   (load "scanners.scm")
   (load "scanstr.scm")
   (load "scancurly.scm")
   (load "scansharp.scm")
   (load "scanchar.scm")
   (load "scannum.scm")
   (load "initscan.scm")

;;   (load "hilight.scm")

   (load "scan.scm")
   (export scan)))
