(define-module rs.util.text.diff ()
  (&module
   (import usual-inlines)
   ;;
   (load "levenshtein.scm")
   ;;
   (export levenshtein-distance)))
