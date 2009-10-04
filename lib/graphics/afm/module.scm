(define-module graphics.afm ()
  (&module
   (import usual-inlines)
   (import paths tables regex syscalls)
   ;
   (import rs.util.charset)
   (import rs.util.properties)
   (import graphics.geometry)
   ;
   (load "loader.scm")
   (export <char-metrics> <afm>)
   (export load-afm get-font-definition)
   ;
   (load "finder.scm")
   (export get-afm push-afm-directory)
   ;
   (load "compute.scm")
   (export font-glyph-names             ; return list of all glyph names (symbols)
           font-characters)             ; return list of all character codes (chars)
   (export make-scaled-metrics string-x-deltas char-widths char-code-range
           string-width xshow-x-list
           get-char-metrics
           kerning-pairs-after
           ;;
           ;; operations on char metrics (or scaled char metrics)...
           ;;
           char-width
           char-height
           char-depth
           char-bbox)
   (export string-bbox)))
