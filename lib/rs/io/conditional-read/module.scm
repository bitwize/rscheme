;;;
;;;  implement something like Common Lisp's #+ / #-
;;;

(define-module rs.io.conditional-read ()
  (&module
   (import usual-inlines)
   (import rs.util.msgs)
   (load "feature.scm")
   (load "readit.scm")
   (export add-feature! remove-feature!)
   (export current-features call-with-features eval-feature)))
