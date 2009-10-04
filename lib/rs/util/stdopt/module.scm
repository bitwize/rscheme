(define-module rs.util.stdopt ()
  (&module
   (import usual-inlines syscalls)
   (load "getopt2.scm")
   (export <opt-spec> 
           ;getopt 
           ;options 
           ;non-options
           for-each-opt
           
           given
           spec
           <getopt-error>
           <getopt-unknown-option-error>
           <getopt-excess-value-error>
           <getopt-missing-value-error>)
   ;;
   (load "patterns.scm")
   (export print-usage
           dispatch-option-patterns
           english-join
           *program-name*)))

#|
(define-module rs.util.stdopt% ()
  (&module
   (import usual-inlines)
   (import tables)
   (import sort)
   ;
   ;(load "classes.scm")
   ;(load "abbrev.scm")
   ;(load "cmdlib.scm")
   ;(load "cmvcargs.scm")
   ;
   (load "abbrev.scm")
   (load "api.scm")
   
   (export
    define-stdopt-parser
    parse-stdopt-description
    run-stdop-parser)))

(define-module rs.util.stdopt ()
  (&module
   (import rs.util.stdopt%)
   (export define-stdopt-parser)))
|#
