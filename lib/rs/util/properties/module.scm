(define-module rs.util.properties ()
  (&module
   (import usual-inlines)
   (import rs.util.msgs)
   ;;
   (load "errors.scm")
   (load "default.scm")
   (load "using-assq.scm")
   (load "using-vassq.scm")
   (load "get.scm")
   (load "set.scm")
   ;;
   (export properties
           set-properties!
	   get-default-property    ;; override for OO default properties
           <property-not-defined>
           add-property!
           set-property!
           has-property?
           get-property
	   remove-property!
           call-with-properties-set)))

