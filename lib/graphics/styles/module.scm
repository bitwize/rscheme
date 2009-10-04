(define-module graphics.styles ()
  (&module
   (import usual-inlines
           tables
           rs.util.properties)
   ;;
   (load "styles.scm")
   (load "access.scm")
   (load "stylesheet.scm")
   ;;
   (export define-style-type
           define-style-definer
           declare-style-type
           <style>
           <<style>>
           ;;
           make-stylesheet
           call-with-stylesheet
           current-stylesheet
           ;;
           get-style           ; looks in current-stylesheet
           get-style-attribute
           get-style-attributes
           get-all-style-attributes
           style-compile
           style-apply         ; (style-apply dev x) == ((style-compile x) dev)
           ;;
           define-style
           make-style
           )))

