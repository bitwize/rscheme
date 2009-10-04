(define-module util.pdf ()
  (&module
   (import usual-inlines
           sort
           tables
           regex
           objsys               ; for make-instance
           graphics.afm
           graphics.geometry
           graphics.fontmgr
           rs.util.properties)
   (load "parse-pdf.scm")
   (load "pdf-scan.scm")
   (load "pdf-read.scm")
   (load "update-pdf.scm")
   (export <pdf> trailer-block
           <pdf-object> owner
           <pdf-stream> dict contents make-dict
           ;;
           open-pdf
           ;;
           value
           dict-lookup
           catalog
           ;;
           dict-insert!
           alloc-object
           alloc-dict
           delete-object!
           set-value!
           flush-pdf
           output-to-pdf-stream
           make-pdf-stream
           open-pdf-output-port
           ;;
           pdf-scan
           pdf-read-eval)
   ;;
   (load "new-pdf.scm")
   (export create-pdf
           pdf-insert-page
           page-append-contents
           page-add-font-resource
           page-add-colorspace-resource
           page-add-pattern-resource
           page-assign-resource-id
           import-font-for-pdf
           pdf-import-font)))



