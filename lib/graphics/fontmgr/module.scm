(define-module graphics.fontmgr ()
  (&module
   (import rs.lang
           iolib                        ; for file->string
           rs.util.properties
           paths
           rs.db.rstore
           rs.util.charset
           graphics.geometry
           graphics.afm
           rs.sys.tables
           regex)
   ;;
   (load "fontmgr.scm")
   (load "persist.scm")
   (load "query.scm")
   ;;
   (export init-font-database
           open-font-database
           open-font-database-for-update
           query-font-database
           commit
           ;;
           <font-entry>
           ;;
           current-font-database
           load-font)
   ;;
   (export <text-font>
           get-text-font
           font-shape
           font-size
           font-metrics
           font-glyph-names
           postscript-name
           font-family
           font-style
           font-glyph-names
           ;;
           set-font-encoding!
           set-font-outlines!
           has-font-outlines?
           get-outline
           string->outline)
   ;;
   (load "pdffont.scm")
   (export get-font-pdf-data)))

