(define-module rs.db.lss ()
  (&module
   (import rs.lang)
   (import rs.glue)
   ;;
   (load "glue.scm")
   (load "classes.scm")
   ;;
   (export lss-create
	   lss-open
	   lss-write
	   lss-read
	   lss-file
	   lss-close
	   lss-commit
           lss-tune
	   <lss>
	   <lss-error>)
   ;;
   (load "gc.scm")
   (export lss-copy-record
           lss-move-record
           lss-find-record-on
           lss-openx*           ; just for testing; we'll hide this later
           lss-set-tip
           lss-get-tip
           lss-attach-vol
           lss-detach-vol
           lss-get-generation
           lss-set-generation
           lss-get-vol-size
           lss-alloc-recs
           lss-delete)
   ;;
   (load "inspect.scm")
   (export lss-record-query 
           lss-record-info)
))
