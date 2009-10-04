(define-module rs.util.memoize ()
  (&module
   (import usual-inlines
           tables
           syscalls)
   (load "memoize.scm")
   ;;
   (export unmemoize
           memoize
           memoize-file-accessor                ; first arg is a file name
           ;;
           clear-all-memoizations
           disable-memoization
           list-memoizations
           ;;
           make-memoized
           make-memoized-file-accessor
           add-memo-table
           )))

