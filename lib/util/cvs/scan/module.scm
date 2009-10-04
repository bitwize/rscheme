(define-module util.cvs.scan ()
  (&module
   (import usual-inlines
           tables
           sort
           paths
           regex
           calendar
           syscalls)
   ;;
   (load "scan.scm")
   (export scan-cvs-logs)
   ;;
   (load "branches.scm")
   (export revision-sequence
           chop-magic-zero
           branch-magic-revision?
           on-branch-recognizer
           previous-revision)
   ;;
   ))
