(define-module srfi.18 ()
  (&module
   (import rs.lang)
   ;;
   (import rs.sys.threads.manager)
   ;;
   (load "mutex.scm")
   (load "threads.scm")
   ;
   (export
    mutex? make-mutex mutex-lock! mutex-unlock! with-mutex
    thread-start! thread-join! thread-terminate!
    )))
