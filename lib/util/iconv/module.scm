(define-module util.iconv ()
  (&module
   (import usual-inlines
           ;rs.sys.threads.manager
           )
   (load "iconv.scm")
   ;;
   ;; low-level procedures
   ;;
   (export iconv-open
           iconv-close
           iconv-bytes)
   ;;
   ;; higher-level
   ;;
   (export unicode-string->utf8
           utf8->unicode-string)))

  
