(define-module rs.sys.config ()
  (&module
   (import usual-inlines
           tables
           compiler)
   (load "config.scm")
   ;;
   (export 
    ;; boolean configuration items
    ;; (these are reflected in *globally-implemented* as config.*)
    config-enable!
    config-disable!
    config-enabled?
    ;; valued configuration items
    ;; (if a config is set, it is reflected in *globally-implemented* also)
    config-set!
    config-value
    )))

           
