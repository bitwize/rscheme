(define-module rs.net.md5 ()
  (&module (import usual-inlines))
  (&module
    (load "md5.scm")

    ;; low-level procedures
    (export make-md5-stream          ;; make an md5-stream
            make-md5-state           ;; make an md5 state
            update-md5-state         ;; roll in a 64-byte block of input
            )
    ;;
    ;; higher-level procedures
    ;;
    ;; this generic can slurp a <string> or an <input-port>
    (export md5-binary-digest)
    ;; uses above to produce an ascii string
    (export md5-digest)
    (export make-md5-accum
            accum->md5                  ; generic on, e.g., a <string>
            md5-finalize)
    ;;
    (export md5-prefix)))
