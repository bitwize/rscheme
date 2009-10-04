
(define-class <debugpoint> (<breakpoint>))


(define-method gen-wrapper-template ((self <debugpoint>))
  (mk-standalone
   (lambda (proc args)
     (let ((port (output-port self))
	   (bak (backup-template self)))
       ;;
       (notify-break self args "debug at")
       (initial-help port)
       ;;
       (debug)
       (apply-template bak proc args)))))

(define *first* #t)

(define (initial-help port)
  (if *first*
      (begin
        (set! *first* #f)
        (format port "Use \"c\" to continue\n")
        (format port " or \"r expr\" to return values instead\n"))))

(define (cmd-proc/debug envt args)
  (brkpt-install envt args <debugpoint> 'debug))

(%early-once-only
(define-command-proc (debug)
  cmd-proc/debug
  ((",(debug proc...)" "install debug breakpoints on procs")))
)
