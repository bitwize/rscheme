(define *state* #f)

(define (init-for-readline fd)
  (tcdrain fd (tcflush-flag 'drain))
  ;(fd-set-blocking fd #f)
  (set! *state* (terminal-get-attr fd))
  (terminal-set-attr fd (terminal-state-make-raw *state*))
  (lambda ()
    (if *state*
	(terminal-set-attr 0 *state*))
    (set! *state* #f)))

