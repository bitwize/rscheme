
(define (connect-to-x-display dpy)
  (bind ((dpy-host dpy-srvr dpy-scrn (parse-display-var dpy)))
    (dm 100 "opening display ~a:~d.~d" dpy-host dpy-srvr dpy-scrn)
    (let* ((cnxn (open-display dpy-host display: dpy-srvr))
	   (r (display-roots cnxn)))
      (if (and (>= dpy-scrn 0)
	       (< dpy-scrn (length r)))
	  (values cnxn (list-ref r dpy-scrn))
	  (em 191
	      "screen `~d' out of range (~d screens available on ~a:~d)"
	      dpy-scrn (length r) dpy-host dpy-srvr)))))

(define dpy-var-fmt2 (with-module regex (unformat->proc "~a:~d")))
(define dpy-var-fmt3 (with-module regex (unformat->proc "~a:~d.~d")))

(define (parse-display-var str)
  (define (fxhost s)
    (if (string=? s "")
	(with-module unixm (hostname))
	s))
  (bind ((host dpy scrn (dpy-var-fmt3 str)))
    (if host
	(values (fxhost host) dpy scrn)
	(bind ((host dpy (dpy-var-fmt2 str)))
	  (if host
	      (values (fxhost host) dpy 0)
	      (values (fxhost str) 0 0))))))
