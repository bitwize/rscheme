#|------------------------------------------------------------*-Scheme-*--|
 | File:	    packages/syscalls/ident.scm
 |
 |          Copyright (C)1998 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |	    See <http://www.rscheme.org/> for the latest info.
 |
 | File version:     1.2
 | File mod date:    1998-02-12 19:22:17
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  syscalls
 |
 | Purpose:          provide useful identd (auth) access, and demonstate
 |		     the construction of simple internet protocol handlers
 |
 | See also:         RFC 1413
 `------------------------------------------------------------------------|#

(define rfc1413-response-parser #f)

(define (parse-rfc1413-response (str <string>))
  (if (not rfc1413-response-parser)
      (set! rfc1413-response-parser
	    (reg-expr->proc
	     '(seq (* (not #\:))
		   #\:
		   (* space)
		   (let resp-type (+ (not (or #\: space))))
		   (* space)
		   #\:
		   (* space)
		   (save (* (not #\cr)))))))
  (rfc1413-response-parser str))

(define (remote-port-owner (socket <fixnum>))
  (bind ((remote (getpeername/inet socket))
	 (local (getsockname/inet socket))
	 (remote-host remote-port (inet-socket-addr-parts remote))
	 (local-host local-port (inet-socket-addr-parts local))
	 (fd (inet-client remote-host 113))
	 (out (open-output-fd fd))
	 (in (open-input-fd fd)))
    (format out "~d,~d\r\n" remote-port local-port)
    (let ((rp (read-line in)))
      (fd-close fd)
      (if (string? rp)
	  (bind ((s e resp-type add-info (parse-rfc1413-response rp)))
	    (if s
		(values resp-type add-info)
		(values)))
	  (values)))))
