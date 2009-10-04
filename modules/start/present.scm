#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/start/present.scm
 |
 |          Copyright (C)1997-2005 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.9
 | File mod date:    2005-02-24 16:22:11
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  start
 |
 | Purpose:          Presentation banners
 `------------------------------------------------------------------------|#

(define *program* "RScheme")

;; `*version*' is really filled in at link time
(define *version* "RScheme (v0.7.3.4-b7u, 2007-05-30)")

(define *license* "\nCopyright (C) 1995-2005 Donovan Kolbly <d.kolbly@rscheme.org>\nRScheme comes with ABSOLUTELY NO WARRANTY.\n")

(define (set-presentation! program version license)
  (set! *program* program)
  (set! *version* version)
  (set! *license* license)
  (values))

;; display the program name, version, and date,
;; and general license info
;;
;; we use the low-level stdio functions because
;; the high-level stuff hasn't been set up yet
;; (the high-level i/o library is set up by the initialization procs,
;; which at this point haven't been run yet)
;;
;; this is called by `start' if the system is not invoked in "quiet" mode

(define (greeting)
  (let ((s (stdout)))
    (fwrite/str s *version*)
    (if *license*
	(fwrite/str s *license*)
	(fwrite/str s "\n"))))
