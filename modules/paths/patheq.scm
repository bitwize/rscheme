#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/paths/patheq.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.4
 | File mod date:    1997-11-29 23:10:37
 | System build:     v0.7.3.4-b7u, 2007-05-30
 |
 | Purpose:          `path=?' implementation
 `------------------------------------------------------------------------|#

;; rather crude for now...
;;
;; fails sometimes, too
;;
;;  e.g.,
;;     (equal? (string->file "[resource]/../foo.data")
;;             (string->file "[install]/foo.data"))     ==> #f
;;

(define-method equal? ((x <file-name>) (y <file-name>))
    (string=? (pathname->os-path x) (pathname->os-path y)))

(define-method equal? ((x <directory-name>) (y <directory-name>))
    (string=? (pathname->os-path x) (pathname->os-path y)))

