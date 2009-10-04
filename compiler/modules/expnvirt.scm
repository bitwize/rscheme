#|------------------------------------------------------------*-Scheme-*--|
 | File:    compiler/modules/expnvirt.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.2
 | File mod date:    1997-11-29 23:10:28
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  (rsc)
 |
 `------------------------------------------------------------------------|#


;; install a hook in the rs-0.6 paths module

(set! other-special-root
      (lambda (str)
	(if (string=? str "[dist]")
	    $dist-root
	    #f)))

(define $dist-root (make <root-dir> 
			 root-name: "[dist]/"
			 expanded-name: (pathname->string *dist-path*)))
