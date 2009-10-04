#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/paths/rootpath.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.5
 | File mod date:    1997-11-29 23:10:37
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  paths
 |
 | Purpose:          Manage the roots of absolute pathnames
 `------------------------------------------------------------------------|#

(define $system-root #f)    ;; /
(define $user-root #f)      ;; ~
(define $install-root #f)   ;; [install]
(define $resource-root #f)  ;; [resource]

(%early-once-only
 (set! $system-root (make <root-dir> 
			  root-name: "/"
			  expanded-name: "/"))
 (set! $user-root (make <root-dir> 
			root-name: "~/"
			expanded-name: "~/"))
 (set! $install-root (make <root-dir> 
			   root-name: "[install]/"
			   expanded-name: "/usr/local/rs/"))
 (set! $resource-root (make <root-dir> 
			    root-name: "[resource]/"
			    expanded-name: "/usr/local/rs/resource/")))

(let ((h (getenv "HOME")))
  (if h
      (set-expanded-name! $user-root (string-append h "/"))))

(let ((inst (rscheme-global-ref 29)))
  (set-expanded-name! $install-root (string-append inst "/"))
  (set-expanded-name! $resource-root (string-append inst "/resource/")))

(define-method pathname->string ((self <root-dir>))
    (root-name self))

;;

(define *special-roots* '())

(define (set-special-roots! lst)
  (set! *special-roots* lst))

(define (add-special-root! (entry <string>) (r <root-dir>))
  (set! *special-roots* (cons (cons entry r) *special-roots*)))

(%early-once-only
 (add-special-root! "[resource]" $resource-root)
 (add-special-root! "[install]" $install-root)
 (add-special-root! "~" $user-root)
 (add-special-root! "" $system-root))

(define (special-root (s <string>))
  (let ((a (assoc s *special-roots*)))
    (if a
	(cdr a)
	#f)))
