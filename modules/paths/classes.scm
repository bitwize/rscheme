#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/paths/classes.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.4
 | File mod date:    1997-11-29 23:10:37
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  paths
 |
 | Purpose:          pathname class definitions
 `------------------------------------------------------------------------|#

(define-class <file-name> (<object>)
    file-directory
    filename
    extension)

(define-class <directory-name> (<object>)
    rooted-at
    steps)

(define-class <root-dir> (<object>)
    root-name
    expanded-name)

(define (root-dir? x)
    (instance? x <root-dir>))

(define-generic-function pathname->string)
