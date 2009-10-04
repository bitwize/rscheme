#|------------------------------------------------------------*-Scheme-*--|
 | File:    rs/io/here-strings/module.scm
 |
 |          Copyright (C) 2003 Donovan Kolbly <donovan@rscheme.org>
 |          as part of the RScheme project, licensed for free use
 |
 | Version: 1.3
 | Date:    2005-02-18 15:58:18
 | Build:   v0.7.3.4-b7u, 2007-05-30
 |
 | Purpose: Allow "here" strings a la the shell to be scanned, e.g.,
 |
 |
 |          #"xxx
 |          this is some line
 |          this is another line
 |          #"xxx ; this is the end
 |
 `------------------------------------------------------------------------|#

(define-module rs.io.here-strings (&module)
  (&module
   (import usual-inlines)
   (load "phs.scm")))
