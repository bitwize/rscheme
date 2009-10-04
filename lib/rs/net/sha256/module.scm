#|------------------------------------------------------------*-Scheme-*--|
 | File:    %p%
 |
 |          Copyright (C) 2005 Donovan Kolbly <donovan@rscheme.org>
 |          as part of the RScheme project, licensed for free use
 |
 | Version: %I%
 | Date:    %E% %U%
 | Build:   %b%
 |
 | Purpose: Module definition for SHA-256 hash algorithm (rs.net.sha256)
 |
 `------------------------------------------------------------------------|#

(define-module rs.net.sha256 ()
  (&module (import usual-inlines))
  (&module
   (load "digest.scm")
   ;;
   (export sha256-binary-digest 
           sha256-digest)))
