#|------------------------------------------------------------*-Scheme-*--|
 | File:    util/ssax/module.scm
 |
 |          Copyright (C) 2003 Donovan Kolbly <donovan@rscheme.org>
 |          as part of the RScheme project, licensed for free use
 |
 | Version: 1.3
 | Date:    2005-02-18 15:58:33
 | Build:   v0.7.3.4-b7u, 2007-05-30
 |
 | Purpose: Low-level XML support layer
 |
 |          Requires SSAX, which should be obtained using:
 |
 |            cvs -d :pserver:anonymous@cvs.sourceforge.net:/cvsroot/ssax \
 |                co SSAX/lib
 |
 |          from this directory (.../util/ssax)
 |
 |          The "util.xml" module comprises a high-level interface to
 |          the functionality exported from this module
 |
 `------------------------------------------------------------------------|#

(define-module util.ssax ()
  (&module

   ;;  SSAX redefines many standard RScheme names; if the bindings
   ;;  are shared (i.e., the name in our namespace points to the
   ;;  same <top-level-var> object as the name in the standard RScheme
   ;;  namespace), then the rest of the system will get very confused.
   ;;  Hence, we import the standard RScheme stuff as "unshared" which
   ;;  clones the <top-level-var> objects

   (import :no-shared-bindings user)
   (import tables)

   (load "myenv-rs.scm")
   (load "SSAX/lib/srfi-13-local.scm")
   (load "SSAX/lib/util.scm")
   (load "SSAX/lib/look-for-str.scm")
   (load "SSAX/lib/input-parse.scm")
   (load "SSAX/lib/char-encoding.scm")
   (load "SSAX/lib/SSAX.scm")

   (export ssax:read-char-data
           ssax:read-external-id
           ssax:read-char-ref
           ssax:Prefix-XML
           ssax:make-elem-parser
           ssax:make-parser
           ssax:skip-internal-dtd
           ssax:read-pi-body-as-string
           ssax:read-markup-token
           ssax:handle-parsed-entity
           ssax:read-QName
           ssax:skip-pi
           ssax:uri-string->symbol
           ssax:assert-token
           ssax:warn
           ssax:S-chars
           ssax:complete-start-tag
           ssax:read-attributes
           ssax:predefined-parsed-entities
           ssax:reverse-collect-str
           ssax:reverse-collect-str-drop-ws
           ssax:read-NCName
           ssax:skip-S
           ssax:ncname-starting-char?
           ssax:largest-unres-name
           ssax:scan-Misc
           ssax:resolve-name
           ssax:make-pi-parser
           ssax:read-cdata-body
           ssax:xml->sxml)

   (load "rscheme.scm")
   (export ssax:port->sxml)
))
