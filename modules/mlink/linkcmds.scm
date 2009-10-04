#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/mlink/linkcmds.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.4
 | File mod date:    1997-11-29 23:10:31
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  mlink
 |
 | Purpose:          link-time command implementations
 `------------------------------------------------------------------------|#

;
;  <link-cmd> objects are used to control linking of modules
;  each link-cmd instance is associated with a particular
;  imported module.  When this module is finally bound to
;  the imported module, the generic function "execute-link-cmd"
;  is invoked on all of the <imported-module>'s link cmds.
;
;  note that when executing a module's link-commands list, 
;  the <link-bdgs>, if any (but there will be at most one), 
;  is done first, followed by any <link-value>'s.  Then
;  the other link-commands are executed, in the order they
;  were introduced

(define-class <link-cmd> (<object>)
  owner)  ;; the <imported-module> for which we are a link-command

;
; <link-bdgs> are the main kind of linking.
; a link-bdgs maintains a table mapping imported symbols
; (under the export names) to patch vectors in this heap
; image.  There is a <patch> per symbol, and it is of local objects
; which need to have a pointer to the imported binding.

(define-class <link-bdgs> (<link-cmd>)
    (imported-bindings type: <table>))

; <link-xform> is a class associated with a transformation
; of some other patch

(define-class <link-xform> (<link-cmd>)
  src-patch
  patch
  operation)


;
; <link-method> is the other significant kind of linking,
; and allows methods to be linked together from different
; modules into one generic function
;

(define-class <link-method> (<link-cmd>)
  gf-bdg      ;; this pointer will be patched
  (methods init-value: '() type: <list>))

;
; <link-value> is the third main kind of linking, and is
; pretty much the same as a link-bdgs, except the pointer
; is to the VALUE of a binding, as may be used to point
; to a class object in another module.

(define-class <link-value> (<link-cmd>)
  binding                ;; this pointer will be patched
  patch)

(mifio-class "<link-bdgs>" <link-bdgs>)
(mifio-class "<link-method>" <link-method>)
(mifio-class "<link-value>" <link-value>)
(mifio-class "<link-xform>" <link-xform>)
