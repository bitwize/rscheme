#|
(if (not (string? (getenv "DISPLAY")))
    (error "DISPLAY variable not set"))
|#

(define-module-extend rs.lang ()
  (&module (export $Pi)))

(load "module.scm")
;(load "dvm.scm")
,(use gui.app.dv)

(define main dv-main)

