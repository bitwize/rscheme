;;
;; configurable hooks
;;
;;  this file provides the default versions
;;
;;-----------------------------------------------------------
;;
;; return a procedure (<fs-absolute-path> <version>) => <list>
;; which returns a list of extra keys for keyword expansion
;;

(define (extra-keys-proc (fs <file-space>) req)
  (lambda ((path <fs-absolute-path>)
	   (v <node-version>))
    '()))
