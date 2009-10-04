#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/editinp/editinp.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.21
 | File mod date:    2005-05-26 09:15:53
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  editinp
 |
 | Purpose:          Friendly input, with prompts & all
 `------------------------------------------------------------------------|#

(define-class <edit-input-port> (<buffered-input-port>)
  (primary-prompt type: <string> init-value: "? ")
  (secondary-prompt type: <string> init-value: "")
  (completions type: <list> init-value: '())
  (use-secondary? type: <boolean> init-value: #f))

(define-class <basic-edit-input-port> (<edit-input-port>)
  (underlying-input-port type: <input-port>)
  (underlying-output-port type: <output-port>))

(define-method more-input-ready? ((self <basic-edit-input-port>))
  (input-port-char-ready? (underlying-input-port self)))

(define-method provide-more-input ((self <basic-edit-input-port>))
  (call-with-markup
   (underlying-output-port self)
   (if (use-secondary? self)
       '(edit-prompt secondary)
       '(edit-prompt primary))
   (lambda ()
     (write-string (underlying-output-port self)
                   (if (use-secondary? self)
                       (secondary-prompt self)
                       (primary-prompt self)))))
  (flush-output-port (underlying-output-port self))
  (let ((line (read-line (underlying-input-port self))))
    (if (string? line)
	(string-append line "\n")
	#f)))

(define-method input-port-scan-token ((p <edit-input-port>))
  (let ((c (input-port-read-char p)))
    ;; not exactly the behavior we want, but an approximation
    ;; (that is, we would like to use the primary prompt after
    ;; a comment, as so:
    ;;    top[0]=>;; this is a comment
    ;;    top[0]=>#| more
    ;;    comments
    ;;    here |#
    ;;    top[0]=>1
    ;;    value ::= 1
    ;; but I'm not sure how to do that; certainly not with the current
    ;; protocol)
    ;;
    ;; we can't even make this conditional on whitespace, because
    ;; the user may enter:
    ;;    top[0]=>    (cons 
    ;; in which case we have to give the secondary prompt!
    (set-use-secondary?! p #t)
    (if (eof-object? c)
	c
	(scanner-dispatch p c))))

;;;  the default implementation creates a "basic" edit input port

(define-method open-edit-port-from ((self <input-port>) 
				    (peer <output-port>)
				    (errs <output-port>))
  (values (make <basic-edit-input-port>
		underlying-input-port: self
		underlying-output-port: peer)
	  peer
	  errs))

;;; a NOP if it gets opened again

(define-method open-edit-port-from ((self <basic-edit-input-port>)
                                    (peer <output-port>)
                                    (errs <output-port>))
  (values self peer errs))

(define (with-edit-port inp out err thunk)
  (bind ((in out err (open-edit-port-from inp out err)))
    (thread-let ((*console-input-port* in)
		 (*console-output-port* out)
		 (*console-error-port* err)
		 (*input-port* in)
		 (*output-port* out)
		 (*error-port* err))
      (thunk))))
