#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/iolib/curlybrc.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.5
 | File mod date:    1998-12-02 10:52:30
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  iolib
 |
 | Purpose:          object support for curly-brace text {...}
 `------------------------------------------------------------------------|#

;;

(define-class <curly-braced> (<object>)
  (text type: <string>)
  (line-number init-value: #f)
  (input-port-name init-value: #f))

;;

(define-method write-object ((self <curly-braced>) port)
  (output-port-write-char port #\{)
  (write-string port (text self))
  (output-port-write-char port #\}))

(define-method display-object ((self <curly-braced>) port)
  (write-string port (text self)))

(define (string->c-text str)
  (make <curly-braced>
	text: str))
