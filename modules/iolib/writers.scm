#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/iolib/writers.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.17
 | File mod date:    2004-02-24 09:32:16
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  iolib
 |
 | Purpose:          most methods on `write-object'
 `------------------------------------------------------------------------|#

#|
    

    This file defines the writer (and display, when they're different)
    methods for the builtin classes.
|#

;----------------- <Pair> -----------------

(define (reader-macro-print (pair <pair>))
    (let ((m (assq (car pair)
		   '((quote . "'")
		     (quasiquote . "`")
		     (unquote . ",")
		     (unquote-splicing . ",@")))))
	(if m
	    (let ((r (cdr pair)))
	      (if (pair? r)
		  (if (null? (cdr r))
		      (cdr m)
		      #f)
		  #f))
	    #f)))

(define-method write-object ((self <empty-list>) port)
  (write-string port "()"))

(define-method write-object ((pair <pair>) port)
    (let ((m (reader-macro-print pair)))
	(if m
	    (begin
		(write-string port m)
		(write-object (cadr pair) port))
	    (begin
		(output-port-write-char port #\()
		(write-object (car pair) port)
		(let loop ((x (cdr pair)))
		    (if (pair? x)
			(begin
			    (output-port-write-char port #\space)
			    (write-object (car x) port)
			    (loop (cdr x)))
			(if (not (null? x))
			    (begin
				(write-string port " . ")
				(write-object x port)))))
		(output-port-write-char port #\))))))

(define-method display-object ((pair <pair>) port)
    (let ((m (reader-macro-print pair)))
	(if m
	    (begin
		(write-string port m)
		(display-object (car (cdr pair)) port))
	    (begin
		(output-port-write-char port #\()
		(display-object (car pair) port)
		(let loop ((x (cdr pair)))
		    (if (pair? x)
			(begin
			    (output-port-write-char port #\space)
			    (display-object (car x) port)
			    (loop (cdr x)))
			(if (not (null? x))
			    (begin
				(write-string port " . ")
				(display-object x port)))))
		(output-port-write-char port #\))))))

;----------------- <String> -----------------

(define-glue (string->printable str str_index)
{
    /* stores it's results directly into REG0 and REG1 */
    printablize_string( str, str_index );
    RETURN(2);
})

(define-method write-object ((str <string>) port)
    (output-port-write-char port #\")
    (let loop ((i 0))
	(bind ((str-part next-i (string->printable str i)))
	    (write-string port str-part)
	    (if next-i
		(loop next-i))))
    (output-port-write-char port #\"))

(define-method display-object ((str <string>) port)
  (write-string port str))

#|
;----------------- <C-Text> -----------------

(define (write/c-text text port)
    (output-port-write-char port #\{)
    (write-string port (c-text->string text))
    (output-port-write-char port #\}))

(define (display/c-text text port)
    (write-string port (c-text->string text)))


;----------------- <FixNum> -----------------


;----------------- <Long-Float> -----------------
|#

;; numbers...

(define-method write-object ((self <number>) port)
  (write-string port (number->string self)))

;----------------- <Vector> -----------------

(define (write-a-vector (vec <vector>) port render-more)
    (write-string port "#(")
    (let (((len <fixnum>) (vector-length vec)))
	(if (fixnum>? len 0)
	    (begin
		(render-more (vector-ref vec 0) port)
		(let loop (((i <fixnum>) 1))
		    (if (fixnum<? i len)
			(begin
			    (output-port-write-char port #\space)
			    (render-more (gvec-ref vec i) port)
			    (loop (add1 i))))))))
    (output-port-write-char port #\)))

(define-method write-object ((self <vector>) port)
  (write-a-vector self port write-object))

(define-method display-object ((self <vector>) port)
  (write-a-vector self port display-object))

;----------------- <Symbol> -----------------

(define-method write-object ((self <symbol>) port)
    (write-string port (symbol->string self)))

#|
;----------------- <Token> -----------------

(define (write/token tok port)
    (write-string port "#[<Token> ")
    (let ((type (token-type tok)))
	(write-string port (symbol->string type))
	(if (memq type '(<literal> <symbol> <number> <object-ref>))
	    (begin
		(write-string port " data:")
		(write-object (token-data tok) port)))
	(write-string port "]")))
|#
;----------------- <ASCII-Char> -----------------

(define $ascii-char-names '#("nul" "soh" "stx" "etx" 
			     "eot" "enq" "ack" "bel" 
			     "bs" "tab" "newline" "vt" 
			     "np" "cr" "so" "si" 
			     "dle" "dc1" "dc2" "dc3" 
			     "dc4" "nak" "syn" "etb" 
			     "can" "em" "sub" "esc" 
			     "fs" "gs" "rs" "us" "space"))

(define-method write-object ((ch <ascii-char>) port)
    (write-string port "#\\")
    (let (((i <fixnum>) (char->integer ch)))
	(if (fixnum>=? i 128)
	    (begin
		(set! i (fixnum- i 128))
		(write-string port "M-")))
	(if (fixnum<? i 33)
	    (write-string port (gvec-ref $ascii-char-names i))
	    (if (eq? i 127)
		(write-string port "del")
		(output-port-write-char port (integer->ascii-char i))))))

(define-method display-object ((ch <ascii-char>) port)
  (output-port-write-char port ch))
    
;----------------- <Boolean> -----------------

(define-method write-object ((self <boolean>) port)
    (write-string port (if self "#t" "#f")))

;----------------- <<Class>> -----------------

(define-method write-object ((self <<class>>) port)
  (write-string port "#{the class ")
  (write-string port (symbol->string (name self)))
  (write-string port "}"))

;----------------- <generic-function> -----------------

(define-method write-object ((self <generic-function>) port)
  (write-string port "#[<GF> ")
  (write-string port (symbol->string (generic-function-name self)))
  (write-string port "]"))


;----------------- <template> -----------------

(define-method write-object ((self <template>) port)
  (format port "#[<template> ~j]" (template-place self)))

(define (template-place (t <template>))
  (let ((info (assq 'function-scope (gvec-ref t 2))))
    (if info
	(reverse (cdr info))
	'(unknown))))

;----------------- <closure> -----------------

(define-method name ((self <template>))
  (let ((o (open-output-string)))
    (format/j o (template-place self))
    (close-output-port o)))

(define-method name ((self <function>))
  (name (template self)))

(define-method name ((self <generic-function>))
  (generic-function-name self))

;;  

(define-method write-object ((self <closure>) port)
  (let ((s (assq 'source (function-descr (template self)))))
    (format port "#[<closure> ~a" (name self))
    (if (and (pair? s)
             (pair? (cdr s))
             (pair? (cddr s)))
        (format port " ~a:~a" (cadr s) (caddr s)))
    (format port "]")))

;----------------- <method> -----------------

(define-method write-object ((self <method>) port)
  (format port "#[<method> ~a]" (name self)))

(define-method write-object ((self <getter>) port)
  (format port "#[<getter> ~a ~a]" 
	  (name (slot-descriptor self))
	  (name (car (function-specializers self)))))

(define-method write-object ((self <setter>) port)
  (format port "#[<setter> ~a ~a]" 
	  (name (slot-descriptor self))
	  (name (car (function-specializers self)))))

#|
;----------------- <Partial-Continuation> -----------------

(define (write/part-cont part-cont port)
    (write-string port "#[<PartialCont> at ")
    (write-string port (addr->name (gvec-ref part-cont 1) 
			         (gvec-ref part-cont 4)))
    (write-string port "]"))


;----------------- <Template> -----------------
;----------------- <ByteVector> -----------------

(define (write/byte-vector bvec port)
    (let ((len (%bvec-length bvec)))
	(write-string port "#[")
	(display (class-name (object-class bvec)) port)
	(write-string port " ")
	(write-int port len)
	(write-string port " bytes:")
	(let loop ((i 0))
	    (if (eq? i len)
		(write-string port "]")
		(begin
		    (write-hex-w/pad (%bvec-read bvec i) port 2)
		    (loop (add1 i)))))))
	    
;----------------- <table> -----------------

(define (write/table table port)
    (write-string port "#[<table>")
    (let ((first #t))
	(table-for-each
	    table
	    (lambda (hash key value)
		(write-string port (if first " " ", "))
		(set! first #f)
		(write-object key port)
		(write-string port "->")
		(write-object value port)))
	(write-string port "]")))

(define (display/table table port)
    (write-string port "#[<table>")
    (let ((first #t))
	(table-for-each
	    table
	    (lambda (hash key value)
		(write-string port (if first " " ", "))
		(set! first #f)
		(display-object key port)
		(write-string port "->")
		(display-object value port)))
	(write-string port "]")))

;----------------- <InputPort> -----------------
;----------------- <OutputPort> -----------------
;----------------- <File-Output-Port> -----------------

(define (write/file-output-port p port)
    (write-string port "#[<File-Output-Port> ")
    (write/string (gvec-ref p 3) port)
    (output-port-write-char port #\]))

;----------------- <Bounded-String-Output-Port2> -----------------

(define (write/bsp p port)
    (write-string port "#[<Bounded-String-Port> ")
    (write-object (gvec-ref p 3) port)
    (output-port-write-char port #\/)
    (write-object (string-length (gvec-ref p 0)) port)
    (output-port-write-char port #\]))

;----------------- <String-Output-Port> -----------------

(define (write/sp p port)
    (write-string port "#[<String-Output-Port> ")
    (write-object (let loop ((i (gvec-ref p 1)) (overflow (gvec-ref p 2)))
    		(if (null? overflow)
		    i
		    (loop (+ i (%bvec-length (car overflow)))
		          (cdr overflow))))
	    port)
    (output-port-write-char port #\]))

;----------------- <Top-Level-Var> -----------------

(define (write/tlv tlv port)
    (write-string port "#[tlv ")
    (write-string port (symbol->string (gvec-ref tlv 0)))
    (if (not (eq? (gvec-ref tlv 1) '#unbound))
	(begin
	    (output-port-write-char port #\=)
	    (write-object (gvec-ref tlv 1) port)))
    (output-port-write-char port #\]))

;----------------- <Top-Level-Envt> -----------------
|#

;----------------- <directory-name> -----------------

(define-method write-object ((thing <directory-name>) port)
    (format port "#[<directory-name> ~a]" (pathname->string thing)))

(define-method display-object ((thing <directory-name>) port)
    (display (pathname->string thing) port))

;----------------- <file-name> -----------------


(define-method write-object ((self <file-name>) port)
    (format port "#[<file-name> ~a]" (pathname->string self)))
    
(define-method display-object ((thing <file-name>) port)
    (display (pathname->string thing) port))

;----------------- <root-dir> -----------------

(define-method write-object ((self <root-dir>) port)
    (format port "#[<root-dir> ~a]" (root-name self)))

(define-method display-object ((self <root-dir>) port)
  (write-string port (root-name self)))

;----------------- <winding-protect> -------------

(define-method write-object ((self <winding-protect>) port)
  (format port "#[<winding-protect> ~s]" 
	  (winding-protected self)))

;----------------- <dequeue> -----------------

(define-method write-object ((self <dequeue>) port)
  (format port "#[<dequeue> ~d: ~j]"
          (dequeue-count self)
          (vector->list (dequeue-state self))))

;----------------- simple conditions and errors -----------------

(define-method display-object ((self <simple-warning>) port)
  (write-string port "warning: ")
  (apply format port (simple-condition-msg self) (simple-condition-args self))
  (newline port))

(define-method display-object ((self <simple-error>) port)
  (write-string port "error: ")
  (apply format port (simple-condition-msg self) (simple-condition-args self))
  (newline port))

;----------------- default: -----------------

(define-method write-object ((self <object>) port)
  (write-string port "#[")
  (write-string port (symbol->string (name (object-class self))))
  (write-string port " ")
  (write-string port (to-string self))
  (write-string port "]"))

(define-method display-object ((self <object>) port)
  (write-string port (to-string self)))

