#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/editinp/readlin.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.6
 | File mod date:    1998-12-09 02:10:38
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  editinp
 |
 | Purpose:          Glue code interfacing to rdln choice (gnu|std)
 `------------------------------------------------------------------------|#

(define-glue (input-isa-tty?)
{
    REG0 = rb_to_bo(rdln_isa_tty());
    RETURN1();
})

(define-glue (readline-enabled?)
{
    REG0 = rb_to_bo(rdln_enabled());
    RETURN1();
})

(define-glue (readline-add-to-history str)
{
    rdln_add_history( str );
    RETURN0();
})

(define-glue (readline-read-line completions prompt)
{
    REG0 = read_console_line( completions, string_text(prompt) );
    RETURN1();
})

;;; editinp implementation using "native" readline

(define-class <readline-input-port> (<basic-edit-input-port>))

(define-method more-input-ready? ((self <readline-input-port>))
  #f)

(define-method provide-more-input ((self <readline-input-port>))
  (let ((line (readline-read-line 
	       (completions self)
	       (if (use-secondary? self)
		   (secondary-prompt self)
		   (primary-prompt self)))))
    (if line
	(begin
	  (readline-add-to-history line)
	  (string-append line "\n"))
	(begin
	  #f))))

(define-method open-edit-port-from ((self <std-input-port>) 
				    (peer <output-port>)
				    (errs <output-port>))
  (if (and (input-isa-tty?) 
	   (readline-enabled?)
	   (eq? self $standard-input-port))
      (values (make <readline-input-port>
		    underlying-input-port: self
		    underlying-output-port: peer)
	      peer
	      errs)
      (next-method)))

