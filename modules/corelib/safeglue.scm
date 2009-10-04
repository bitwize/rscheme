#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/corelib/safeglue.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.11
 | File mod date:    1999-01-25 07:46:26
 | System build:     v0.7.3.4-b7u, 2007-05-30
 |
 | Purpose:          Provide the 'define-safe-glue' rewriter
 |------------------------------------------------------------------------|
 | Notes:
 |      Most of this functionality (except the definitions of builtin
 |      type handlers) has been moved into the compiler's `define-glue'
 `------------------------------------------------------------------------|#

(define-rewriter (define-safe-glue form)
  (define builtin-type-handlers
    '((<raw-int> (primitive)
		 ("int ~a" "basic_raw_int(~a)"))
      (<raw-float> (primitive)
		   ("double ~a" "basic_raw_float(~a)"))
      (<raw-bool> (primitive "OBJ_ISA_BOOLEAN" <boolean>)
		  ("rs_bool ~a" "truish(~a)"))
      (<fixnum> (primitive "OBJ_ISA_FIXNUM" <fixnum>))
      (<string> (primitive "STRING_P" <string>))
      (<ascii-char> (primitive "OBJ_ISA_ASCII_CHAR" <ascii-char>))
      (<<class>> (primitive "CLASS_P" <<class>>))
      (<allocation-area> (class-eq? <allocation-area>)
			 ("AllocArea *~a"
			  "(AllocArea *)PTR_TO_DATAPTR(~a)"))
      (<raw-ascii-char> (primitive "OBJ_ISA_ASCII_CHAR" <ascii-char>)
			("UINT_8 ~a" "ASCII_CHAR_VALUE(~a)"))
      (<raw-string> (primitive "STRING_P" <string>)
		    ("char *~a" "(char *)string_text(~a)"))))
  `(define-glue ,(cadr form)
     ,@(apply append
	      (map (lambda (th)
		     `(type-handler: ,th))
		   builtin-type-handlers))
     ,@(cddr form)))
