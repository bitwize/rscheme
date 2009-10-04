#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/lowscm/chars.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.6
 | File mod date:    1997-11-29 23:10:37
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  low-scheme
 |
 | Purpose:          general character operations
 `------------------------------------------------------------------------|#

; ** CHARACTERS **

(define (char? x)
  (or (ascii-char? x)
      (unicode-char? x)))

(define (char->integer (ch <char>))
  (if (ascii-char? ch)
      (ascii-char->integer ch)
      (if (unicode-char? ch)
	  (unicode-char->integer ch)
	  (error "cannot convert character to integer: ~s" ch))))

(define (integer->char (int <fixnum>))
  (if (fixnum>=? int 0)
      (if (fixnum<? int 256)
	  (integer->ascii-char int)
	  (if (fixnum<? int 65536)
	      (integer->unicode-char int)
	      (range-error integer->char 0 0 65536 int)))
      (range-error integer->char 0 0 65536 int)))

(define-syntax (char-cmp cmp ch1 ch2)
  (let (((a <fixnum>) (char->integer ch1))
	((b <fixnum>) (char->integer ch2)))
    (cmp a b)))

(define (char<? ch1 ch2)  (char-cmp fixnum<? ch1 ch2))
(define (char=? ch1 ch2)  (char-cmp eq? ch1 ch2))
(define (char<=? ch1 ch2)  (char-cmp fixnum<=? ch1 ch2))
(define (char>? ch1 ch2)  (char-cmp fixnum>? ch1 ch2))
(define (char>=? ch1 ch2)  (char-cmp fixnum>=? ch1 ch2))

(define-syntax (char-code-to-upper code)
  (if (and (fixnum>=? code 97)
	   (fixnum<=? code 122))
      (fixnum- code 32)
      code))

(define-syntax (char-ci-cmp cmp ch1 ch2)
  (let (((a <fixnum>) (char->integer ch1))
	((b <fixnum>) (char->integer ch2)))
    (cmp (char-code-to-upper a) 
	 (char-code-to-upper b))))

(define (char-ci=? ch1 ch2)  (char-ci-cmp eq? ch1 ch2))
(define (char-ci<? ch1 ch2)  (char-ci-cmp fixnum<? ch1 ch2))
(define (char-ci<=? ch1 ch2)  (char-ci-cmp fixnum<=? ch1 ch2))
(define (char-ci>? ch1 ch2)  (char-ci-cmp fixnum>? ch1 ch2))
(define (char-ci>=? ch1 ch2)  (char-ci-cmp fixnum>=? ch1 ch2))

(define (char-upcase (ch <char>))
  (let (((i <fixnum>) (char->integer ch)))
    (integer->char (char-code-to-upper i))))

(define (char-downcase (ch <ascii-char>))
  (let (((i <fixnum>) (char->integer ch)))
    (if (and (fixnum>=? i 65) (fixnum<=? i 90))
	(integer->ascii-char (fixnum+ i 32))
	ch)))

(define-glue (char-alphabetic? ch)
{
   REG0 = rb_to_bo(OBJ_ISA_ASCII_CHAR(ch) && isalpha(GET_IMMEDIATE_VALUE(ch)));
   RETURN1();
})

(define-glue (char-numeric? ch)
{
   REG0 = rb_to_bo(OBJ_ISA_ASCII_CHAR(ch) && isdigit(GET_IMMEDIATE_VALUE(ch)));
   RETURN1();
})

(define-glue (char-whitespace? ch)
{
   REG0 = rb_to_bo(OBJ_ISA_ASCII_CHAR(ch) && isspace(GET_IMMEDIATE_VALUE(ch)));
   RETURN1();
})

(define-glue (char-upper-case? ch)
{
   REG0 = rb_to_bo(OBJ_ISA_ASCII_CHAR(ch) && isupper(GET_IMMEDIATE_VALUE(ch)));
   RETURN1();
})

(define-glue (char-lower-case? ch)
{
   REG0 = rb_to_bo(OBJ_ISA_ASCII_CHAR(ch) && islower(GET_IMMEDIATE_VALUE(ch)));
   RETURN1();
})


(define-glue (bit-ref value bit)
{
int i;

    COUNT_ARGS(2);
    if (!OBJ_ISA_FIXNUM(value) || !OBJ_ISA_FIXNUM(bit))
        scheme_error( "(bit-ref ~s ~s): invalid arg", 2, value, bit );

    i = fx2int(bit);
    if (i < 0 || i >= 30)
	scheme_error( "bit-ref: bit ~d is invalid", 1, bit );

    REG0 = (VAL(value) & (1 << (i+2))) ? TRUE_OBJ : FALSE_OBJ;
    RETURN(1);
})

(define-glue (bit-set! value bit flag) 
{
int i;

    if (arg_count_reg == 2)
      flag = TRUE_OBJ;
    else
      COUNT_ARGS(3);
    if (!OBJ_ISA_FIXNUM(value) || !OBJ_ISA_FIXNUM(bit))
        scheme_error( "(bit-set! ~s ~s): invalid arg", 2, value, bit );

    i = fx2int(bit);

    if (i < 0 || i >= 30)
	scheme_error( "bit-set: bit ~d is invalid", 1, bit );

    if (truish(flag))
      REG0 = OBJ(VAL(value) | (1 << (i+2)));
    else
      REG0 = OBJ(VAL(value) & ~(1 << (i+2)));
    RETURN(1);
})

