#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/corelib/string.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.28
 | File mod date:    2004-03-24 14:39:47
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  corelib
 |
 | Purpose:          <string> manipulation primitives etc
 `------------------------------------------------------------------------|#

#|
(define-method string-length ((self <substring>))
  ...)
|#
  
(define-method string-length ((self <string>))
  (sub1 (bvec-length self)))

#|
;; would like to be able to write something like:

(define (make-string (num <fixnum>) (init <char> default: #\space))
  (if (string? init)
      (make-string-str num init)
      (if (char? init)
	  (make-string-ch num init)
	  (type-error...))))
|#

(define-glue (make-string num ch)
 literals: ((& make-string) (& type-error)
	    "not a <fixnum>" "not an <ascii-char>")
{
char c;
UINT_32 n = fx2int(num);

    if (arg_count_reg == 1)
	c = ' ';
    else
      {
	COUNT_ARGS(2);
	if (!OBJ_ISA_ASCII_CHAR(ch))
	  {
	    REG2 = ch;
	    REG3 = LITERAL(3);
	    REG0 = TLREF(0);
	    REG1 = int2fx(1);
	    APPLY(4,TLREF(1));
	  }
	c = GET_IMMEDIATE_VALUE(ch);
      }
    if (!OBJ_ISA_FIXNUM(num))
      {
	REG2 = num;
	REG3 = LITERAL(2);
	REG0 = TLREF(0);
	REG1 = int2fx(0);
	APPLY(4,TLREF(1));
      }
    if (fx2int(num) < 0)
      {
	scheme_error( "make-string: length ~s < 0 is invalid", 1, num );
      }

    REG0 = bvec_alloc( fx2int(num)+1, string_class );
    memset( PTR_TO_DATAPTR(REG0), c, n );
    RETURN1();
})

(define-safe-glue (string-fill! (str <string>) (fill <raw-ascii-char>))
{
    memset( PTR_TO_DATAPTR(str), fill, string_length(str) );
    RETURN1();
})

(define-safe-glue (string-copy (str <string>))
{ 
UINT_32 len = SIZEOF_PTR(str);
obj new_str;

    new_str = bvec_alloc( len, string_class );
    memcpy( PTR_TO_DATAPTR(new_str), PTR_TO_DATAPTR(str), len );
    REG0 = new_str;
    RETURN1();
})

(define-glue (substring str start end)
{
INT_32 i, j;
UINT_32 len, new_len;
obj new_str;

    if (arg_count_reg != 2 && arg_count_reg != 3)
	scheme_error( "substring: expected 2 or 3 args, got ~d",
			1, int2fx(arg_count_reg) );
    if (!STRING_P(str))
	scheme_error( "substring: str not a string: ~s", 1, str );
    if (!OBJ_ISA_FIXNUM(start))
	scheme_error( "substring: start not a fixnum: ~s", 1, start );
	
    len = SIZEOF_PTR(str) - 1;
    i = fx2int(start);
    if (arg_count_reg == 2)
        j = len;
    else
    {
	if (!OBJ_ISA_FIXNUM(end))
	    scheme_error( "substring: end not a fixnum: ~s", 1, end );
	j = fx2int(end);
    }
    if (i < 0 || i > len)
	scheme_error( "start ~d out of range 0..~d", 
			2, start, int2fx(len) );
    if (j < i || j > len)
	scheme_error( "end ~d out of range ~d..~d", 
			3, end, start, int2fx(len) );
    
    new_len = j - i;
    new_str = bvec_alloc( new_len + 1, string_class );
    
    /* Note that we don't have to copy or put in a terminal NUL,
       because bvec_alloc guarantees that the whole last
       word is ZERO, so if you alloc n+1 BYTES, then the last
       BYTE will surely be zero (but the next-to-the-last byte
       might not be, because it may fall on the preceding word).
       This is even true if n=0. */
       
    memcpy( PTR_TO_DATAPTR(new_str), 
    	    string_text(str)+i,
	    new_len );
    REG0 = new_str;
    RETURN1();
})

(define-glue (string-search str search_crit skip)
{
INT_32 skipn = 0;

    if (arg_count_reg == 3)
      {
	if (OBJ_ISA_FIXNUM(skip))
	    skipn = fx2int(skip);
	else 
	    scheme_error( "string-search: skip ~s not a fixnum", 1, skip );
      }
    else 
      {
	COUNT_ARGS(2);
      }

    if (BYTE_STRING_P(str) || UNICODE_STRING_P(str))
      REG0 = rs_string_search( str, search_crit, skipn );
    else
      scheme_error( "string-search: ~s not a string", 1, str );
    RETURN(1);
})

(define-method splitter-procedure ((self <ascii-char>))
  (lambda (str i)
    (let ((n (string-search str self i)))
      (if n
          (values n (add1 n))
          (values #f #f)))))

(define-method splitter-procedure ((self <string>))
  (let (((l <fixnum>) (string-length self)))
    (lambda (str i)
      (let ((n (string-search str self i)))
        (if n
            (values n (fixnum+ n l))
            (values #f #f))))))

(define-method splitter-procedure ((self <function>))
  self)

;;; The break function `brk' should take two arguments
;;; and return (at least) two values.  The first argument
;;; is the string to operate on, and the second argument
;;; is the offset (starting at 0) to consider.  The two return values
;;; should be the location (start and end) of the next match;
;;; #f or no values should be returned if there are no more matches

(define (string-split str brk)
  (letrec ((find-next (splitter-procedure brk))
           (split-rest (lambda ((i <fixnum>))
			 (bind ((s e (find-next str i)))
			   (if s
			       (if (eq? i e)
				   ;; Perl has some rule for making this do
				   ;; something that is sometimes useful,
				   ;; but it isn't clear that it's desirable
				   ;; to overload `split' with it
				   (error "pattern matched nothing at: ~#*@40s"
					  (substring str i))
				   (cons (substring str i s)
					 (split-rest e)))
			       (cons (substring str i) '()))))))
    (split-rest 0)))

;;; Like string-split, but include the results of the match
;;; in the resulting string.

(define (string-split/including str brk)
  (letrec ((find-next (splitter-procedure brk))
           (split-rest (lambda ((i <fixnum>))
			 (bind ((s e (find-next str i)))
			   (if s
			       (if (eq? i e)
				   ;; Perl has some rule for making this do
				   ;; something that is sometimes useful,
				   ;; but it isn't clear that it's desirable
				   ;; to overload `split' with it
				   (error "pattern matched nothing at: ~#*@40s"
					  (substring str i))
				   (cons (substring str i s)
                                         (cons (substring str s e)
                                               (split-rest e))))
			       (cons (substring str i) '()))))))
    (split-rest 0)))

(define-glue (string)
{
unsigned char *p;
int i;
obj str, ch;

    str = bvec_alloc( arg_count_reg+1, string_class );
    p = (unsigned char *)PTR_TO_DATAPTR(str);
    for (i=0; i<arg_count_reg; i++)
    {
	ch = reg_ref(i);
        if (!OBJ_ISA_ASCII_CHAR(ch))
	   scheme_error( "string: arg[~d] is ~s, not an <ascii-char>",
			 2, int2fx(i), ch );
	*p++ = GET_IMMEDIATE_VALUE(ch);
    }
    REG0 = str;
    RETURN(1);
})

(define-glue (list->string)
   literals: ("list->string: arg[0] is not a proper list (at ~d)"
	      "list->string: element[~d] is not a char: ~s"
	      "list->string: arg[0] is circular or too long")
{
obj i, new_str;
UINT_32 n;
unsigned char *p;

    COUNT_ARGS(1);
    n = 0;
    for (i=REG0; PAIR_P(i); i=pair_cdr(i))
    {
	if (!OBJ_ISA_ASCII_CHAR(pair_car(i)))
	    scheme_error( string_text(LITERAL(1)), 2, int2fx(n), pair_car(i) );
	if (n > 1000000)
	    scheme_error( string_text(LITERAL(2)), 0 );
	n++;
    }
    if (!NULL_P(i))
      {
	scheme_error( string_text(LITERAL(0)), 1, int2fx(n) );
      }
    new_str = bvec_alloc( n+1, string_class );
    p = (unsigned char *)PTR_TO_DATAPTR(new_str);
    for (i=REG0; PAIR_P(i); i=pair_cdr(i))
    {
	*p++ = GET_IMMEDIATE_VALUE(pair_car(i));
    }
    REG0 = new_str;
    RETURN(1);
})

(define-safe-glue (string->list (string <string>))
{
obj result;
const unsigned char *str;
unsigned i;

    result = NIL_OBJ;
    i = string_length(string);
    str = (unsigned char *)string_text(string) + i;
    while (i > 0)
    {
        result = cons( MAKE_ASCII_CHAR(*--str), result );
	i--;
    }
    REG0 = result;
    RETURN(1);
})

(define-syntax string-is
  (syntax-form ('< c) (fixnum<? c 0))
  (syntax-form ('<= c) (fixnum<=? c 0))
  (syntax-form ('> c) (fixnum>? c 0))
  (syntax-form ('>= c) (fixnum>=? c 0)))

(define (string<? s1 s2) (string-is < (string-compare s1 s2)))
(define (string>? s1 s2) (string-is > (string-compare s1 s2)))
(define (string<=? s1 s2) (string-is <= (string-compare s1 s2)))
(define (string>=? s1 s2) (string-is >= (string-compare s1 s2)))


(define (string-ci<? s1 s2) (string-is < (string-ci-compare s1 s2)))
(define (string-ci>? s1 s2) (string-is > (string-ci-compare s1 s2)))
(define (string-ci<=? s1 s2) (string-is <= (string-ci-compare s1 s2)))
(define (string-ci>=? s1 s2) (string-is >= (string-ci-compare s1 s2)))


(define gensym
  (let (((i <fixnum>) 0))
    (lambda ()
      (let ((s (string->symbol (string-append "G#" (fixnum->string i 10)))))
	(set! i (add1 i))
	s))))

(define-glue (string-append)
{
unsigned i, len, n;
char *p;
obj new_str, str;

    len = 0;
    for (i=0; i<arg_count_reg; i++)
    {
	str = reg_ref(i);
	if (!STRING_P(str))
	    scheme_error( "string-append: Not a string ~a", 1, str );
	len += string_length(str);
    }
    new_str = bvec_alloc( len+1, string_class );
    p = (char *)PTR_TO_DATAPTR(new_str);
    for (i=0; i<arg_count_reg; i++)
    {
	str = reg_ref(i);
        n = string_length( str );
	memcpy( p, string_text(str), n );
	p += n;
    }
    REG0 = new_str;
    RETURN(1);
})

(define-glue (string-join join str_lst)
{
unsigned jlen, len, n, k;
char *p, *j, temp;
obj i, new_str, str;

    COUNT_ARGS(2);

    if (OBJ_ISA_ASCII_CHAR(join))
      {
	temp = ASCII_CHAR_VALUE(join);
	j = &temp;
	jlen = 1;
      }
    else if (STRING_P(join))
      {
	j = string_text(join);
	jlen = string_length(join);
      }
    else
      {
	scheme_error( "string-join: join not a string or char: ~s", 1, join );
	j = 0;
	jlen = 0;
      }

    len = 0;
    for (i=str_lst, k=0; PAIR_P(i); i=pair_cdr(i), k++)
      {
	str = pair_car(i);
	if (!STRING_P(str))
	  {
	    scheme_error( "string-join: ~s in list not a string", 1, str );
	  }
	len += string_length(str);
	if (k)
	  {
	    len += jlen;
	    if (k > 10000)
	      scheme_error( "string-join: circular or list too long", 0 );
	  }
      }
    if (!EQ(i,NIL_OBJ))
      scheme_error( "string-join: Not a proper list: ~s", 1, str_lst );

    new_str = bvec_alloc( len+1, string_class );
    p = (char *)PTR_TO_DATAPTR(new_str);

    for (i=str_lst, k=0; PAIR_P(i); i=pair_cdr(i), k++)
    {
	str = pair_car(i);
	if (k)
	  {
	    memcpy( p, j, jlen );
	    p += jlen;
	  }
        n = string_length( str );
	memcpy( p, string_text(str), n );
	p += n;
    }
    REG0 = new_str;
    RETURN(1);
})

(define-method string-ref ((str <string>) (i <fixnum>))
  (if (and (fixnum>=? i 0) (fixnum<? i (string-length str)))
      (integer->ascii-char (bvec-ref str i))
      (limit-check 'string-ref i (string-length str))))
    
(define-method string-set! ((str <string>) (i <fixnum>) (c <ascii-char>))
  (if (and (fixnum>=? i 0) (fixnum<? i (string-length str)))
      (begin
	(bvec-set! str i (ascii-char->integer c))
	c)
      (limit-check 'string-set! i (string-length str))))

(define-method to-string ((self <string>))
  self)

(define-method to-string ((self <symbol>))
  (symbol->string self))

;;;

(define-method to-string ((self <object>))
  (string-append "@" (machine-bits->string self)))

(define-method to-string ((self <boolean>))
  (if self "#t" "#f"))

(define-method to-string ((self <char>))
  (string self))

(define-method to-string ((self <empty-list>))
  "()")

(define-method to-string ((self <<class>>))
  (to-string (class-name self)))

;;;

(define-safe-glue (trim-whitespace (str <string>))
{
  int i = 0;
  int j = string_length( str );
  UINT_8 *s = string_text( str );
  obj so = str;

  while ((i < j) && isspace( s[i] ))
    {
      i++;
    }
  while ((j > i) && isspace( s[j-1] ))
    {
      j--;
    }
  if ((i == 0) && (j == string_length( str )))
    {
      REG0 = str;
    }
  else
    {
      REG0 = bvec_alloc( j-i+1, string_class );
      memcpy( PTR_TO_DATAPTR(REG0), s+i, j-i );
    }
  RETURN1();
})

;;;

(define (string->byte-vector (s <string>))
  (let* (((n <fixnum>) (string-length s))
         (d (bvec-alloc <byte-vector> n)))
    (bvec-copy d 0 s 0 n)
    d))

(define (bvec->string s)
  (let* (((n <fixnum>) (bvec-length s))
         (d (make-string n)))
    (bvec-copy d 0 s 0 n)
    d))


(define-glue (bvec-append2 a b)
{
  char *d;
  obj r;
  UINT_32 n;

  CHECK_BVEC(a);
  CHECK_BVEC(b);
  n = SIZEOF_PTR(a) + SIZEOF_PTR(b);
  r = bvec_alloc( n, CLASSOF_PTR(a) );
  d = PTR_TO_DATAPTR(r);

  memcpy( d, PTR_TO_DATAPTR(a), SIZEOF_PTR(a) );
  d += SIZEOF_PTR(a);

  memcpy( d, PTR_TO_DATAPTR(b), SIZEOF_PTR(b) );
  REG0 = r;
  RETURN1();
})

(define-safe-glue (bvec-append3 a b c)
{
  char *d;
  obj r;
  UINT_32 n;

  CHECK_BVEC(a);
  CHECK_BVEC(b);
  CHECK_BVEC(c);

  n = SIZEOF_PTR(a) + SIZEOF_PTR(b) + SIZEOF_PTR(c);
  r = bvec_alloc( n, CLASSOF_PTR(a) );
  d = PTR_TO_DATAPTR(r);

  memcpy( d, PTR_TO_DATAPTR(a), SIZEOF_PTR(a) );
  d += SIZEOF_PTR(a);

  memcpy( d, PTR_TO_DATAPTR(b), SIZEOF_PTR(b) );
  d += SIZEOF_PTR(b);

  memcpy( d, PTR_TO_DATAPTR(c), SIZEOF_PTR(c) );

  REG0 = r;
  RETURN1();
})

(define-glue (bvec-append*)
{
  unsigned i, n = 0;
  obj r, c = byte_vector_class;
  UINT_8 *d;

  switch (arg_count_reg) {
  case 0:
    c = byte_vector_class;
    break;

  default:
     for (i=4; i<arg_count_reg; i++) {
       obj a = reg_ref( i );
       CHECK_BVEC( a );
       n += SIZEOF_PTR( a );
    }
   
  case 4:
    CHECK_BVEC( REG3 );
    n += SIZEOF_PTR( REG3 );

  case 3:
    CHECK_BVEC( REG2 );
    n += SIZEOF_PTR( REG2 );

  case 2:
    CHECK_BVEC( REG1 );
    n += SIZEOF_PTR( REG1 );

  case 1:
    c = CLASSOF_PTR( REG0 );
    CHECK_BVEC( REG0 );
    n += SIZEOF_PTR( REG0 );
  }

  r = bvec_alloc( n, c );
  d = PTR_TO_DATAPTR( r );

  for (i=0; i<arg_count_reg; i++) {
    obj a = reg_ref( i );
    unsigned l = SIZEOF_PTR( a );
    memcpy( d, PTR_TO_DATAPTR( a ), l );
    d += l;
  }
  REG0 = r;
  RETURN1();
})

              
(define-syntax bvec-append
  (syntax-form (a)
    a)
  (syntax-form (a b)
    (bvec-append2 a b))
  (syntax-form (a b c . r)
    (bvec-append* a b c . r))
  (else
   bvec-append*))

