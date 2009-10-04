/**********************************************
THIS FILE WAS AUTOMATICALLY GENERATED, AND MAY
BE AUTOMATICALLY RE-GENERATED WHEN THE COMPILER
OR SOURCE CHANGES.  DO NOT MODIFY THIS FILE BY HAND!
RScheme Build (v0.7.3.4-b7u, 2007-05-30)
**********************************************/

/******************************** Preamble ********************************/

#define _MODULE_CORELIB
#define _SCM_STRING
#define _C_STRING
#include "corelib_p.h"
#include <rscheme/vinsns.h>
extern struct module_descr module_corelib;
extern struct part_descr corelib_part_string;
static char sccsid[] = "@(#)corelib modules/corelib/string.scm [207072260] (RS v0.7.3.4-b7u, 2007-05-30)";

/************************** Function Definitions **************************/


/************************* Raw glue `make-string' *************************/
#define num REG0
#define ch REG1

static char rsfn_make_string_name[] = "make-string";
#define FUNCTION rsfn_make_string_name

PROLOGUE(make_string)

BEGIN_FWD(make_string)
  FWD_MONOTONE(make_string_0)
END_FWD(make_string)

#define FPLACE_CODE (1000+0)
MONOTONE(make_string_0)
{
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
}}
#undef FPLACE_CODE

EPILOGUE(make_string)

BEGIN_BACK(make_string)
  BACK_MONOTONE(make_string_0)
END_BACK(make_string)

static struct function_descr make_string_descr = {
	&corelib_part_string,
	JUMP_TABLE( make_string ),
	rsfn_make_string_name };
#undef FUNCTION

#undef num
#undef ch

/************************ Raw glue `string-fill!' ************************/
#define raw_str REG0
#define raw_fill REG1

static char rsfn_string_fill_name[] = "string-fill!";
#define FUNCTION rsfn_string_fill_name

PROLOGUE(string_fill)

BEGIN_FWD(string_fill)
  FWD_MONOTONE(string_fill_0)
END_FWD(string_fill)

#define FPLACE_CODE (1000+0)
MONOTONE(string_fill_0)
{  obj str;
  UINT_8 fill;
  COUNT_ARGS(2);
  if (!STRING_P(raw_str))
    {
      obj c;
      c = make5( TLREFB(0),
                 NIL_OBJ,  /* properties */
                 lookup_symbol( FUNCTION ),
                 cons( raw_str, NIL_OBJ ),
                 lookup_symbol( "str" ),
                 TLREFB(1) );
      raise_error( c );
    }
  str = raw_str;

  if (!OBJ_ISA_ASCII_CHAR(raw_fill))
    {
      obj c;
      c = make5( TLREFB(0),
                 NIL_OBJ,  /* properties */
                 lookup_symbol( FUNCTION ),
                 cons( raw_fill, NIL_OBJ ),
                 lookup_symbol( "fill" ),
                 TLREFB(2) );
      raise_error( c );
    }
  fill = ASCII_CHAR_VALUE(raw_fill);


{
    memset( PTR_TO_DATAPTR(str), fill, string_length(str) );
    RETURN1();
}}
#undef FPLACE_CODE

EPILOGUE(string_fill)

BEGIN_BACK(string_fill)
  BACK_MONOTONE(string_fill_0)
END_BACK(string_fill)

static struct function_descr string_fill_descr = {
	&corelib_part_string,
	JUMP_TABLE( string_fill ),
	rsfn_string_fill_name };
#undef FUNCTION

#undef raw_str
#undef raw_fill

/************************* Raw glue `string-copy' *************************/
#define raw_str REG0

static char rsfn_string_copy_name[] = "string-copy";
#define FUNCTION rsfn_string_copy_name

PROLOGUE(string_copy)

BEGIN_FWD(string_copy)
  FWD_MONOTONE(string_copy_0)
END_FWD(string_copy)

#define FPLACE_CODE (1000+0)
MONOTONE(string_copy_0)
{  obj str;
  COUNT_ARGS(1);
  if (!STRING_P(raw_str))
    {
      obj c;
      c = make5( TLREFB(0),
                 NIL_OBJ,  /* properties */
                 lookup_symbol( FUNCTION ),
                 cons( raw_str, NIL_OBJ ),
                 lookup_symbol( "str" ),
                 TLREFB(1) );
      raise_error( c );
    }
  str = raw_str;


{ 
UINT_32 len = SIZEOF_PTR(str);
obj new_str;

    new_str = bvec_alloc( len, string_class );
    memcpy( PTR_TO_DATAPTR(new_str), PTR_TO_DATAPTR(str), len );
    REG0 = new_str;
    RETURN1();
}}
#undef FPLACE_CODE

EPILOGUE(string_copy)

BEGIN_BACK(string_copy)
  BACK_MONOTONE(string_copy_0)
END_BACK(string_copy)

static struct function_descr string_copy_descr = {
	&corelib_part_string,
	JUMP_TABLE( string_copy ),
	rsfn_string_copy_name };
#undef FUNCTION

#undef raw_str

/************************** Raw glue `substring' **************************/
#define str REG0
#define start REG1
#define end REG2

static char rsfn_substring_name[] = "substring";
#define FUNCTION rsfn_substring_name

PROLOGUE(substring)

BEGIN_FWD(substring)
  FWD_MONOTONE(substring_0)
END_FWD(substring)

#define FPLACE_CODE (1000+0)
MONOTONE(substring_0)
{
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
}}
#undef FPLACE_CODE

EPILOGUE(substring)

BEGIN_BACK(substring)
  BACK_MONOTONE(substring_0)
END_BACK(substring)

static struct function_descr substring_descr = {
	&corelib_part_string,
	JUMP_TABLE( substring ),
	rsfn_substring_name };
#undef FUNCTION

#undef str
#undef start
#undef end

/************************ Raw glue `string-search' ************************/
#define str REG0
#define search_crit REG1
#define skip REG2

static char rsfn_string_search_name[] = "string-search";
#define FUNCTION rsfn_string_search_name

PROLOGUE(string_search)

BEGIN_FWD(string_search)
  FWD_MONOTONE(string_search_0)
END_FWD(string_search)

#define FPLACE_CODE (1000+0)
MONOTONE(string_search_0)
{
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
}}
#undef FPLACE_CODE

EPILOGUE(string_search)

BEGIN_BACK(string_search)
  BACK_MONOTONE(string_search_0)
END_BACK(string_search)

static struct function_descr string_search_descr = {
	&corelib_part_string,
	JUMP_TABLE( string_search ),
	rsfn_string_search_name };
#undef FUNCTION

#undef str
#undef search_crit
#undef skip

/*************************** Raw glue `string' ***************************/

static char rsfn_string_name[] = "string";
#define FUNCTION rsfn_string_name

PROLOGUE(string)

BEGIN_FWD(string)
  FWD_MONOTONE(string_0)
END_FWD(string)

#define FPLACE_CODE (1000+0)
MONOTONE(string_0)
{
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
}}
#undef FPLACE_CODE

EPILOGUE(string)

BEGIN_BACK(string)
  BACK_MONOTONE(string_0)
END_BACK(string)

static struct function_descr string_descr = {
	&corelib_part_string,
	JUMP_TABLE( string ),
	rsfn_string_name };
#undef FUNCTION


/************************ Raw glue `list->string' ************************/

static char rsfn_list_string_name[] = "list->string";
#define FUNCTION rsfn_list_string_name

PROLOGUE(list_string)

BEGIN_FWD(list_string)
  FWD_MONOTONE(list_string_0)
END_FWD(list_string)

#define FPLACE_CODE (1000+0)
MONOTONE(list_string_0)
{
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
}}
#undef FPLACE_CODE

EPILOGUE(list_string)

BEGIN_BACK(list_string)
  BACK_MONOTONE(list_string_0)
END_BACK(list_string)

static struct function_descr list_string_descr = {
	&corelib_part_string,
	JUMP_TABLE( list_string ),
	rsfn_list_string_name };
#undef FUNCTION


/************************ Raw glue `string->list' ************************/
#define raw_string REG0

static char rsfn_string_list_name[] = "string->list";
#define FUNCTION rsfn_string_list_name

PROLOGUE(string_list)

BEGIN_FWD(string_list)
  FWD_MONOTONE(string_list_0)
END_FWD(string_list)

#define FPLACE_CODE (1000+0)
MONOTONE(string_list_0)
{  obj string;
  COUNT_ARGS(1);
  if (!STRING_P(raw_string))
    {
      obj c;
      c = make5( TLREFB(0),
                 NIL_OBJ,  /* properties */
                 lookup_symbol( FUNCTION ),
                 cons( raw_string, NIL_OBJ ),
                 lookup_symbol( "string" ),
                 TLREFB(1) );
      raise_error( c );
    }
  string = raw_string;


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
}}
#undef FPLACE_CODE

EPILOGUE(string_list)

BEGIN_BACK(string_list)
  BACK_MONOTONE(string_list_0)
END_BACK(string_list)

static struct function_descr string_list_descr = {
	&corelib_part_string,
	JUMP_TABLE( string_list ),
	rsfn_string_list_name };
#undef FUNCTION

#undef raw_string

/************************ Raw glue `string-append' ************************/

static char rsfn_string_append_name[] = "string-append";
#define FUNCTION rsfn_string_append_name

PROLOGUE(string_append)

BEGIN_FWD(string_append)
  FWD_MONOTONE(string_append_0)
END_FWD(string_append)

#define FPLACE_CODE (1000+0)
MONOTONE(string_append_0)
{
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
}}
#undef FPLACE_CODE

EPILOGUE(string_append)

BEGIN_BACK(string_append)
  BACK_MONOTONE(string_append_0)
END_BACK(string_append)

static struct function_descr string_append_descr = {
	&corelib_part_string,
	JUMP_TABLE( string_append ),
	rsfn_string_append_name };
#undef FUNCTION


/************************* Raw glue `string-join' *************************/
#define join REG0
#define str_lst REG1

static char rsfn_string_join_name[] = "string-join";
#define FUNCTION rsfn_string_join_name

PROLOGUE(string_join)

BEGIN_FWD(string_join)
  FWD_MONOTONE(string_join_0)
END_FWD(string_join)

#define FPLACE_CODE (1000+0)
MONOTONE(string_join_0)
{
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
}}
#undef FPLACE_CODE

EPILOGUE(string_join)

BEGIN_BACK(string_join)
  BACK_MONOTONE(string_join_0)
END_BACK(string_join)

static struct function_descr string_join_descr = {
	&corelib_part_string,
	JUMP_TABLE( string_join ),
	rsfn_string_join_name };
#undef FUNCTION

#undef join
#undef str_lst

/*********************** Raw glue `trim-whitespace' ***********************/
#define raw_str REG0

static char rsfn_trim_whitespace_name[] = "trim-whitespace";
#define FUNCTION rsfn_trim_whitespace_name

PROLOGUE(trim_whitespace)

BEGIN_FWD(trim_whitespace)
  FWD_MONOTONE(trim_whitespace_0)
END_FWD(trim_whitespace)

#define FPLACE_CODE (1000+0)
MONOTONE(trim_whitespace_0)
{  obj str;
  COUNT_ARGS(1);
  if (!STRING_P(raw_str))
    {
      obj c;
      c = make5( TLREFB(0),
                 NIL_OBJ,  /* properties */
                 lookup_symbol( FUNCTION ),
                 cons( raw_str, NIL_OBJ ),
                 lookup_symbol( "str" ),
                 TLREFB(1) );
      raise_error( c );
    }
  str = raw_str;


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
}}
#undef FPLACE_CODE

EPILOGUE(trim_whitespace)

BEGIN_BACK(trim_whitespace)
  BACK_MONOTONE(trim_whitespace_0)
END_BACK(trim_whitespace)

static struct function_descr trim_whitespace_descr = {
	&corelib_part_string,
	JUMP_TABLE( trim_whitespace ),
	rsfn_trim_whitespace_name };
#undef FUNCTION

#undef raw_str

/************************ Raw glue `bvec-append2' ************************/
#define a REG0
#define b REG1

static char rsfn_bvec_append2_name[] = "bvec-append2";
#define FUNCTION rsfn_bvec_append2_name

PROLOGUE(bvec_append2)

BEGIN_FWD(bvec_append2)
  FWD_MONOTONE(bvec_append2_0)
END_FWD(bvec_append2)

#define FPLACE_CODE (1000+0)
MONOTONE(bvec_append2_0)
{
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
}}
#undef FPLACE_CODE

EPILOGUE(bvec_append2)

BEGIN_BACK(bvec_append2)
  BACK_MONOTONE(bvec_append2_0)
END_BACK(bvec_append2)

static struct function_descr bvec_append2_descr = {
	&corelib_part_string,
	JUMP_TABLE( bvec_append2 ),
	rsfn_bvec_append2_name };
#undef FUNCTION

#undef a
#undef b

/************************ Raw glue `bvec-append3' ************************/
#define a REG0
#define b REG1
#define c REG2

static char rsfn_bvec_append3_name[] = "bvec-append3";
#define FUNCTION rsfn_bvec_append3_name

PROLOGUE(bvec_append3)

BEGIN_FWD(bvec_append3)
  FWD_MONOTONE(bvec_append3_0)
END_FWD(bvec_append3)

#define FPLACE_CODE (1000+0)
MONOTONE(bvec_append3_0)
{  COUNT_ARGS(3);

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
}}
#undef FPLACE_CODE

EPILOGUE(bvec_append3)

BEGIN_BACK(bvec_append3)
  BACK_MONOTONE(bvec_append3_0)
END_BACK(bvec_append3)

static struct function_descr bvec_append3_descr = {
	&corelib_part_string,
	JUMP_TABLE( bvec_append3 ),
	rsfn_bvec_append3_name };
#undef FUNCTION

#undef a
#undef b
#undef c

/************************ Raw glue `bvec-append*' ************************/

static char rsfn_bvec_append_name[] = "bvec-append*";
#define FUNCTION rsfn_bvec_append_name

PROLOGUE(bvec_append)

BEGIN_FWD(bvec_append)
  FWD_MONOTONE(bvec_append_0)
END_FWD(bvec_append)

#define FPLACE_CODE (1000+0)
MONOTONE(bvec_append_0)
{
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
}}
#undef FPLACE_CODE

EPILOGUE(bvec_append)

BEGIN_BACK(bvec_append)
  BACK_MONOTONE(bvec_append_0)
END_BACK(bvec_append)

static struct function_descr bvec_append_descr = {
	&corelib_part_string,
	JUMP_TABLE( bvec_append ),
	rsfn_bvec_append_name };
#undef FUNCTION

/******************************* Postamble *******************************/
/**************************** Part Link Table ****************************/


static struct function_descr *(part_string_tab[]) = {
    &make_string_descr,
    &string_fill_descr,
    &string_copy_descr,
    &substring_descr,
    &string_search_descr,
    &string_descr,
    &list_string_descr,
    &string_list_descr,
    &string_append_descr,
    &string_join_descr,
    &trim_whitespace_descr,
    &bvec_append2_descr,
    &bvec_append3_descr,
    &bvec_append_descr,
    NULL };
struct part_descr corelib_part_string = {
    207072260,
    &module_corelib,
    part_string_tab,
    "string",
    0, sccsid };
#undef _MODULE_CORELIB
#undef _SCM_STRING
#undef _C_STRING
