/**********************************************
THIS FILE WAS AUTOMATICALLY GENERATED, AND MAY
BE AUTOMATICALLY RE-GENERATED WHEN THE COMPILER
OR SOURCE CHANGES.  DO NOT MODIFY THIS FILE BY HAND!
RScheme Build (v0.7.3.4-b7u, 2007-05-30)
**********************************************/

/******************************** Preamble ********************************/

#define _MODULE_IMAGEIO
#define _SCM_GLUE
#define _C_GLUE
#include "imageio_p.h"
#include <rscheme/vinsns.h>
extern struct module_descr module_imageio;
extern struct part_descr imageio_part_glue;
static char sccsid[] = "@(#)imageio modules/imageio/glue.scm [239655936] (RS v0.7.3.4-b7u, 2007-05-30)";

/************************** Function Definitions **************************/


/*********************** Raw glue `compact-buffers' ***********************/
#define raw_dest_str REG0
#define raw_offset REG1
#define raw_source REG2

static char rsfn_compact_buffers_name[] = "compact-buffers";
#define FUNCTION rsfn_compact_buffers_name

PROLOGUE(compact_buffers)

BEGIN_FWD(compact_buffers)
  FWD_MONOTONE(compact_buffers_0)
END_FWD(compact_buffers)

#define FPLACE_CODE (1000+0)
MONOTONE(compact_buffers_0)
{  char *dest_str;
  int offset;
  obj source;
  COUNT_ARGS(3);
  if (!STRING_P(raw_dest_str))
    {
      obj c;
      c = make5( TLREFB(0),
                 NIL_OBJ,  /* properties */
                 lookup_symbol( FUNCTION ),
                 cons( raw_dest_str, NIL_OBJ ),
                 lookup_symbol( "dest_str" ),
                 TLREFB(1) );
      raise_error( c );
    }
  dest_str = (char *)string_text(raw_dest_str);

  offset = basic_raw_int(raw_offset);

  if (!instance_p(raw_source,TLREFB(2)))
    {
      obj c;
      c = make5( TLREFB(0),
                 NIL_OBJ,  /* properties */
                 lookup_symbol( FUNCTION ),
                 cons( raw_source, NIL_OBJ ),
                 lookup_symbol( "source" ),
                 TLREFB(2) );
      raise_error( c );
    }
  source = raw_source;


{
  UINT_32 n = offset;
  UINT_8 *buf, *dest;
  obj s = source;
  
  dest = (UINT_8 *)dest_str + offset;
  for (s = source; PAIR_P(s); s = pair_cdr(s))
    {
      buf = (UINT_8 *)PTR_TO_DATAPTR(pair_car(s));
      n = *(UINT_32 *)buf;
      memcpy( dest, buf + sizeof(UINT_32), n );
      dest += n;
    }
  REG0 = raw_dest_str;
  REG1 = int2fx( dest - (UINT_8 *)dest_str );
  RETURN(2);
}}
#undef FPLACE_CODE

EPILOGUE(compact_buffers)

BEGIN_BACK(compact_buffers)
  BACK_MONOTONE(compact_buffers_0)
END_BACK(compact_buffers)

static struct function_descr compact_buffers_descr = {
	&imageio_part_glue,
	JUMP_TABLE( compact_buffers ),
	rsfn_compact_buffers_name };
#undef FUNCTION

#undef raw_dest_str
#undef raw_offset
#undef raw_source

/************************** Raw glue `compress' **************************/
#define raw_source REG0

static char rsfn_compress_name[] = "compress";
#define FUNCTION rsfn_compress_name

PROLOGUE(compress)

BEGIN_FWD(compress)
  FWD_MONOTONE(compress_0)
END_FWD(compress)

#define FPLACE_CODE (1000+0)
MONOTONE(compress_0)
{  obj source;
  COUNT_ARGS(1);
  if (!instance_p(raw_source,TLREFB(0)))
    {
      obj c;
      c = make5( TLREFB(1),
                 NIL_OBJ,  /* properties */
                 lookup_symbol( FUNCTION ),
                 cons( raw_source, NIL_OBJ ),
                 lookup_symbol( "source" ),
                 TLREFB(0) );
      raise_error( c );
    }
  source = raw_source;


{
   REG0 = rs_compress( source );
   RETURN1();
}}
#undef FPLACE_CODE

EPILOGUE(compress)

BEGIN_BACK(compress)
  BACK_MONOTONE(compress_0)
END_BACK(compress)

static struct function_descr compress_descr = {
	&imageio_part_glue,
	JUMP_TABLE( compress ),
	rsfn_compress_name };
#undef FUNCTION

#undef raw_source

/************************* Raw glue `uncompress' *************************/
#define raw_source REG0

static char rsfn_uncompress_name[] = "uncompress";
#define FUNCTION rsfn_uncompress_name

PROLOGUE(uncompress)

BEGIN_FWD(uncompress)
  FWD_MONOTONE(uncompress_0)
END_FWD(uncompress)

#define FPLACE_CODE (1000+0)
MONOTONE(uncompress_0)
{  obj source;
  COUNT_ARGS(1);
  if (!STRING_P(raw_source))
    {
      obj c;
      c = make5( TLREFB(0),
                 NIL_OBJ,  /* properties */
                 lookup_symbol( FUNCTION ),
                 cons( raw_source, NIL_OBJ ),
                 lookup_symbol( "source" ),
                 TLREFB(1) );
      raise_error( c );
    }
  source = raw_source;


{
   REG0 = rs_extract( (UINT_8 *)string_text(source),
		      string_length(source) );
   RETURN1();
}}
#undef FPLACE_CODE

EPILOGUE(uncompress)

BEGIN_BACK(uncompress)
  BACK_MONOTONE(uncompress_0)
END_BACK(uncompress)

static struct function_descr uncompress_descr = {
	&imageio_part_glue,
	JUMP_TABLE( uncompress ),
	rsfn_uncompress_name };
#undef FUNCTION

#undef raw_source

/******************* Raw glue `derender-unmarshalling' *******************/
#define raw_source REG0
#define raw_refs REG1

static char rsfn_derender_unmarshalling_name[] = "derender-unmarshalling";
#define FUNCTION rsfn_derender_unmarshalling_name

PROLOGUE(derender_unmarshalling)

BEGIN_FWD(derender_unmarshalling)
  FWD_MONOTONE(derender_unmarshalling_0)
END_FWD(derender_unmarshalling)

#define FPLACE_CODE (1000+0)
MONOTONE(derender_unmarshalling_0)
{  obj source;
  obj refs;
  COUNT_ARGS(2);
  if (!STRING_P(raw_source))
    {
      obj c;
      c = make5( TLREFB(0),
                 NIL_OBJ,  /* properties */
                 lookup_symbol( FUNCTION ),
                 cons( raw_source, NIL_OBJ ),
                 lookup_symbol( "source" ),
                 TLREFB(1) );
      raise_error( c );
    }
  source = raw_source;

  if (!instance_p(raw_refs,TLREFB(2)))
    {
      obj c;
      c = make5( TLREFB(0),
                 NIL_OBJ,  /* properties */
                 lookup_symbol( FUNCTION ),
                 cons( raw_refs, NIL_OBJ ),
                 lookup_symbol( "refs" ),
                 TLREFB(2) );
      raise_error( c );
    }
  refs = raw_refs;


{
   REG0 = rs_load_image( (UINT_8 *)string_text(source), 
			 string_length(source),
			 refs );
   RETURN1();
}}
#undef FPLACE_CODE

EPILOGUE(derender_unmarshalling)

BEGIN_BACK(derender_unmarshalling)
  BACK_MONOTONE(derender_unmarshalling_0)
END_BACK(derender_unmarshalling)

static struct function_descr derender_unmarshalling_descr = {
	&imageio_part_glue,
	JUMP_TABLE( derender_unmarshalling ),
	rsfn_derender_unmarshalling_name };
#undef FUNCTION

#undef raw_source
#undef raw_refs

/********************* Raw glue `render-marshalling' *********************/
#define raw_sections REG0
#define raw_marshall_table REG1
#define raw_fix_slot_table REG2
#define root REG3

static char rsfn_render_marshalling_name[] = "render-marshalling";
#define FUNCTION rsfn_render_marshalling_name

PROLOGUE(render_marshalling)

BEGIN_FWD(render_marshalling)
  FWD_MONOTONE(render_marshalling_0)
END_FWD(render_marshalling)

#define FPLACE_CODE (1000+0)
MONOTONE(render_marshalling_0)
{  obj sections;
  obj marshall_table;
  obj fix_slot_table;
  COUNT_ARGS(4);
  if (!instance_p(raw_sections,TLREFB(0)))
    {
      obj c;
      c = make5( TLREFB(1),
                 NIL_OBJ,  /* properties */
                 lookup_symbol( FUNCTION ),
                 cons( raw_sections, NIL_OBJ ),
                 lookup_symbol( "sections" ),
                 TLREFB(0) );
      raise_error( c );
    }
  sections = raw_sections;

  if (!instance_p(raw_marshall_table,TLREFB(2)))
    {
      obj c;
      c = make5( TLREFB(1),
                 NIL_OBJ,  /* properties */
                 lookup_symbol( FUNCTION ),
                 cons( raw_marshall_table, NIL_OBJ ),
                 lookup_symbol( "marshall_table" ),
                 TLREFB(2) );
      raise_error( c );
    }
  marshall_table = raw_marshall_table;

  if (!instance_p(raw_fix_slot_table,TLREFB(2)))
    {
      obj c;
      c = make5( TLREFB(1),
                 NIL_OBJ,  /* properties */
                 lookup_symbol( FUNCTION ),
                 cons( raw_fix_slot_table, NIL_OBJ ),
                 lookup_symbol( "fix_slot_table" ),
                 TLREFB(2) );
      raise_error( c );
    }
  fix_slot_table = raw_fix_slot_table;


{
  REG0 = rs_save_image( sections,
		        marshall_table,
		        fix_slot_table,
		        root );
  RETURN1();
}}
#undef FPLACE_CODE

EPILOGUE(render_marshalling)

BEGIN_BACK(render_marshalling)
  BACK_MONOTONE(render_marshalling_0)
END_BACK(render_marshalling)

static struct function_descr render_marshalling_descr = {
	&imageio_part_glue,
	JUMP_TABLE( render_marshalling ),
	rsfn_render_marshalling_name };
#undef FUNCTION

#undef raw_sections
#undef raw_marshall_table
#undef raw_fix_slot_table
#undef root

/************************* Raw glue `parse-refs' *************************/
#define raw_src REG0
#define raw_offset REG1
#define raw_class_table REG2
#define intern_q REG3
#define raw_class_dict REG4
#define raw_symbol_dict REG5

static char rsfn_parse_refs_name[] = "parse-refs";
#define FUNCTION rsfn_parse_refs_name

PROLOGUE(parse_refs)

BEGIN_FWD(parse_refs)
  FWD_MONOTONE(parse_refs_0)
END_FWD(parse_refs)

#define FPLACE_CODE (1000+0)
MONOTONE(parse_refs_0)
{  char *src;
  int offset;
  obj class_table;
  obj class_dict;
  obj symbol_dict;
  COUNT_ARGS(6);
  if (!STRING_P(raw_src))
    {
      obj c;
      c = make5( TLREFB(3),
                 NIL_OBJ,  /* properties */
                 lookup_symbol( FUNCTION ),
                 cons( raw_src, NIL_OBJ ),
                 lookup_symbol( "src" ),
                 TLREFB(4) );
      raise_error( c );
    }
  src = (char *)string_text(raw_src);

  offset = basic_raw_int(raw_offset);

  if (!instance_p(raw_class_table,TLREFB(5)))
    {
      obj c;
      c = make5( TLREFB(3),
                 NIL_OBJ,  /* properties */
                 lookup_symbol( FUNCTION ),
                 cons( raw_class_table, NIL_OBJ ),
                 lookup_symbol( "class_table" ),
                 TLREFB(5) );
      raise_error( c );
    }
  class_table = raw_class_table;

  if (!instance_p(raw_class_dict,TLREFB(6)))
    {
      obj c;
      c = make5( TLREFB(3),
                 NIL_OBJ,  /* properties */
                 lookup_symbol( FUNCTION ),
                 cons( raw_class_dict, NIL_OBJ ),
                 lookup_symbol( "class_dict" ),
                 TLREFB(6) );
      raise_error( c );
    }
  class_dict = raw_class_dict;

  if (!instance_p(raw_symbol_dict,TLREFB(6)))
    {
      obj c;
      c = make5( TLREFB(3),
                 NIL_OBJ,  /* properties */
                 lookup_symbol( FUNCTION ),
                 cons( raw_symbol_dict, NIL_OBJ ),
                 lookup_symbol( "symbol_dict" ),
                 TLREFB(6) );
      raise_error( c );
    }
  symbol_dict = raw_symbol_dict;


{
  obj result, misses;
  UINT_32 n;
  UINT_8 *s;

  /* note:  herein I rely on gvec_write_fresh being a nop
   * (by storing directly into dst) 
   */
   misses = NIL_OBJ;
  s = ((UINT_8 *)src) + offset;
  result = parse_refs( class_table, 
		   &s, 
		   string_length(raw_src) - offset,
		   truish(intern_q) ? YES : NO, 
		   TLREF(0), TLREF(1), 
		   gvec_read( class_dict, SLOT(0) ),
		   gvec_read( symbol_dict, SLOT(0) ),
		   &misses );
  REG0 = result;
  REG1 = int2fx( s - (UINT_8*)src );
  if (EQ(misses,NIL_OBJ))
    RETURN(2);
  else
  {
    REG2 = misses;
    APPLY( 3, TLREF(2) );
  }
}}
#undef FPLACE_CODE

EPILOGUE(parse_refs)

BEGIN_BACK(parse_refs)
  BACK_MONOTONE(parse_refs_0)
END_BACK(parse_refs)

static struct function_descr parse_refs_descr = {
	&imageio_part_glue,
	JUMP_TABLE( parse_refs ),
	rsfn_parse_refs_name };
#undef FUNCTION

#undef raw_src
#undef raw_offset
#undef raw_class_table
#undef intern_q
#undef raw_class_dict
#undef raw_symbol_dict
/******************************* Postamble *******************************/
/**************************** Part Link Table ****************************/


static struct function_descr *(part_glue_tab[]) = {
    &compact_buffers_descr,
    &compress_descr,
    &uncompress_descr,
    &derender_unmarshalling_descr,
    &render_marshalling_descr,
    &parse_refs_descr,
    NULL };
struct part_descr imageio_part_glue = {
    239655936,
    &module_imageio,
    part_glue_tab,
    "glue",
    0, sccsid };
#undef _MODULE_IMAGEIO
#undef _SCM_GLUE
#undef _C_GLUE
