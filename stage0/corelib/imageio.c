/**********************************************
THIS FILE WAS AUTOMATICALLY GENERATED, AND MAY
BE AUTOMATICALLY RE-GENERATED WHEN THE COMPILER
OR SOURCE CHANGES.  DO NOT MODIFY THIS FILE BY HAND!
RScheme Build (v0.7.3.4-b7u, 2007-05-30)
**********************************************/

/******************************** Preamble ********************************/

#define _MODULE_CORELIB
#define _SCM_IMAGEIO
#define _C_IMAGEIO
#include "corelib_p.h"
#include <rscheme/vinsns.h>
extern struct module_descr module_corelib;
extern struct part_descr corelib_part_imageio;
static char sccsid[] = "@(#)corelib modules/corelib/imageio.scm [186795015] (RS v0.7.3.4-b7u, 2007-05-30)";

/************************** Function Definitions **************************/


/************************* Raw glue `save-image' *************************/
#define raw_path REG0
#define root REG1
#define raw_ref_vec REG2
#define raw_ref_names REG3
#define rplc_tbl REG4

static char rsfn_save_image_name[] = "save-image";
#define FUNCTION rsfn_save_image_name

PROLOGUE(save_image)

BEGIN_FWD(save_image)
  FWD_MONOTONE(save_image_0)
END_FWD(save_image)

#define FPLACE_CODE (1000+0)
MONOTONE(save_image_0)
{  obj path;
  obj ref_vec;
  obj ref_names;
  COUNT_ARGS(5);
  if (!STRING_P(raw_path))
    {
      obj c;
      c = make5( TLREFB(0),
                 NIL_OBJ,  /* properties */
                 lookup_symbol( FUNCTION ),
                 cons( raw_path, NIL_OBJ ),
                 lookup_symbol( "path" ),
                 TLREFB(1) );
      raise_error( c );
    }
  path = raw_path;

  if (!instance_p(raw_ref_vec,TLREFB(2)))
    {
      obj c;
      c = make5( TLREFB(0),
                 NIL_OBJ,  /* properties */
                 lookup_symbol( FUNCTION ),
                 cons( raw_ref_vec, NIL_OBJ ),
                 lookup_symbol( "ref_vec" ),
                 TLREFB(2) );
      raise_error( c );
    }
  ref_vec = raw_ref_vec;

  if (!instance_p(raw_ref_names,TLREFB(2)))
    {
      obj c;
      c = make5( TLREFB(0),
                 NIL_OBJ,  /* properties */
                 lookup_symbol( FUNCTION ),
                 cons( raw_ref_names, NIL_OBJ ),
                 lookup_symbol( "ref_names" ),
                 TLREFB(2) );
      raise_error( c );
    }
  ref_names = raw_ref_names;


{
extern obj rs_save_image_file( obj root, obj ref_vec, obj ref_names, 
			       obj rplc, obj out_info );

    rs_save_image_file( root, ref_vec, ref_names, rplc_tbl, path );
    RETURN0();
}}
#undef FPLACE_CODE

EPILOGUE(save_image)

BEGIN_BACK(save_image)
  BACK_MONOTONE(save_image_0)
END_BACK(save_image)

static struct function_descr save_image_descr = {
	&corelib_part_imageio,
	JUMP_TABLE( save_image ),
	rsfn_save_image_name };
#undef FUNCTION

#undef raw_path
#undef root
#undef raw_ref_vec
#undef raw_ref_names
#undef rplc_tbl

/************************* Raw glue `load-image' *************************/
#define path REG0
#define link REG1
#define options REG2

static char rsfn_load_image_name[] = "load-image";
#define FUNCTION rsfn_load_image_name

PROLOGUE(load_image)

BEGIN_FWD(load_image)
  FWD_MONOTONE(load_image_0)
END_FWD(load_image)

#define FPLACE_CODE (1000+0)
MONOTONE(load_image_0)
{
{
extern obj load_image_file( const char *the_path, 
			    obj the_link_table,
			    obj opt, int *vers );
int v;

    if (arg_count_reg == 2)
      options = FALSE_OBJ;
    else
      COUNT_ARGS(3);

    REG0 = load_image_file( string_text(path), link, options, &v );
    REG1 = int2fx(v);
    RETURN(2);
}}
#undef FPLACE_CODE

EPILOGUE(load_image)

BEGIN_BACK(load_image)
  BACK_MONOTONE(load_image_0)
END_BACK(load_image)

static struct function_descr load_image_descr = {
	&corelib_part_imageio,
	JUMP_TABLE( load_image ),
	rsfn_load_image_name };
#undef FUNCTION

#undef path
#undef link
#undef options

/********************* Raw glue `image-write-header' *********************/
#define path REG0
#define hdr REG1

static char rsfn_image_write_header_name[] = "image-write-header";
#define FUNCTION rsfn_image_write_header_name

PROLOGUE(image_write_header)

BEGIN_FWD(image_write_header)
  FWD_MONOTONE(image_write_header_0)
END_FWD(image_write_header)

#define FPLACE_CODE (1000+0)
MONOTONE(image_write_header_0)
{
{
extern FILE *os_fopen( const char *f_path, const char *mode );
FILE *f = os_fopen( string_text(path), "r+b" );
UINT_32 n;
#define FILE_HDR_OFFSET (128)

    fseek( f, 0L, SEEK_SET );
    n = SIZEOF_PTR(hdr);
    if (n > FILE_HDR_OFFSET)
        n = FILE_HDR_OFFSET;
    fwrite( PTR_TO_DATAPTR(hdr), 1, n, f );
    fclose(f);
    REG0 = int2fx( n );
    RETURN1();
#undef FILE_HDR_OFFSET
}}
#undef FPLACE_CODE

EPILOGUE(image_write_header)

BEGIN_BACK(image_write_header)
  BACK_MONOTONE(image_write_header_0)
END_BACK(image_write_header)

static struct function_descr image_write_header_descr = {
	&corelib_part_imageio,
	JUMP_TABLE( image_write_header ),
	rsfn_image_write_header_name };
#undef FUNCTION

#undef path
#undef hdr

/********************** Raw glue `image-read-header' **********************/
#define path REG0

static char rsfn_image_read_header_name[] = "image-read-header";
#define FUNCTION rsfn_image_read_header_name

PROLOGUE(image_read_header)

BEGIN_FWD(image_read_header)
  FWD_MONOTONE(image_read_header_0)
END_FWD(image_read_header)

#define FPLACE_CODE (1000+0)
MONOTONE(image_read_header_0)
{
{
extern FILE *os_fopen( const char *f_path, const char *mode );
FILE *f = os_fopen( string_text(path), "r+b" );
long n;
#define FILE_HDR_OFFSET (128)

    fseek( f, 0L, SEEK_SET );
    REG0 = bvec_alloc( FILE_HDR_OFFSET+1, string_class );
    n = fread( PTR_TO_DATAPTR(REG0), 1, FILE_HDR_OFFSET, f );
    fclose(f);
    REG2 = int2fx(n);
    RETURN(2);
#undef FILE_HDR_OFFSET
}}
#undef FPLACE_CODE

EPILOGUE(image_read_header)

BEGIN_BACK(image_read_header)
  BACK_MONOTONE(image_read_header_0)
END_BACK(image_read_header)

static struct function_descr image_read_header_descr = {
	&corelib_part_imageio,
	JUMP_TABLE( image_read_header ),
	rsfn_image_read_header_name };
#undef FUNCTION

#undef path
/******************************* Postamble *******************************/
/**************************** Part Link Table ****************************/


static struct function_descr *(part_imageio_tab[]) = {
    &save_image_descr,
    &load_image_descr,
    &image_write_header_descr,
    &image_read_header_descr,
    NULL };
struct part_descr corelib_part_imageio = {
    186795015,
    &module_corelib,
    part_imageio_tab,
    "imageio",
    0, sccsid };
#undef _MODULE_CORELIB
#undef _SCM_IMAGEIO
#undef _C_IMAGEIO
