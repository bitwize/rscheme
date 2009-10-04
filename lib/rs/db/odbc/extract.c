#include <stdio.h>
#include "/u/donovan/w/dev/rs/db/odbc/extract.h"

/***
 *
 *       <extraction-plan>
 *      +------------------+
 *   0  | properties       |
 *   1  | result-class  *--+---> #{the class <bob>}
 *   2  | tuple buffer  *--+---> #[<byte-vector>]
 *   3  | instructions  *--+---> #u8(0 2 2 1 0) ; 0=>use literal value
 *   4  | sql-stmt      *--+---> #[<hstmt>]
 *   5  | literal value    |
 *   :  :                  :
 *      +------------------+
 *
 **/

#define GET_PLAN_RESULT_CLASS(plan)   gvec_ref( plan, SLOT(1) )
#define GET_PLAN_TUPLE_BUFFER(plan)   gvec_ref( plan, SLOT(2) )
#define GET_PLAN_INSTRUCTIONS(plan)   gvec_ref( plan, SLOT(3) )
#define GET_PLAN_LITERAL(plan,k)      gvec_ref( plan, SLOT(5+(k)) )

obj compile_odbc_extraction_plan( obj meta )
{
  scheme_error( "compile_odbc_extraction_plan(): not implemented natively",
		1, meta );
  return FALSE_OBJ;
}

obj run_odbc_extraction_plan( HSTMT hstmt, obj plan )
{
  unsigned bufix, i, n, litnum;
  UINT_8 *insns;
  obj item;
  obj val = ZERO;
  SDWORD *len_list;
  UINT_8 *data_base;

  insns = PTR_TO_DATAPTR( GET_PLAN_INSTRUCTIONS( plan ) );
  n = SIZEOF_PTR( GET_PLAN_INSTRUCTIONS( plan ) );

  /*printf( "%u slots\n", n );*/

  item = alloc( SLOT(n), GET_PLAN_RESULT_CLASS( plan ) );
  litnum = 0;
  bufix = 0;

  data_base = PTR_TO_DATAPTR( GET_PLAN_TUPLE_BUFFER( plan ) );
  len_list = (SDWORD *)data_base;

  for (i=0; i<n; i++)
    {
      /*printf( "insn: %d\n", insns[0] );*/
      switch (*insns++)
	{
	case 0:  /* literal value */
	  val = GET_PLAN_LITERAL( plan, litnum );
	  litnum++;
	  break;

	  /* how to check for null values? */
	case 1:  /* default (ie, a string) */
	  {
	    UINT_8 *data_ptr = data_base + len_list[0];
	    /*printf( "string(%ld) @ %ld\n", len_list[1], len_list[0] );*/

	    val = bvec_alloc( len_list[1] + 1, string_class );
	    memcpy( PTR_TO_DATAPTR(val), data_ptr, len_list[1] );
	    len_list += 2;
	    break;
	  }

	case 2:  /* int */
	  {
	    void *data_ptr = data_base + len_list[0];
	    /*printf( "long(%ld) @ %ld = %ld\n", 
		    len_list[1], len_list[0],
		    *(long *)data_ptr ); */
	    val = int2fx( *(long *)data_ptr );
	    len_list += 2;
	    break;
	  }
	}
      gvec_write_init( item, SLOT(i), val );
    }
  return item;
}
