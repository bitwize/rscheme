
(define (pack-using (desc <structure-descriptor>) argv)
  (pack% desc argv
	 (bvec-alloc (structure-type desc)
		     (instance-size desc))
	 0))

(define-safe-glue (pack% (desc <structure-descriptor>) argv 
			 buf 
			 (buf_offset <raw-int>))
  properties: ((other-c-files "packlib.c")
	       (other-h-files "packlib.h"))
{
  unsigned i, n, x;
  x = buf_offset;
  n = SIZEOF_PTR( buf );

  if ( SIZEOF_PTR( argv ) != (SIZEOF_PTR( desc ) - SLOT(2)) )
    {
      scheme_error( "pack: incommensurate argv ~s for ~s", 
                    2, argv, desc );
    }
  if (x + fx2int( gvec_ref( desc, SLOT(1) ) ) > n)
    {
      scheme_error( "pack: not enough room in ~s (starting from ~s) for ~s",
		    3, buf, int2fx( x ), desc );
    }

  n = SIZEOF_PTR( argv );
  for (i = SLOT(0); i < n; i += SLOT(1) )
    {
      obj a = gvec_ref( argv, i );
      unsigned m;

      switch (fx2int( gvec_ref( desc, i + SLOT(2) ) ))
	{
	  case 0: bvec_packn_u8( buf, x, a ); x += 1; break;
	  case 1: bvec_packn_s8( buf, x, a ); x += 1; break;
	  case 2: bvec_packb_u8( buf, x, a ); x += 1; break;
	  case 3: bvec_packb_s8( buf, x, a ); x += 1; break;
	  case 4: bvec_packl_u8( buf, x, a ); x += 1; break;
	  case 5: bvec_packl_s8( buf, x, a ); x += 1; break;

	  case 6:  bvec_packn_u16( buf, x, a ); x += 2; break;
	  case 7:  bvec_packn_s16( buf, x, a ); x += 2; break;
	  case 8:  bvec_packb_u16( buf, x, a ); x += 2; break;
	  case 9:  bvec_packb_s16( buf, x, a ); x += 2; break;
	  case 10: bvec_packl_u16( buf, x, a ); x += 2; break;
	  case 11: bvec_packl_s16( buf, x, a ); x += 2; break;

	  case 12: bvec_packn_u32( buf, x, a ); x += 4; break;
	  case 13: bvec_packn_s32( buf, x, a ); x += 4; break;
	  case 14: bvec_packb_u32( buf, x, a ); x += 4; break;
	  case 15: bvec_packb_s32( buf, x, a ); x += 4; break;
	  case 16: bvec_packl_u32( buf, x, a ); x += 4; break;
	  case 17: bvec_packl_s32( buf, x, a ); x += 4; break;

	  case 18: bvec_packn_u64( buf, x, a ); x += 8; break;
	  case 19: bvec_packn_s64( buf, x, a ); x += 8; break;
	  case 20: bvec_packb_u64( buf, x, a ); x += 8; break;
	  case 21: bvec_packb_s64( buf, x, a ); x += 8; break;
	  case 22: bvec_packl_u64( buf, x, a ); x += 8; break;
	  case 23: bvec_packl_s64( buf, x, a ); x += 8; break;

	  case 24: bvec_packn_f32( buf, x, a ); x += 4; break;
	  case 26: bvec_packb_f32( buf, x, a ); x += 4; break;
	  case 28: bvec_packl_f32( buf, x, a ); x += 4; break;

	  case 30: bvec_packn_f64( buf, x, a ); x += 8; break;
	  case 32: bvec_packb_f64( buf, x, a ); x += 8; break;
	  case 34: bvec_packl_f64( buf, x, a ); x += 8; break;

          case 36: m = SIZEOF_PTR( a );
                   memcpy( ((char *)PTR_TO_DATAPTR( buf ))+x,
                           PTR_TO_DATAPTR( a ),
                           m );
                   x += m;
                   break;
          case 40: m = SIZEOF_PTR( a ) - 1;
                   memcpy( ((char *)PTR_TO_DATAPTR( buf ))+x,
                           PTR_TO_DATAPTR( a ),
                           m );
                   x += m;
                   break;
	  default:
	    scheme_error( "pack: unrecognized code ~s",
			  1, gvec_ref( desc, i + SLOT(2) ) );
	    break;
        }
    }
  REG0 = buf;
  RETURN1();
})

