
(define (unpack-using desc buf #optional (offset default: 0))
  (unpack% desc buf offset))

(define-safe-glue (unpack% (desc <structure-descriptor>)
			   buf 
			   (buf_offset <raw-int>))
  properties: ((other-c-files "packlib.c")
	       (other-h-files "packlib.h"))
{
  unsigned i, n, x;
  obj desc_reg = desc;
  obj buf_reg = buf;
  int r;

  n = fx2int( gvec_ref( desc, SLOT(1) ) );
  x = buf_offset;

  if ((n + x) > SIZEOF_PTR( buf ))
    {
      scheme_error( "unpack: buffer ~s doesn't have enough data for ~s",
		    2, buf, desc );
    }
#if 0
  if (!BVEC_P( buf ))
    {
      scheme_error( "unpack: buffer ~s not a bvec", 1, buf );
    }
#endif
  n = SIZEOF_PTR( desc ) - SLOT(2);
  r = 0;
  for (i = SLOT(0); i < n; i += SLOT(1) )
    {
      obj v;
      switch (fx2int( gvec_ref( desc_reg, i + SLOT(2) ) ))
	{
	  case 0: v = bvec_unpackn_u8( buf_reg, x ); x += 1; break;
	  case 1: v = bvec_unpackn_s8( buf_reg, x ); x += 1; break;
	  case 2: v = bvec_unpackb_u8( buf_reg, x ); x += 1; break;
	  case 3: v = bvec_unpackb_s8( buf_reg, x ); x += 1; break;
	  case 4: v = bvec_unpackl_u8( buf_reg, x ); x += 1; break;
	  case 5: v = bvec_unpackl_s8( buf_reg, x ); x += 1; break;

	  case 6: v =  bvec_unpackn_u16( buf_reg, x ); x += 2; break;
	  case 7: v =  bvec_unpackn_s16( buf_reg, x ); x += 2; break;
	  case 8: v =  bvec_unpackb_u16( buf_reg, x ); x += 2; break;
	  case 9: v =  bvec_unpackb_s16( buf_reg, x ); x += 2; break;
	  case 10: v = bvec_unpackl_u16( buf_reg, x ); x += 2; break;
	  case 11: v = bvec_unpackl_s16( buf_reg, x ); x += 2; break;

	  case 12: v = bvec_unpackn_u32( buf_reg, x ); x += 4; break;
	  case 13: v = bvec_unpackn_s32( buf_reg, x ); x += 4; break;
	  case 14: v = bvec_unpackb_u32( buf_reg, x ); x += 4; break;
	  case 15: v = bvec_unpackb_s32( buf_reg, x ); x += 4; break;
	  case 16: v = bvec_unpackl_u32( buf_reg, x ); x += 4; break;
	  case 17: v = bvec_unpackl_s32( buf_reg, x ); x += 4; break;

	  case 18: v = bvec_unpackn_u64( buf_reg, x ); x += 8; break;
	  case 19: v = bvec_unpackn_s64( buf_reg, x ); x += 8; break;
	  case 20: v = bvec_unpackb_u64( buf_reg, x ); x += 8; break;
	  case 21: v = bvec_unpackb_s64( buf_reg, x ); x += 8; break;
	  case 22: v = bvec_unpackl_u64( buf_reg, x ); x += 8; break;
	  case 23: v = bvec_unpackl_s64( buf_reg, x ); x += 8; break;

	  case 24: v = bvec_unpackn_f32( buf_reg, x ); x += 4; break;
	  case 26: v = bvec_unpackb_f32( buf_reg, x ); x += 4; break;
	  case 28: v = bvec_unpackl_f32( buf_reg, x ); x += 4; break;

	  case 30: v = bvec_unpackn_f64( buf_reg, x ); x += 8; break;
	  case 32: v = bvec_unpackb_f64( buf_reg, x ); x += 8; break;
	  case 34: v = bvec_unpackl_f64( buf_reg, x ); x += 8; break;
	  default:
	    scheme_error( "pack: unrecognized code ~s",
			  1, gvec_ref( desc, i + SLOT(2) ) );
	    v = ZERO;
	    break;
        }
	reg_set( r, v );
	r++;
    }
  if (r == 0) {
    RETURN0();
  } else {
    RETURN(r);
  }
})
