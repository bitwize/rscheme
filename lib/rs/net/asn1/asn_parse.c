
(define-asn-glue (ber-parse (src <substring>))
  literals: ((& <asn-parse-error>))
{

obj asn_parse(

  unsigned char tag = src_base[src_offset];
  unsigned long len;

  /*  read the TAG octet  */
  
  if (src_offset+1 > src_limit) {
    raise_error( make4( TLREF(0), NIL_OBJ, int2fx(7301), raw_src, FALSE_OBJ ) );
  }
  tag = src_base[src_offset++];

  /*  read the (possibly first) octet of the LENGTH  */
 
  if (src_offset+1 > src_limit) {
    raise_error( make4( TLREF(0), NIL_OBJ, int2fx(7302), raw_src, FALSE_OBJ ) );
  }
  len = src_base[src_offset++];

  /*  read remaining octets, if any, of the LENGTH  */

  if (len & 0x80) {
    switch (len) {

      case 0x81:
           if (src_offset+1 > src_limit) {
             raise_error( make4( TLREF(0), NIL_OBJ, int2fx(7303), raw_src, FALSE_OBJ ) );
           }
           len = src_base[src_offset];
           src_offset += 1;
           break;


      case 0x82:
           if (src_offset+2 > src_limit) {
             raise_error( make4( TLREF(0), NIL_OBJ, int2fx(7304), raw_src, FALSE_OBJ ) );
           }
           len = (src_base[src_offset] << 8) 
               + src_base[src_offset+1];
           src_offset += 2;
           break;

      case 0x83:
           if (src_offset+3 > src_limit) {
             raise_error( make4( TLREF(0), NIL_OBJ, int2fx(7305), raw_src, FALSE_OBJ ) );
           }
           len = (src_base[src_offset] << 16)
               + (src_base[src_offset+1] << 8)
               + (src_base[src_offset+2]);
           src_offset += 3;
           break;

      case 0x84:
           if (src_offset+4 > src_limit) {
             raise_error( make4( TLREF(0), NIL_OBJ, int2fx(7306), raw_src, FALSE_OBJ ) );
           }
           len = (src_base[src_offset] << 24)
               + (src_base[src_offset+1] << 16)
               + (src_base[src_offset+2] << 8);
               + (src_base[src_offset+3]);
           src_offset += 4;
           break;

      case 0x80:
           /* this should not happen */
           raise_error( make4( TLREF(0), NIL_OBJ, int2fx(7307), raw_src, FALSE_OBJ ) );

      default:
           /* how many octets is the length going to be!? */
           /* theoretically, we should handle this case, since they are
              allowed to use more octets of encoding than are required */
           raise_error( make4( TLREF(0), NIL_OBJ, int2fx(7308), raw_src, FALSE_OBJ ) );
      }
   }
   REG0 = int2fx( tag );
   REG1 = int2fx( len );

   switch (tag) {
     case 0x02:   /* INTEGER */
     case 0x04:   /* OCTET STRING */
     case 0x05:   /* NULL */
     case 0x30:   /* SEQUENCE */
     default:
        raise_error( make4( TLREF(0), NIL_OBJ, int2fx(7311), raw_src, 
                            int2fx( tag ) ) );
   }

   RETURN(2);
})
