#include <openssl/des.h>
#include <rscheme.h>

obj rs_des_make_key_schedule( obj key )
{
  obj sched;
  int rc;

  sched = bvec_alloc( sizeof( DES_key_schedule ), byte_vector_class );
  
  rc = DES_set_key_checked( (DES_cblock *)PTR_TO_DATAPTR( key ), 
                            (DES_key_schedule *)PTR_TO_DATAPTR( sched ) );
  if (rc < 0) {
    if (rc == -2) {
      scheme_error( "DES_set_key_checked: weak key: ~s", 1, key );
    } else if (rc == -1) {
      scheme_error( "DES_set_key_checked: parity error: ~s", 1, key );
    } else {
      scheme_error( "DES_set_key_checked: error ~d", 1, int2fx( rc ) );
    }
    return FALSE_OBJ;
  } else {
    return sched;
  }
}


/*DES_ede3_cbc_encrypt*/
