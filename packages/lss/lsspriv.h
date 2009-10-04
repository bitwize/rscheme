#define LSS_MAGIC  (0x5c4c5353)  /* '\\LSS' */

#include <rscheme/pkgs/lss/lss.h>
#include <rscheme/pkgs/lss/lsserrno.h>

#undef _rs_volatile
#define _rs_volatile

struct LSSMethods {
  char *implementation;
  void (*close_meth)( LSS *l );
  UINT_32 (*commit_meth)( LSS *l, int flag );

  /* read methods */
  LSSAccess *(*read_access_meth)( LSS *lss, UINT_32 recnum );
  void (*readv_meth)( LSS *lss, zipbuf *vec, LSSAccess *a );
  void (*read_release_meth)( LSS *lss, LSSAccess *a );

  /* write methods */
  void (*writev_meth)( LSS *lss, UINT_32 recnum, zipbuf *vec,
		       zip_algorithm *use );

  /* introspection methods */
  UINT_32 *(*get_index_meth)( LSS *lss, UINT_32 *cnt );
  void (*get_record_info_meth)( LSS *lss, UINT_32 rec, 
				struct LSSRecordInfo *info );

  /* read/write */
  size_t (*copy_record_meth)( LSS *src, LSS *dst, UINT_32 rec );
  void (*raw_write_meth)( LSS *lss, UINT_32 rec, 
			  void *data, size_t unz_len,
			  zip_algorithm *use,
			  size_t z_len );
  const char *(*filename_meth)( LSS *lss, int vol_num );
  UINT_32 (*find_vrecord_meth)( LSS *lss, unsigned mask, int pass );
  int (*alloc_recs_meth)( LSS *lss, unsigned min, unsigned count, 
                          UINT_32 *rec );
  int (*delete_meth)( LSS *lss, unsigned rec );
};

struct LSS {
  struct LSSMethods   *fn;
  void                *client_info;
  lss_error_handler_t *error_handler;
  int                  writable;
  /* version-specific content goes here... */
};

#define LSS_INIT_COM(x,fv) do { (x).fn = fv; \
	                        (x).client_info = NULL; \
	                        (x).error_handler = NULL; \
			      } while (0)
							  

_rs_volatile void lssi_signal_error( LSS *lss, int code, char *fmt, ... );
_rs_volatile void lssi_sys_error( LSS *lss, char *fmt, ... );

LSS *lssv2_open( const char *file, int fd, int writable, UINT_32 CR_offset );
LSS *lssv3_open( const char *file, int fd, int writable, int gen );

LSS *lssv3_create( const char *file, int filemode );
LSS *lssv3_extend( const char *file, int filemode,
		   const char *from, int gen );

UINT_32 lssi_hash( UINT_32 key ); /* a CRC-based hash */

