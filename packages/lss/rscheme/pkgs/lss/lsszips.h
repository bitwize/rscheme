#ifndef _H_LSSZIPS
#define _H_LSSZIPS

/*  empty entries are prohibited.  
 *  the end of a vector marked with NULL `ptr'
 */

typedef struct zipbuf {
  void  *ptr;
  void  *limit;
} zipbuf;

typedef struct zip_algorithm zip_algorithm;

/*  note the client can supply multiple buffers worth of data
 *  (like a `writev')
 */

typedef void *lss_zip_proc( zip_algorithm *self,
			    void *dest, 
			    zipbuf *src );

/*  note that we require the client to know how much content to expect
 *  (ie, the copy length is controlled by the destination size and
 *  not the source size)
 *  and they can provide multiple buffers for it (like a `readv').
 *
 *  The LSS will tell the client what the uncompressed size is, so
 *  that's not a major issue.
 */

typedef void *lss_unzip_proc( zip_algorithm *self,
			      zipbuf *dest, 
			      void *src );

struct zip_algorithm {
  /* a symbolic name for linking or whatever */
  const char *name;

  /* some info in case these algorithms are instantiated dynamically */
  void *info;

  /* the compression method */
  lss_zip_proc *zip_meth;

  /* the decompression method */
  lss_unzip_proc *unzip_meth;
};

/* returns NULL if not found */
zip_algorithm *lss_find_zip_algorithm( const char *name );

/* register a new algorithm */

void lss_register_zip_algorithm( zip_algorithm *alg );

/* add up the lengths of the parts */

size_t zipbuf_len( zipbuf *vec );

/* a well-known algorithm -- the null one, aka `memcpy'! */

extern zip_algorithm lss_null_zip;

/*  an upper bound on the space required by all compression algorithms,
 *  as a function of the input size.  Somewhat arbitrary -- adjust
 *  as algorithms are invented, or make it dynamic (per alg.)
 */

#define MAX_ZLEN(len)  ((2*(len))+100)

#endif /* _H_LSSZIPS */

