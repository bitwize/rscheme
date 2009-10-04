;;;
;;;  decompression
;;;

;;; these could save a buffer copy, too, by writing directly
;;; into the SOP buffers...

(define-zlib-glue (bvec-uncompress dest
				   src
				   (src_off <raw-int>) 
				   (src_len <raw-int>))
{
  extern void SOP_write( obj port, const char *src, UINT_32 len );
  z_stream strm;
  int rc;
  void *end_ptr;
  Bytef temp[1000];
  INT_32 zipped = 0;

  strm.zalloc = Z_NULL;
  strm.zfree = Z_NULL;
  strm.opaque = Z_NULL;

  rc = inflateInit( &strm );
  assert( rc == Z_OK );

  strm.avail_in = src_len;
  strm.next_in = src_off + (Bytef *)PTR_TO_DATAPTR( src );

  do {
    strm.avail_out = 1000;
    strm.next_out = temp;
    rc = inflate( &strm, Z_SYNC_FLUSH );
    if ((rc == Z_OK) || (rc == Z_STREAM_END))
      {
        zipped += strm.next_out - temp;
        SOP_write( dest, temp, strm.next_out - temp );
      }
    else
      {
        scheme_error( "bvec-uncompress: error ~d", 1, int2fx( rc ) );
      }
  } while (rc != Z_STREAM_END);
  inflateEnd( &strm );
  REG0 = int2fx( zipped );
  RETURN1();
})

(define-method uncompress ((self <string>))
  (let ((o (open-output-string)))
    (bvec-uncompress o self 0 (string-length self))
    (close-output-port o)))

(define-method uncompress ((self <byte-vector>))
  (let ((o (open-output-string)))
    (bvec-uncompress o self 0 (bvec-length self))
    (close-output-port o)))


;;;
;;;  compression
;;;

(define-zlib-glue (bvec-compress dest
				 src
				 (src_off <raw-int>)
				 (src_len <raw-int>)
				 level)
{
  extern void SOP_write( obj port, const char *src, UINT_32 len );
  z_stream strm;
  int rc;
  void *end_ptr;
  Bytef temp[1000];
  INT_32 zipped = 0;
  int lvl = Z_DEFAULT_COMPRESSION;

  if (FIXNUM_P(level))
    {
      lvl = fx2int( level );
      if (lvl < Z_NO_COMPRESSION)
        lvl = Z_NO_COMPRESSION;
      else if (lvl > Z_BEST_COMPRESSION)
        lvl = Z_BEST_COMPRESSION;
    }

  strm.zalloc = Z_NULL;
  strm.zfree = Z_NULL;
  strm.opaque = Z_NULL;

  rc = deflateInit( &strm, lvl );
  assert( rc == Z_OK );

  strm.avail_in = src_len;
  strm.next_in = src_off + (Bytef *)PTR_TO_DATAPTR( src );

  do {
    strm.avail_out = 1000;
    strm.next_out = temp;
    rc = deflate( &strm, Z_FINISH );
    if ((rc == Z_OK) || (rc == Z_STREAM_END))
      {
        zipped += strm.next_out - temp;
        SOP_write( dest, temp, strm.next_out - temp );
      }
    else
      {
        scheme_error( "bvec-compress: error ~d", 1, int2fx( rc ) );
      }
  } while (rc != Z_STREAM_END);
  deflateEnd( &strm );
  REG0 = int2fx( zipped );
  RETURN1();
})

;;;  these could be optimized to handle the short-string cases
;;;  more cheaply (ie, using a stack buffer inside bvec-compress),
;;;  since the expansion ratio is bounded (see zlib.h)

(define-method compress ((self <string>) #key 
			                 (level default: #f)
					 (into default: #f))
  (if into
      (bvec-compress into self 0 (string-length self) level)
      (let ((o (open-output-string)))
	(bvec-compress o self 0 (string-length self) level)
	(close-output-port o))))

(define-method compress ((self <byte-vector>) #key 
			                      (level default: #f)
					      (into default: #f))
  (if into
      (bvec-compress into self 0 (bvec-length self) level)
      (let ((o (open-output-string)))
	(bvec-compress o self 0 (bvec-length self) level)
	(close-output-port o))))

