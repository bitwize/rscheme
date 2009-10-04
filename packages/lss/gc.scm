
(define-lss-glue (lss-find-record-on (lss <lss>)
                                     (mask <raw-int>)
                                     (pass <raw-int>))
{
  UINT_32 r;

  r = lss_find_record_on_vol( lss, mask, pass );
  if (r == ~(UINT_32)0)
    REG0 = FALSE_OBJ;
  else
    REG0 = int2fx( r );

  RETURN1();
})

(define-lss-glue (lss-copy-record (dst <lss>) (src <lss>) (rec <raw-int>))
{
  size_t n;

  n = lss_copy_record( dst, src, rec );
  REG0 = int2fx( n );
  RETURN1();
})

(define-lss-glue (lss-move-record (lss <lss>) (vol <raw-int>) (rec <raw-int>))
{
  size_t n;

  n = lss_move_record( lss, vol, rec );
  REG0 = int2fx( n );
  RETURN1();
})

;;;
;;;  The `tip' is the volume where new records (including internal
;;;  structures, such as commit records) are being written.
;;;

(define-lss-glue (lss-set-tip (lss <lss>) (vol <raw-int>))
{
  int rc;

  rc = lss_set_tip( lss, vol );
  RETURN0();
})

(define-lss-glue (lss-get-tip (lss <lss>))
{
  int rc;

  rc = lss_get_tip( lss );
  REG0 = int2fx( rc );
  RETURN1();
})

;;;
;;;  Attaching a volume will create the file (volume),
;;;  and then set it as the current tip.
;;;

(define-lss-glue (lss-attach-vol (lss <lss>) 
                                 (vol <raw-int>) 
                                 (file <raw-string>))
{
  lss_attach_vol( lss, vol, file );
  RETURN0();
})

;;;
;;;  NOTE!  Detaching a volume will implicitly commit the
;;;  LSS store, because new indexing structures may have to
;;;  be written out, and they are present only in the commit
;;;  record.

(define-lss-glue (lss-detach-vol (lss <lss>) (vol <raw-int>))
{
  lss_detach_vol( lss, vol );
  RETURN0();
})

(define-lss-glue (lss-get-generation (lss <lss>))
{
  UINT_32 g = lss_current_generation( lss );
  REG0 = int_64_compact( int_32_to_int_64( g ) );
  RETURN1();
})

(define-lss-glue (lss-set-generation (lss <lss>) (gen <raw-int>))
{
  int rc = lss_set_generation( lss, gen );
  if (rc < 0) {
    scheme_error( "lss-set-generation: could not set ~d", 1, raw_gen );
  }
  RETURN0();
})

(define-lss-glue (lss-get-vol-size (lss <lss>) (vol <raw-int>))
{
  REG0 = int2fx( lss_get_vol_size( lss, vol ) );
  RETURN1();
})

(define-lss-glue (lss-alloc-recs (lss <lss>) (min <raw-int>) (count <raw-int>))
{
  int rc;
  UINT_32 r;

  rc = lss_alloc_recs( lss, min, count, &r );
  if (rc < 0) {
    RETURN0();
  } else {
    REG0 = int2fx( r );
    RETURN1();
  }
})
