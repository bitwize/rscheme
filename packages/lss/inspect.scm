#|------------------------------------------------------------*-Scheme-*--|
 | File:	    packages/lss/inspect.scm
 |
 |          Copyright (C)2000 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |	    See <http://www.rscheme.org/> for the latest info.
 |
 | File version:     1.3
 | File mod date:    2005-02-16 14:56:47
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  lss
 |
 | Purpose:          Probe and query the record index of an LSS
 `------------------------------------------------------------------------|#

;;;
;;;  returns the record location in ( volume . byteoffset ) form
;;;

(define-lss-glue (lss-record-info (lss <lss>) (rec <raw-int>))
{
  struct LSSRecordInfo info;

  lss_get_record_info( lss, rec, &info );
  REG0 = cons( int2fx( info.volume ), int2fx( info.offset ) );
  RETURN1();
})

;;;  `from' and `to' are in generic volume locator form
;;;  i.e., ( volume . byte-offset )  This is converted to a
;;;  LSS V3 storage locator which is:
;;;
;;;   +--------+------------------------+
;;;   |  vol#  |   granule offset       |
;;;   +--------+------------------------+

(define-lss-glue (lss-record-query (lss <lss>) (from <pair>) (to <pair>))
{
  UINT_32 *qr, froml, tol;
  int n;

  froml = (fx2int( pair_car( from ) ) << 28) 
        + (fx2int( pair_cdr( from ) ) >> 4);
  tol = (fx2int( pair_car( to ) ) << 28) 
      + (fx2int( pair_cdr( to ) ) >> 4);

  n = lss_record_query( lss, froml, tol, &qr );
  if (n < 0) {
    REG0 = FALSE_OBJ;
  } else if (n == 0) {
    REG0 = gvec_alloc( SLOT(0), vector_class );
  } else {
    obj v = gvec_alloc( n, vector_class );
    int i;

    for (i=0; i<n; i++) {
      gvec_write_fresh_non_ptr( v, SLOT(i), int2fx( qr[i] ) );
    }
    free( qr );
    REG0 = v;
  }
  RETURN1();
})
