


(define-safe-glue (gen-ip-header (self <ipv4-header>))
{
  obj opts = gvec_ref( self, SLOT(14) );
  obj hdr;
  unsigned ip_hl;
  unsigned char *ptr;
  unsigned short i;
  unsigned long cksum;

  ip_hl = 5 + (string_length( opts ) + 3) / 4;

  hdr = bvec_alloc( 4*ip_hl + 1, string_class );
  ptr = PTR_TO_DATAPTR( hdr );

  if (ip_hl > 15) {
    scheme_error( "ipv4 header too big", 0 );
  }

  ptr[0] = (fx2int( gvec_ref( self, SLOT(0) ) ) << 4) + ip_hl;
  ptr[1] = fx2int( gvec_ref( self, SLOT(2) ) );

  ptr[2] = fx2int( gvec_ref( self, SLOT(3) ) ) >> 8;
  ptr[3] = fx2int( gvec_ref( self, SLOT(3) ) ) & 0xFF;

  ptr[4] = fx2int( gvec_ref( self, SLOT(4) ) ) >> 8;
  ptr[5] = fx2int( gvec_ref( self, SLOT(4) ) ) & 0xFF;

  ptr[6] = (fx2int( gvec_ref( self, SLOT(8) ) ) >> 8) & 0x1F;
  if (truish( gvec_ref( self, SLOT(5) ) )) {
    ptr[6] |= 0x80;
  }
  if (truish( gvec_ref( self, SLOT(6) ) )) {
    ptr[6] |= 0x40;
  }
  if (truish( gvec_ref( self, SLOT(7) ) )) {
    ptr[6] |= 0x20;
  }
  ptr[7] = fx2int( gvec_ref( self, SLOT(8) ) ) & 0xFF;
  ptr[8] = fx2int( gvec_ref( self, SLOT(9) ) );
  ptr[9] = fx2int( gvec_ref( self, SLOT(10) ) );
  ptr[10] = ptr[11] = 0;

  memcpy( &ptr[12], PTR_TO_DATAPTR( gvec_ref( self, SLOT(12) ) ), 4 );
  memcpy( &ptr[16], PTR_TO_DATAPTR( gvec_ref( self, SLOT(13) ) ), 4 );
  memcpy( &ptr[20], PTR_TO_DATAPTR( opts ), string_length( opts ) );

  cksum = 0;
  for (i=0; i<4*ip_hl; i+=2) {
    unsigned short w = ntohs( *(unsigned short *)&ptr[i] );
    cksum += w;
  }
  cksum = ~(cksum + (cksum >> 16));
  ptr[10] = cksum >> 8;
  ptr[11] = cksum & 0xFF;

  REG0 = hdr;
  RETURN1();
})

(define (t)
  (gen-ip-header
   (make <ipv4-header>
         ip-v: 4
         ip-hl: 5
         ip-tos: 0
         ip-len: 0
         ip-id: 0
         ip-RF: #f
         ip-DF: #f
         ip-MF: #f
         ip-off: 0
         ip-ttl: 0
         ip-proto: 0
         ip-sum: 0
         ip-src: (string->inet-addr "10.0.0.2")
         ip-dst: (string->inet-addr "10.0.0.1")
         ip-opts: ""
         checksum: 0)))
