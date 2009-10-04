(define-class <ipv4-header> (<object>)
  ip-v          ; 0
  ip-hl         ; 1
  ip-tos        ; 2
  ip-len        ; 3
  ip-id         ; 4
  ip-RF         ; 5
  ip-DF         ; 6
  ip-MF         ; 7
  ip-off        ; 8
  ip-ttl        ; 9
  ip-proto      ; 10
  ip-sum        ; 11
  ip-src        ; 12
  ip-dst        ; 13
  ip-opts       ; 14
  checksum)     ; 15

(define-class <ip-parse-error> (<condition>)
  code
  data)

(define-safe-glue (parse-ip-header (ss <substring>))
  literals: ((& <ipv4-header>)
             (& <inet-addr>)
             (& <ip-parse-error>)
             (& <substring>))
  properties: ((other-h-files "<string.h>"))
{
  unsigned char *ptr = PTR_TO_DATAPTR( gvec_ref( ss, SLOT(0) ) );
  unsigned char *lim;
  unsigned short i;
  unsigned long cksum;
  unsigned ip_v, ip_hl, ip_tos, ip_len, ip_id, ip_RF, ip_DF, ip_MF,
           ip_off, ip_ttl, ip_proto, ip_sum, opt_off;
  obj ip_src, ip_dst, ip_opts;

  ptr += fx2int( gvec_ref( ss, SLOT(1) ) );
  lim = ptr + fx2int( gvec_ref( ss, SLOT(2) ) );

  ip_v = (ptr[0] & 0xF0) >> 4;
  ip_hl = (ptr[0] & 0x0F);

  if ((lim <= ptr) || (ip_hl < 5) || ((ptr + ip_hl*4) > lim)) {
    raise_error( make3( TLREF(2), NIL_OBJ, int2fx(7911), ss ) );
  }

  ip_tos = ptr[1];
  ip_len = ((unsigned)ptr[2] << 8) + ptr[3];
  ip_id = ((unsigned)ptr[4] << 8) + ptr[5];
  ip_RF = (ptr[6] & 0x80) ? 1 : 0;
  ip_DF = (ptr[6] & 0x40) ? 1 : 0;
  ip_MF = (ptr[6] & 0x20) ? 1 : 0;
  ip_off = (((unsigned)ptr[6] & 0x1F) << 8) + ptr[7];
  ip_ttl = ptr[8];
  ip_proto = ptr[9];
  ip_sum = (((unsigned)ptr[10]) << 8) + ptr[11];

  ip_src = bvec_alloc( 4, TLREF(1) );
  memcpy( PTR_TO_DATAPTR( ip_src ), &ptr[12], 4 );

  ip_dst = bvec_alloc( 4, TLREF(1) );
  memcpy( PTR_TO_DATAPTR( ip_dst ), &ptr[16], 4 );

  opt_off = &ptr[20] - ptr;
  ip_opts = make4( TLREF(3),
                   gvec_ref( ss, SLOT(0) ),
                   int2fx( fx2int( gvec_ref( ss, SLOT(1) ) ) + opt_off ),
                   int2fx( 4*(ip_hl - 5) ),
                   int2fx( 4*(ip_hl - 5) ) );
  REG0 = gvec_alloc( 16, TLREF(0) );

  gvec_write_init( REG0, SLOT(0), int2fx( ip_v ) );
  gvec_write_init( REG0, SLOT(1), int2fx( ip_hl ) );
  gvec_write_init( REG0, SLOT(2), int2fx( ip_tos ) );
  gvec_write_init( REG0, SLOT(3), int2fx( ip_len ) );
  gvec_write_init( REG0, SLOT(4), int2fx( ip_id ) );
  gvec_write_init( REG0, SLOT(5), rb_to_bo( ip_RF ) );
  gvec_write_init( REG0, SLOT(6), rb_to_bo( ip_DF ) );
  gvec_write_init( REG0, SLOT(7), rb_to_bo( ip_MF ) );
  gvec_write_init( REG0, SLOT(8), int2fx( ip_off ) );
  gvec_write_init( REG0, SLOT(9), int2fx( ip_ttl ) );
  gvec_write_init( REG0, SLOT(10), int2fx( ip_proto ) );
  gvec_write_init( REG0, SLOT(11), int2fx( ip_sum ) );
  gvec_write_init( REG0, SLOT(12), ip_src );
  gvec_write_init( REG0, SLOT(13), ip_dst );
  gvec_write_init( REG0, SLOT(14), ip_opts );

  cksum = 0;
  for (i=0; i<4*ip_hl; i+=2) {
    unsigned short w = ntohs( *(unsigned short *)&ptr[i] );
    if (i == 10) {
      w = 0;
    }
    cksum += w;
  }
  cksum = 0xFFFF ^ ((cksum & 0xFFFF) + (cksum >> 16));

  gvec_write_init( REG0, SLOT(15), int2fx( cksum ) );

  REG1 = make4( TLREF(3),
                gvec_ref( ss, SLOT(0) ),
                int2fx( fx2int( gvec_ref( ss, SLOT(1) ) ) + 4*ip_hl ),
                int2fx( fx2int( gvec_ref( ss, SLOT(2) ) ) - 4*ip_hl ),
                int2fx( fx2int( gvec_ref( ss, SLOT(2) ) ) - 4*ip_hl ) );
  RETURN(2);
})

;;;

(define (process-ip-frame packet)
  ;;(print packet)
  (bind (((h <ipv4-header>) rest (parse-ip-header packet)))
    ;;(print h)
    (if (not (= (checksum h) (ip-sum h)))
        (error "bad IPV4 header checksum"))
    ;;
    (case (ip-proto h)
      ((1)      ; ICMP
       (process-icmp-packet h rest))
      ((6)      ; TCP
       (process-tcp-packet h rest))
      ((17)    ; UDP
       (process-udp-packet h rest))
      (else
       (error "unsupported protocol")))))
