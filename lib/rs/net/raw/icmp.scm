
;;; c.f. RFC 792 - Internet Control Message Protocol

(define (process-icmp-packet h rest)
  (case (octet-ref rest 0)
    ((8) (process-icmp-echo-packet h rest))
    ((0) (process-icmp-echo-reply-packet h rest))
    (else
     (error "unsupported ICMP packet"))))

(define (process-icmp-echo-packet h rest)
  (with-unpack
   (to-string rest)
   (u8: type u8: code u16/b: checksum u16/b: id u16/b: seq)
   (format #t "ECHO Type <~d> Code <~d> Checksum <~04x> id <~04x> seq <~04x>\n"
           type
           code
           checksum
           id
           seq)
   (format #t "                       Computed <~04x>\n"
           (simple-checksum (string-append
                             (substring (to-string rest) 0 2)
                             (substring (to-string rest) 4))))
   ;;
   (let* ((icmp (make-icmp-echo-packet 
                 type: 0
                 id: id
                 seq: seq
                 data: (substring* rest 8 (string-length rest))))
          (ip (make <ipv4-header>
                    ip-v: 4
                    ip-hl: 5
                    ip-tos: 0
                    ip-len: (+ 20 (string-length icmp))
                    ip-id: #xCAFE
                    ip-RF: #f
                    ip-DF: #f
                    ip-MF: #f
                    ip-off: 0
                    ip-ttl: 64
                    ip-proto: 1
                    ip-sum: 0
                    ip-src: (ip-dst h)
                    ip-dst: (ip-src h)
                    ip-opts: (empty-string)
                    checksum: 0)))
     (let ((p (string-append (gen-ip-header ip) icmp)))
       (print p)
       (send-ip-packet p)))))



(define (make-icmp-echo-packet #key 
                               type
                               (code default: 0)
                               (id default: 0)
                               (seq default: 0)
                               (data default: ""))
  (let* ((p (string-append (pack-string u8: type
                                        u8: code
                                        u16: 0
                                        u16/b: id
                                        u16/b: seq)
                           (to-string data)))
         (c (simple-checksum p)))
    (print p)
    (bvec-set! p 2 (logical-shift-right c 8))
    (bvec-set! p 3 (bitwise-and c #xFF))
    (print p)
    p))

(define-method simple-checksum ((self <string>))
  (simple-checksum/string self))

(define-glue (simple-checksum/string s)
{
  unsigned long sum = 0;
  unsigned char *ptr = PTR_TO_DATAPTR( s );
  unsigned char *lim = ptr + SIZEOF_PTR( s ) - 1;

  /* for <string> inputs, we already have the NUL padding
     called for in the checksum algorithm */

  while (ptr < lim) {
    unsigned short w = ntohs( *(unsigned short *)ptr );
    sum += w;
    ptr += 2;
  }
  sum = 0xFFFF ^ (((sum & 0xFFFF) + (sum >> 16)));
  REG0 = int2fx( sum );
  RETURN1();
})

#|
17:55:43.400292 170.20.66.28 > star.westgate.xynthesis.com: icmp: echo request (DF)
                         4500 0054 0000 4000 4001 6261 aa14 421c
                         aa14 4203 0800 9fec 222e 0001 c446 3443
                         3c5a 0100 ffff ffff ffff ffff ffff ffff
                         ffff ffff ffff ffff ffff ffff ffff ffff
                         ffff ffff ffff ffff ffff ffff ffff ffff
                         ffff
17:55:43.400342 star.westgate.xynthesis.com > 170.20.66.28: icmp: echo reply
                         4500 0054 c667 0000 4001 dbf9 aa14 4203
                         aa14 421c 0000 a7ec 222e 0001 c446 3443
                         3c5a 0100 ffff ffff ffff ffff ffff ffff
                         ffff ffff ffff ffff ffff ffff ffff ffff
                         ffff ffff ffff ffff ffff ffff ffff ffff
                         ffff
|#
