,(use syscalls)

(define (tun)
  (let ((fd (fd-open "/dev/net/tun" #b10 0)))
    (values fd (configure-tun-device fd #f))))
   
(define-safe-glue (configure-tun-device (fd <raw-int>) name)
  properties: ((other-c-files "tuntap.c"))
{
  extern int rs_tun_setup( int fd, char *dev_in, char *dev_out );
  int rc;
  char *d_in = NULL, d_out[1024];

  if (STRING_P(name)) {
    d_in = string_text( name );
  }

  rc = rs_tun_setup( fd, d_in, d_out );
  REG0 = make_string( d_out );
  RETURN1();
})

(define *tunfd* #f)

(define (g)
  (bind ((fd dev (tun))
         (buf (make-string 65536)))
    (format #t "[Listening on ~s]\n" dev)
    (set! *tunfd* fd)
    (let loop ()
      (let ((n (fd-read fd buf 0 (string-length buf))))
        (if (> n 0)
            (begin
              (format #t "Received ~d bytes...\n" n)
              (process-ethernet-frame (substring* buf 0 n))
              (loop)))))))

(define (XMIT (p <string>))
  (let ((n (fd-write *tunfd* p 0 (string-length p))))
    (if n
        (begin
          (format #t "(wrote ~s)\n" n)
          (print p))
        (let ((e (errno)))
          (format #t "bummer: ~s: ~a\n" e (errno-message e))))))
  
#|
        ifconfig tun0 10.0.0.1 pointopoint 10.0.0.2 up
|#



(load "ethernet.scm")
(load "ip.scm")
(load "icmp.scm")
