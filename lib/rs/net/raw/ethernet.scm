,(use rs.util.pack)


(define (process-ethernet-frame packet)
  (with-unpack 
   (to-string packet) (u16/b: flags u16/b: proto)
   (case proto
     ((#x800)
      (process-ip-frame (substring* packet 4 (string-length packet))))
     (else
      (error "Unknown ethernet frame type: #x~04x" proto)))))


(define (send-ip-packet (packet <string>))
  (XMIT (string-append (pack-string u16/b: 0 u16/b: #x800) packet)))
                       
