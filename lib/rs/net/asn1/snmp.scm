,(use rs.sys.threads.manager)

;(define s (open-udp-socket 161))
  
(define (EXPAND s)
  (vector->values (ber-parse (to-substring s))))

(define (EXPAND* s)
  (vector->values (ber-parse-sequence (to-substring s))))

(define (process-next s)
  (bind ((pkt peer (receive-packet s)))
    (process-snmp pkt)))

(define (process-snmp packet)
  (bind ((ver community pdu (EXPAND packet)))
    (case (tag pdu)
      ((#xA0)           ; GET-REQUEST
       (bind ((request-id err-status err-index vars (EXPAND* (contents pdu)))
              (result (make-dequeue)))
         (format #t "request id: ~s\n" request-id)
         ;;
         (vector-for-each
          (lambda (v)
            (format #t "  var ~s = ~s\n" (vector-ref v 0) (vector-ref v 1))
            (dequeue-push-back! result (vector (vector-ref v 0)
                                               "Foo-System-Bar"))
            (values))
          vars)
         (ber-encode
          (vector 0 
                  community 
                  (make <asn-other-field>
                        tag: #xA2                ; GET-RESPONSE
                        contents: (string-append
                                   (ber-encode request-id)
                                   (ber-encode 0)
                                   (ber-encode 0)
                                   (ber-encode (dequeue-state result)))))))))))
