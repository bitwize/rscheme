(define-module rs.net.dns ()
  (&module
   (import usual-inlines)
   ;;
   (load "lang.scm")
   (load "byte-string.scm")
   (load "type-class.scm")
   (load "query.scm")
   (load "rr.scm")
   (load "message.scm")
   (load "qrm.scm")
   (load "encode-ip.scm")
   (load "encode-label.scm")
   (load "encode-query.scm")
   (load "encode-rr.scm")
   (load "encode-domain-name.scm")
   (load "encode-message.scm")
   (load "cache.scm")
   (load "parse-message.scm")
   (load "ask-server.scm")
   (load "recursive-query.scm")
   (load "caching-nameserver.scm")
   (load "simple-non-blocking.scm")
   (load "config.scm")
   ;;
   (load "client-side.scm")
   ;;
   (export start-dns-server     ; DNS server
           dns-query            ; DNS client
           
           ;; support for being authoritative
           <soa>
           <mx>
           <hinfo>
           soa-rr?
           cache-add-rr
           cache-add-rr/authoritative
           cache-add-zone
           dns-set
           make-rr)))
