(define-module rs.net.soap ()
  (&module
   (import usual-inlines
           syscalls
           calendar
           regex
           unixm
           rs.net.md5
           tables
           rs.sys.threads.manager
           util.xml
           util.xpath

           rs.net.pem
           rs.sys.compression
           )
   ;;
   (load "http.scm")
   (load "soap-ns.scm")
   (load "encoding.scm")
   (load "soap-client.scm")
   ;;
   (export with-soap-session
           make-soap-binding
           open-soap-session
           soap-session-live?
           <soap-error>

           soap-encode-value
           soap-decode-value

           soap-rpc
           base64-gzip-decode
           base64-gzip-encode)))
