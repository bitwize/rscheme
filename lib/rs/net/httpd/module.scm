(define-module rs.net.httpd ()
  (&module
   (import usual-inlines
           rs.sys.threads.manager
           rs.util.properties
           rs.util.msgs
           paths
           syscalls
           calendar                ; need string->time for if-modified-since
           unixm
           regex
           sort
           rs.net.html
           rs.net.pem
           rs.net.nvt
           util.xml
           rs.db.rstore
           rs.util.realm
           rs.util.logfile))
  ;;
  (define-message-table rs.net.httpd 542)
  ;;
  (&module
   ;;
   (load "webspace.scm")
   (load "raw.scm")
   (load "server.scm")
   (load "http.scm")
   (load "xslt.scm")
   (load "basicauth.scm")               ; bonus feature
   (load "cookiesession.scm")           ; bonus feature
   ;;
   (export make-web-space
           import-webspace
           make-uri-cached-raw
           make-uri-directory
           make-uri-complete-script     ; must set up response structure
           make-uri-simple-script       ; returns content and optional type
           make-uri-simple-rsp-script   ; same, but gets rsp instead of req
           make-uri-disk-node
           make-uri-disk-dir
           make-uri-literal-node
           make-uri-redirect
           make-uri-union-space
           can-handle?          ; invoked on members of a union space
           make-uri-pattern-directory
           make-uri-dynamic-directory
           make-uri-post-form
           ;;
           uri
           dispatch-uri         ; esp. for re-entering dispatch subsystem
           redirection          ; redirection return from script
           <redirection>
           decode-request-query
           encode-request-query
           url-encode-step
           url-decode-step
           ;;
           response->request
           peer
           ;;
           make-auth-area
           ;make-basic-auth-realm
           ;add-realm-user!
           ;authenticate                 ; REALM NAME PASSWORD => #f | TOKEN
           check-authentication         ; lower level than a uri-auth node
           ;;
           make-cookie-session-area
           cookie-key
           set-cookie-key!
           current-cookie-session
           <cookie-manager>
           ;;
           web-session-create           ; / cookie manager protocol
           web-session-access           ; \
           web-session-cookie           ; related...
           with-cookie-reset
           ;;
           <uri-node>
           uri-link-add!
           uri-link-remove!
           ;;
           guess-mime-type
           ;;
           permit-listing?
           set-permit-listing?!
           mime-type
           set-mime-type!
           disk-file-path
           set-disk-file-path!
           ;;
           file->content
           sxml->content
           ;;
                                        ;  _
           send-content                 ; / 
           content-mtime                ; |   response content
           content-length               ; \_  protocol
           ;;
           clear-static-max-age
           set-static-max-age!
           start-http-server
           handle-http-socket-connection)
   ;;
   ;;  should these be exported...?
   ;;
   (export generate-error-response/not-found
           generate-error-response/internal
           generate-redirect-response
           add-http-service-method
           ;;
           http-error-context->sxml
           set-content-and-type!
           set-result!
           clear-header-field!
           set-header-field!
           parse-fields->table
           parse-multi-fields->table
           ;; ideally through a different module view...
           <port-content>
           http-read-headers
           read-content
           open-content
           current-http-connection
           underlying-http-socket
           request-message-buffer
           request-number
           parse-request-cookies
           suppress-body-content!
           )))
           
