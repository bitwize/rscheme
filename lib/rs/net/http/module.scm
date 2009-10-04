#|------------------------------------------------------------*-Scheme-*--|
 | File:    %p%
 |
 |          Copyright (C) 2003 Donovan Kolbly <donovan@rscheme.org>
 |          as part of the RScheme project, licensed for free use
 |
 | Version: %I%
 | Date:    %E% %U%
 | Build:   %b%
 |
 | Purpose: HTTP Client Support
 |
 `------------------------------------------------------------------------|#

(define-module rs.net.http ()
  (&module
   (import usual-inlines
           regex
           rs.sys.threads.manager
           syscalls
           rs.util.properties
           rs.util.msgs
           rs.net.httpd)
   ;;
   (load "client.scm")
   ;;
   (export open-http-client

           response-code
           response-msg
           read-content
           open-content
           add-http-client-header
           update-cookies-from

           http-get     
           http-put
           http-post
           
           <http-non-success-response>
           <http-error-response>
           <http-redirect-response>
           response
           request-type
           response-content

           )
   ;;
   (import regex)
   (load "escapes.scm")
   (load "url.scm")
   (export string->url
           expand-relative-url
           url->string
           url-local->string)
   ;;
   ;;  at last, the high-level interface
   ;;
   (load "pool.scm")
   (export url-pool-clean
           url-get)))

