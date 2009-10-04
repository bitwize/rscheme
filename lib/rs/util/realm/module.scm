#|------------------------------------------------------------*-Scheme-*--|
 | File:    rs/util/realm/module.scm
 |
 |          Copyright (C) 2003 Donovan Kolbly <donovan@rscheme.org>
 |          as part of the RScheme project, licensed for free use
 |
 | Version: 1.4
 | Date:    2005-09-23 20:14:33
 | Build:   v0.7.3.4-b7u, 2007-05-30
 |
 | Purpose: Common interface for authentication realms, used
 |          by various network services:
 |
 |              rs.net.httpd
 |              rs.net.console
 |
 |          and it can be used for challenge/response services
 |          such as APOP and CRAM-MD5, as by:
 |
 |              rs.net.smtpd
 |              rs.net.popd
 |
 `------------------------------------------------------------------------|#

(define-module rs.util.realm ()
  (&module
   (import usual-inlines
           tables)
   ;;
   (load "basicauth.scm")
   ;;
   (export 
    ;;
    ;;  the abstract interface.  Feel free to subclass
    ;;  and provide special methods for authenticate and 
    ;;  chap-authenticate
    ;;
    <auth-realm>
    ;;
    ;;  a basic-auth-realm has a name and a simple
    ;;  table of users
    ;;
    make-basic-auth-realm
    <basic-auth-realm>
    lookup-realm-user
    ;;
    ;;  given a user, figure out if they need to authenticate
    ;;
    need-password?
    ;;
    ;;  To authenticate a user when presented with a plaintext
    ;;  password, `p', call:
    ;;
    ;;    (authenticate (self <auth-realm>)
    ;;                  (user-name <string>)
    ;;                  (password <string>))
    ;;
    ;;  Returns the user datum that was inserted using 
    ;;  `add-realm-user!'
    ;;
    authenticate
    ;;
    ;;  To authenticate a user in a challenge/response scenario,
    ;;  call:
    ;;
    ;;   (chap-authenticate (self <auth-realm>)
    ;;                      (user-name <string>)
    ;;                      (response <string>)
    ;;                      (hasher <function>))
    ;;
    ;;  In which case `hasher' gets called with the plaintext
    ;;  password, and should compute the expected response string,
    ;;  based on the challenge (which `hasher' should already know)
    ;;
    chap-authenticate
    ;;
    ;;  Add a new user to the realm, 
    ;;
    ;;   (add-realm-user! (self <auth-realm>)
    ;;                    (user-name <string>)
    ;;                    (user-password <string>)
    ;;                    (user-datum <object>))
    add-realm-user!)))
