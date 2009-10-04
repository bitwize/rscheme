(define-module rs.net.popd ()
  (&module
   ;(meta description: "A POP (RFC 1939) server")
   (import usual-inlines
           rs.sys.threads.manager
           rs.net.md5
           rs.net.uuid
           rs.util.properties
           rs.util.memoize
           tables
           regex
           unixm
           syscalls
           sort
           rs.util.realm
           rs.util.msgs
           rs.util.logfile)
   ;;
   (load "pop.scm")
   ;;
   (export pop-server)
   ;;
   ;;  Pluggable User Interface
   ;;
   (export
    ;;  The GF `get-pop-mailbox' is invoked on the user object
    ;;  returned from the authentication realm, and it should
    ;;  return a vector of <mailbox-entry> objects
    get-pop-mailbox
    ;;
    <mailbox-entry>
    id
    mtime
    uid

    deleted?
    set-deleted?!
    mailbox-entry-content
    commit-deletion)
   ;;
   (load "maildir.scm")
   (export make-pop3-realm)
   ))
           
           
