(define-module rs.util.logfile ()
  (&module
   (import usual-inlines
           rs.sys.threads.manager
           syscalls
           paths
           rs.util.msgs)
   (load "logfiles.scm")
   (export <log-fileset>
           open-log-fileset
           log-file-message-dest
           with-output-to-log-file)))

   
