
(define-macro (define-syscall-glue args . body)
  `(define-safe-glue ,args
     type-handler: (<inet-addr> 
		    (direct-instance? <inet-addr>)
		    ("struct in_addr *~a"
		     "(struct in_addr *)PTR_TO_DATAPTR(~a)"))
     type-handler: (<inet-socket-addr> 
		    (direct-instance? <inet-socket-addr>)
		    ("struct sockaddr_in *~a"
		     "(struct sockaddr_in *)PTR_TO_DATAPTR(~a)"))
     type-handler: (<stat-buf> 
		    (direct-instance? <stat-buf>)
		    ("struct stat *~a"
		     "(struct stat *)PTR_TO_DATAPTR(~a)"))
     type-handler: (<time>
		    (direct-instance? <time>)
		    ("struct scheme_time *~a"
		     "PTR_TO_SCMTIME(~a)"))
     type-handler: (<interval> 
		    (direct-instance? <interval>)
		    ("struct scheme_time *~a"
		     "PTR_TO_SCMTIME(~a)"))
     ,@body))
