
(define-class <sockopt-error> (<condition>))

(define-class <unknown-socket-option> (<sockopt-error>)
  where
  name)

(define-class <sockopt-read-only> (<sockopt-error>)
  name)

(define-class <unknown-socket-level> (<sockopt-error>)
  where
  name)

(define-method display-object ((self <sockopt-read-only>) port)
  (format port "set-socket-option: option ~s is read-only\n" (name self)))

(define-method display-object ((self <unknown-socket-option>) port)
  (format port "~a: unknown socket option: ~s\n" (where self) (name self))
  (format port "-- expected one of: ~j\n" (map car $portable-sockopt-options)))

(define-method display-object ((self <unknown-socket-level>) port)
  (format port "~a: unknown socket level: ~s\n" (where self) (name self))
  (format port "-- expected one of: ~j\n" (map car $portable-sockopt-levels)))
  

;;;  these are mapped to local (machine-specific) values
;;;  by `local-socket-level' and `local-socket-option'

(define $portable-sockopt-options
 '((socket/debug 0 <boolean>)
   (socket/reuse-addr 1 <boolean>)
   (socket/keep-alive 2 <boolean>)
   (socket/dont-route 3 <boolean>)
   (socket/linger 4 <linger>)         ; linger is ~ (union #f <integer>)
   (socket/broadcast 5 <boolean>)
   (socket/oob-inline 6 <boolean>)
   (socket/send-buffer 7 <integer>)
   (socket/receive-buffer 8 <integer>)
   (socket/type 9 <integer> read-only)
   (socket/error 10 <integer> read-only)))

(define $portable-sockopt-levels
  '((level/socket 0)
    (level/ip 1)
    (level/tcp 2)
    (level/udp 3)))

(define (do-sockopt socket level option value type)
  (case type
    ((<boolean>)
     (if (boolean? value)
	 (values #t (setsockopt-int socket level option (if value 1 0)))
	 #f))
    ((<integer>)
     (if (fixnum? value)
	 (values #t (setsockopt-int socket level option value))
	 #f))
    ((<interval>)
     (if (instance? value <interval>)
	 (values #t (setsockopt-time socket level option value))
	 (if (instance? value <number>)
	     ;; SCSH compatibility; a number denotes # microseconds
	     (values #t (setsockopt-time
			 socket
			 level
			 option
			 (seconds->interval (* value 0.000001))))
	     #f)))
    ((<linger>)
     (if (or (eq? value #f) (integer? value))
	 (values #t (setsockopt-linger socket level option value))
	 #f))))

(define (sockopt-name-binding where level option)
  (let ((opt (assq option $portable-sockopt-options))
	(lvl (assq level $portable-sockopt-levels)))
    (if (and opt lvl)
        (values opt lvl
                (local-socket-option (cadr opt))
                (local-socket-level (cadr lvl)))
        (if opt
            (signal (make <unknown-socket-level>
                          where: where
                          name: level))
            (signal (make <unknown-socket-option>
                          where: where
                          name: option))))))

(define (set-socket-option socket level option value)
  (bind ((opt lvl optn lvln (sockopt-name-binding 'set-socket-option
                                                  level
                                                  option)))
    (if (null? (cdddr opt))
        (bind ((ok? rc (do-sockopt
                        socket 
                        lvln
                        optn
                        value 
                        (caddr opt))))
          (if ok?
              (if (eq? rc 0)
                  (values)
                  (error
                   (make <os-error>
                         error-number: (errno)
                         system-call: "setsockopt"
                         arguments: (vector socket lvln optn value))))
              (error
               (make <argument-type-error>
                     argument-error-function-name: 'set-socket-option
                     argument-error-arguments: (list value)
                     argument-error-bad-arg: 'value
                     argument-required-type: (caddr opt)))))
        (error
         (make <sockopt-read-only>
               name: option)))))

(define (get-socket-option socket level option)
  (bind ((opt lvl optn lvln (sockopt-name-binding 'get-socket-option
                                                  level 
                                                  option)))
    (let ((typ (caddr opt)))
      (getsockopt socket lvln optn typ))))
	   
(define-syscall-glue (getsockopt (fd <raw-int>)
				 (level <raw-int>)
				 (opt <raw-int>)
				 type)
  literals: ('<linger>
	     '<boolean> 
	     '<integer> 
	     '<interval>
	     (& <interval>))
{
  int len, rc = 0;

  if (EQ(type,LITERAL(1)))
    {
      int v;
      len = sizeof v;

      rc = getsockopt( fd, level, opt, (void *)&v, &len );
      REG0 = v ? TRUE_OBJ : FALSE_OBJ;
    }
  else if (EQ(type,LITERAL(2)))
    {
      int v;
      len = sizeof v;
      rc = getsockopt( fd, level, opt, (void *)&v, &len );
      REG0 = int2fx(v);
    }
  else if (EQ(type,LITERAL(3)))
    {
      struct timeval v;
      len = sizeof v;
      rc = getsockopt( fd, level, opt, (void *)&v, &len );
      REG0 = os_time( &v, TLREF(4) );
    }
  else if (EQ(type,LITERAL(0)))
    {
      struct linger v;
      len = sizeof v;

      rc = getsockopt( fd, level, opt, (void *)&v, &len );
      if (v.l_onoff)
	REG0 = int2fx(v.l_linger);
      else
	REG0 = FALSE_OBJ;
    }
  else
    {
      scheme_error( "bad sockopt type: ~s", 1, type );
    }
  if (rc < 0) {
    os_error( "getsockopt", 1, raw_fd );
  }
  RETURN1();
})
