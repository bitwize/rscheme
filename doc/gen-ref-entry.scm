(define-module-extend util.xml () (set-rscheme-sgml-mode!))

,(use util.xml
      tables)

(define (variant-arguments v)
  (let ((m (memq 'arguments: v)))
    (if m
        (cadr m)
        '())))

(define (variant-returns v)
  (let ((m (memq 'returns: v)))
    (if m
        (cadr m)
        '())))

(define (describe a)
  (cond
   ((memq '#optional a)
    (describe-optional a))
   ((every? string? (cdr a))
    (describe-class-list a "An instance of "))
   (else
    (error "can't describe ~s" a))))

(define (describe-optional a)
  (describe-class-list
   (cons (car a) (cddr a))
   "Optional; an instance of "))

(define (describe-class-list a term)
  (case (length (cdr a))
    ((1) `(para ,term (classname ,(cadr a))))
    ((2) `(para ,term (classname ,(cadr a))
                " or " (classname ,(caddr a))))
    ((3) `(para ,term (classname ,(cadr a)) ", "
                (classname ,(caddr a)) ", "
                " or " (classname ,(cadddr a))))
    ((4) `(para ,term (classname ,(list-ref a 0)) ", "
                (classname ,(list-ref a 1)) ", "
                (classname ,(list-ref a 2)) ", "
                " or " (classname ,(list-ref a 3))))
    (else (error "list too long"))))

;;;
;;;  Creates a subset list that contains only
;;;  those entries that are unique in the string car,
;;;  preserving the first such occurrence,
;;;  e.g.,
;;;
;;;    (first-only '(("a" 1 2) ("b" x y) ("a" 9 9)))  ==> (("a" 1 2) ("b" x y))
;;;

(define (first-only lst)
  (let ((tbl (make-string-table)))
    (let loop ((l lst)
               (r '()))
      (if (null? l)
          (reverse! r)
          (if (table-lookup tbl (caar l))
              (loop (cdr l) r)
              (begin
                (table-insert! tbl (caar l) #t)
                (loop (cdr l) (cons (car l) r))))))))

(define (string->id str)
  (string-join "." (string-split str #\/)))

(define (make-ref-entry #key refname
                             purpose
                             (type default: "function")
                             (arguments type: <list> default: '())
                             (returns type: <list> default: '())
                             (variants default: #f)
                             description)
  ;;
  (define (all-arguments)
    (first-only
     (apply append (map variant-arguments (all-variants)))))
  ;;
  (define (all-returns)
    (first-only
     (apply append (map variant-returns (all-variants)))))
  ;;
  (define (all-variants)
    (if variants
        variants
        (list (list arguments: arguments returns: returns))))
  ;;
  `(refentry
    ;;
    (refnamediv
     (refname (@ (id ,(string-append "f." (string->id refname)))) ,refname)
     (refpurpose ,purpose)
     (refclass (@ (role "Type")) ,type))
    ;;
    (refsynopsisdiv
     ,@(map (lambda (v)
              `(funcsynopsis
                (@ (role "Scheme"))
                "\n"
                (funcprototype
                 (funcdef (function ,refname))
                 (paramdef (@ (role "arguments"))
                           ,@(apply 
                              append
                              (map (lambda (a)
                                     (if (memq '#optional a)
                                         `(" " (parameter (optional ,(car a))))
                                         `(" " (parameter ,(car a)))))
                                   (variant-arguments v))))
                 (paramdef (@ (role "returns"))
                           ,@(apply
                              append
                              (map (lambda (a)
                                     `(" " (parameter ,(car a))))
                                   (variant-returns v)))))
                "\n"))
            (all-variants))
     ;;
     "\n"
     ,@(if (pair? (all-arguments))
           `((refsect2
              (title "Arguments")
              (variablelist
               ,@(map (lambda (a)
                        `(varlistentry
                          (term ,(car a))
                          (listitem 
                           ,(describe a))
                          "\n"))
                      (all-arguments)))))
           '())
     "\n"
     ;;
     ,@(if (pair? (all-returns))
           `((refsect2
              (title "Return Values")
              (variablelist
               ,@(map (lambda (a)
                        `(varlistentry
                          (term ,(car a))
                          (listitem ,(describe a))
                          "\n"))
                      (all-returns)))))
           '())
     "\n\n")
    ;; -- end of refsynopsisdiv --
    (refsect1
     (title "Description")
     ,@description)))

(define (t)
  (gen "open-server-socket"))

(define (gen name #optional file)
  (let ((sxml (apply make-ref-entry
                     (apply append
                            (map (lambda (e)
                                   (list (car e) (cdr e)))
                                 (table-lookup *documentation* name))))))
    (call-with-output-file
        (~ "ref/~a.sgml" (or file name))
      (lambda (port)
        (format port "<!-- NOTICE: This file was automatically generated\n")
        (format port "     by gen-ref-entry.scm -->\n\n")
        (write-sxml sxml port)))))


(define *documentation* (make-string-table))

(define (documentation . elems)
  (let ((a (keyword-list->assoc-list elems)))
    (table-insert! *documentation*
                   (cdr (assq 'refname: a))
                   a)))

(documentation
 refname: "socket-bind/inet-sockaddr"
 purpose: "Bind a socket to a socket address (IP and port)"
 arguments: '(("sockfd" "<fixnum>")
              ("sockaddr" "<inet-socket-addr>"))
 returns: '()
 description: '((para
                 "Binds the socket specified by "
                 (parameter "sockfd")
                 " to an internet socket address,\n"
                 "including the IP address and port number.\n"
                 
                 "This is useful when writing a server for a multi-homed\n"
                 "host or host with IP aliases.  For example, an HTTP "
                 "server present on only one or a subset of IP addresses\n"
                 " of a server.  This function allows the application to\n"
                 "bind to only one IP address, or bind\n"
                 "different socket file descriptors to different IP\n"
                 "addresses.\n")))
              
(documentation
 refname: "open-server-socket"
 purpose: "Create a TCP server (listening) socket"
 arguments: '(("port" "<fixnum>" "<inet-socket-addr>"))
 returns: '(("socket" "<server-socket>"))
 description:
 '((para
    "Creates a socket object which listens\n"
    "on the given port.\n")
   (para
    "If the " (parameter "port") " argument\n"
    "is an integer, then it represents a TCP port number\n"
    "and in which case the socket is bound to IN_ADDR_ANY\n"
    "and the socket will accept connections on any\n"
    "IP address.  The " (parameter "port") " argument\n"
    "may also be a " (classname "<inet-socket-addr>") ",\n"
    "in which case the socket will accept connections only\n"
    "on the particular IP address and port.\n")))


(documentation
 refname: "open-client-socket"
 purpose: "Create a TCP client socket"
 variants: '((arguments: (("addr" "<inet-socket-addr>")
                          ("name" #optional "<object>"))
                         returns: (("socket" "<initiator-socket>")))
             (arguments: (("host" "<string>")
                          ("port" "<fixnum>"))
                         returns: (("socket" "<initiator-socket>"))))
 description:
 '((para "Creates a socket object connected to a remote server.\n")))


(documentation
 refname: "close"
 type: "method"
 purpose: "Close a TCP socket"
 arguments: '(("socket" "<peer-socket>" "<server-socket>"))
 returns: '()
 description:
 '((para "Close a TCP socket.")))


(define (gen-all)
  (gen "open-client-socket")
  (gen "open-server-socket")
  (gen "close" "socket-close")
  (gen "socket-bind/inet-sockaddr" "socketbindinetsockaddr"))

