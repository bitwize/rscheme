;;;
;;;  A CVS protocol client
;;;

(define-class <cvs-connection> (<object>)
  server
  root
  user
  socket
  to-cvs-server
  from-cvs-server)

(define-class <cvs-error> (<condition>)
  connection
  message)

(define-method display-object ((self <cvs-error>) port)
  (format port "*** CVS Error: ~a\n" (message self))
  (format port "    server: ~s  user: ~s  root: ~s\n"
          (server (connection self))
          (user (connection self))
          (root (connection self))))

(define-method cvs-close ((self <cvs-connection>))
  (close-output-port (to-cvs-server self))
  (close-input-port (from-cvs-server self))
  (fd-close (socket self)))

(define (read-cvspass host port root user)
  (let ((srch (format #f "~a@~a:~a~a" user host (or port "") root))
        (pat (reg-expr->proc '(seq ":pserver:"
                                   (save (+ (not #\space)))
                                   (+ #\space)
                                   (save (+ any))))))
    (let loop ((l (string-split (file->string "~/.cvspass") #\newline)))
      (if (null? l)
          (error "Cannot determine CVS password for ~a@~a" user host)
          (bind ((s e key value (pat (car l))))
            (if (and s (string=? key srch))
                value
                (loop (cdr l))))))))

(define cvsroot-pat
  (reg-expr->proc '(entire
                    (seq
                     (? ":pserver:")
                     (save (+ (not (or space #\@))))
                     #\@
                     (save (+ (not (or space #\:))))
                     #\:
                     (save (* digit))
                     (save (seq #\/ (* any)))))))
                    
(define (cvs-open #key 
                  (cvsroot default: #f)
                  (host default: #f)
                  (root default: #f)
                  (user default: (getenv "USER"))
                  (password default: #f)
                  (port default: #f))
  (if (not (or cvsroot host root password port))
      (set! cvsroot (getenv "CVSROOT")))
  (if cvsroot
      (bind ((s e user-part host-part port-part root-part (cvsroot-pat cvsroot)))
        (if s
            (begin
              (set! user user-part)
              (set! host host-part)
              (if (> (string-length port-part) 0)
                  (set! port (string->number port-part)))
              (set! root root-part))
            (error "Can't parse CVSROOT ~s" cvsroot))))
  ;;
  (if (not user)
      (error "Cannot determine CVS user"))
  (if (not password)
      (set! password (read-cvspass host port root user)))
  (let* ((fd (inet-client host (or port 2401)))
         (cnx (make <cvs-connection>
                    server: host
                    root: root
                    socket: fd
                    user: user
                    to-cvs-server: (open-queued-output fd)
                    from-cvs-server: (open-mbox-input-port fd))))
    (write-string 
     (to-cvs-server cnx)
     (format #f "BEGIN AUTH REQUEST\n~a\n~a\n~a\nEND AUTH REQUEST\n"
             root
             user
             password))
    (flush-output-port (to-cvs-server cnx))
    (let ((response (read-line (from-cvs-server cnx))))
      (if (string=? response "I LOVE YOU")
          (begin
            (cvs-request cnx "Root ~a" root)
            cnx)
          (begin
            (cvs-close cnx)
            (signal (make <cvs-error>
                          connection: cnx
                          message: response)))))))

(define (cvs-request (self <cvs-connection>) msg . args)
  (apply format (to-cvs-server self) msg args)
  (write-string (to-cvs-server self) "\n"))
    
(define (sync c)
  (flush-output-port (to-cvs-server c))
  (read-string (from-cvs-server c)))

(define (cvs-log (self <cvs-connection>) dirs #optional verbosity)
  (for-each (lambda (dir)
              (cvs-request self "Directory ~a\n~a/~a" 
                           dir
                           (root self)
                           dir))
            dirs)
  (cvs-request self "Directory ~a\n~a/~a" 
               (car dirs)
               (root self)
               (car dirs))
  (cvs-request self "log")
  (flush-output-port (to-cvs-server self))
  (let loop ((r '()))
    (let ((l (read-line (from-cvs-server self))))
      (if (string? l)
          (case (string-ref l 0)
            ((#\M)
             (loop (cons (substring l 2) r)))
            ((#\E)
             (case verbosity
               ((hash) 
                (format (current-error-port) "#")
                (flush-output-port (current-error-port)))
               ((verbose)
                (format #t ">> ~a\n" (substring l 2))))
             (loop r))
            ((#\o)
             (if (equal? l "ok")
                 (reverse r)
                 (error "log request failed: ~@#*30s" l)))
            (else
             (error "log request failed: ~@#*30s" l)))
          (error "log request failed: (unexpected eof)")))))

(define updated-msg (reg-expr->proc '(prefix (seq "Updated "))))

(define (is-updated-msg? str)
  (and (updated-msg str) #t))


(define (cvs-checkout (self <cvs-connection>) module #optional verbosity)
  ;;
  (define (huh? l)
    (signal (make <cvs-error>
                  connection: self
                  message: (format #f "unexpected `co' response line ~s" l))))
  ;;
  (cvs-request self "Argument ~a" module)
  (cvs-request self "co")
  (flush-output-port (to-cvs-server self))
  (let ((p (from-cvs-server self))
        (errline (case verbosity
                   ((hash)
                    (lambda (l)
                      (format (current-error-port) "#")
                      (flush-output-port (current-error-port))))
                   ((verbose)
                    (lambda (l)
                      (format #t "** ~a\n" (substring l 2))))
                   (else
                    (lambda (l)
                      (values)))))
        (msgline (if (eq? verbosity 'verbose)
                     (lambda (l)
                       (format #t "   ~a\n" (substring l 2)))
                     (lambda (l)
                       (values)))))
    ;;
    (let loop ((r '()))
      (let ((l (read-line p)))
        ;(format #t "_ ~s\n" l)
        (if (string? l)
            (case (string-ref l 0)
              ((#\E)
               (errline l)
               (loop r))
              ((#\M)
               (msgline l)
               (loop r))
              ((#\o)
               (if (string=? l "ok")
                   (reverse! r)
                   (huh? l)))
              ((#\U)
               (if (is-updated-msg? l)
                   (let* ((file-name (read-line p))
                          (entries-line (read-line p))
                          (mode-line (read-line p))
                          (len-line (read-line p))
                          (len (string->number len-line))
                          (content (read-string p len)))
                     (format #t "~a (~d bytes)\n" file-name len)
                     (loop (cons (list file-name
                                       entries-line
                                       mode-line
                                       content)
                                 r)))
                   (huh? l)))))))))
