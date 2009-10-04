
(define-class <pop3-realm> (<auth-realm>)
  (user-table init-function: make-string-table))

(define-class <pop3-maildir-user> (<pop3-user>)
  maildir)

(define (make-pop3-realm)
  (make <pop3-realm>))

(define-method add-realm-user! ((self <pop3-realm>) 
                                user-name
                                #key (directory default: #f))
  (table-insert! (user-table self)
                 user-name
                 (make <pop3-maildir-user>
                       name: name
                       maildir: directory)))

(define-method chap-authenticate ((self <pop3-realm>)
                                  name 
                                  response
                                  hasher)
  (let ((e (table-lookup (user-table self) name)))
    (and e (string=? (hasher (password e)) response) e)))



;;;

;;;

(define (read-password-from-file file)
  (dm 910 "Reading password from ~s" file)
  (cadr (string-split 
         (file->string file)
         #\:)))

(memoize-file-accessor read-password-from-file)

(define-method password ((self <pop3-maildir-user>))
  (or (get-property self 'password #f)
      (read-password-from-file (string-append (maildir self) "/.passwd"))))


;;;

(define-class <maildir-entry> (<mailbox-entry>)
  size
  mtime
  uid
  directory
  subpath)

(define-method commit-deletion ((self <maildir-entry>))
  (let ((f (path self)))
    (dm 710 "deleting ~a" f)
    (unlink f)))

(define-method path ((self <maildir-entry>))
  (string-append (directory self) "/" (subpath self)))

(define-method mailbox-entry-content ((self <maildir-entry>))
  (file->string (path self)))

(define-method get-pop-mailbox ((self <pop3-maildir-user>))
  (load-mailbox (maildir self)))

(define (load-mailbox dir)
  (let ((lst '()))
    ;;
    (define (maildir subdir)
      (for-each
       (lambda (e)
         (if (not (char=? (string-ref e 0) #\.))
             (let* ((sb (stat (string-append dir "/" subdir "/" e)))
                    (ent (and sb
                              (make <maildir-entry>
                                    directory: dir
                                    subpath: (string-append subdir "/" e)
                                    uid: (apply format #f "~a:~d:~d:~d:~d"
                                                e
                                                (vector->list
                                                 (stat-id-vector sb)))
                                    size: (stat-size sb)
                                    mtime: (stat-mtime sb)))))
               (if ent
                   (set! lst (cons ent lst))))))
       (handler-case
        (scandir (string-append dir "/" subdir))
        ((<condition>) '()))))
    ;;
    (maildir "new")
    (maildir "cur")
    (let ((l (sort lst (lambda (a b)
                         (time<? (mtime a) (mtime b))))))
      (let loop ((i 1)
                 (p l))
        (if (null? p)
            (list->vector l)
            (begin
              (set-id! (car p) i)
              (loop (+ i 1) (cdr p))))))))
          
;;;




;;;

;;;

(define (g)
  (thread-resume (make-thread pop-server "popd")))

(define (gt)
  (thread-resume (make-thread 
                  (lambda () (pop-server port: 9110))
                  "popd")))

