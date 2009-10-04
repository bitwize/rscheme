
(define (read-n-lines port n)
  (if (= n 0)
      '()
      (cons (read-line port) (read-n-lines port (- n 1)))))

(define (dispatch-request (self <pserver-connection>) word args)
  (let ((entry (table-lookup *request-table* word)))
    (if entry
        (let ((proc (car entry)))
          (dm 700 "dispatch ~s" entry)
          (if (null? (cadr entry))
              (if (string=? args "")
                  (proc self)
                  (em 791 "arg supplied when unexpected"))
              (apply proc 
                     self
                     args
                     (read-n-lines (input-port (socket self))
                                   (- (length (cadr entry)) 1)))))
        (em 792 "unknown word ~s" word))))

(define (clear-inventory (self <pserver-connection>))
  (set-inventory! self #f))

(define (get-inventory (self <pserver-connection>))
  (or (inventory self)
      (let ((t (make-string-table)))
        (set-inventory! self t)
        t)))

(define-method relative-file ((self <pserver-connection>) file)
  (if (get-property self 'directory #f)
      (pathname->os-path
       ;; relative to the repository location
       (append-path (cadr (get-property self 'directory))
                    (string->file file)))
      file))

(define (get-inventory-file (self <pserver-connection>) file)
  (or (table-lookup (get-inventory self) (relative-file self file))
      (begin
        (print (get-inventory self))
        (em 804 "No file ~s (~s) in current inventory" 
            file (relative-file self file)))))

(define (read-file-contents (self <pserver-connection>) len)
  (let ((n (string->number len)))
    (if n
        (read-string (input-port (socket self)) n)
        (em 803 "Unknown file transmission length spec ~s" len))))

;;;

(define *request-table* (make-string-table))

(define (send-ok (self <pserver-connection>))
  (let ((r "ok\n"))
    (dm 382 "<< ~#@*50s" r)
    (write-string (output-port (socket self)) r)
    (values)))
  
(define (send-response (self <pserver-connection>) msg args)
  (let ((r (apply format #f msg args)))
    (dm 381 "<< ~#@*50s" r)
    (write-string (output-port (socket self)) r)
    (values)))

(define-macro (define-pserver-fn (name . args) . body)
  `(define (,name (self <pserver-connection>) ,@args) 
     (let-syntax ((response (syntax-form (msg . args)
                              (send-response self msg (list . args))))
                  (ok (syntax-form ()
                        (send-ok self)))
                  (consume-args (syntax-form ()
                                  (get-and-clear-args self))))
       ,@body)))

(define-macro (define-pserver-request (name . args) . body)
  (let ((s (symbol-append "pserver:" name)))
    ;;
    `(begin
       (define-pserver-fn (,s ,@args) ,@body)
       #|
       (define (,s (self <pserver-connection>) ,@args) 
         (let-syntax ((response (syntax-form (msg . args)
                                  (send-response self msg (list . args))))
                      (ok (syntax-form ()
                            (send-ok self)))
                      (consume-args (syntax-form ()
                                      (get-and-clear-args self))))
           ,@body))
       |#
       (table-insert! *request-table* ,name (list ,s ',args)))))

(define (get-and-clear-args (self <pserver-connection>))
  (let ((a (reverse (arg-accum self))))
    (set-arg-accum! self '())
    a))

;;;

(define-pserver-request ("Root" args)
  (dm 701 "root ~s" args)
  (set-property! self 'root (bind-to-root self args))
  (set-property! self 'user (string->user (cadr (auth-token self)))))
                           

(define-pserver-request ("Valid-responses" args)
  (let ((x (string-split args #\space)))
    (dm 702 "valid responses ~s" x)))

(define-pserver-request ("valid-requests")
  (let ((p (output-port (socket self))))
    (let ((x (key-sequence *request-table*)))
      (dm 703 "valid requests ~s" x)
      (format p "Valid-requests ~a\n" (string-join #\space x))
      (format p "ok\n"))))
  

(define-pserver-request ("Directory" dir repos)
  (dm 704 "Directory ~s" dir)
  (dm 705 "Repository ~s" repos)
  (set-property! self 'directory (list dir (string->dir repos))))

(define-pserver-request ("Max-dotdot")
  (values))

(define-pserver-request ("Static-directory")
  (values))

(define-pserver-request ("Sticky")
  (values))

(define-pserver-request ("Checkin-prog")
  (values))

(define-pserver-request ("Update-prog")
  (values))

(define-class <inventory-item> (<object>)
  (properties init-value: '#())
  name
  version
  status)

(define-method write-object ((self <inventory-item>) port)
  (format port "#[<inventory-item> ~a ~a]"
          (name self)
          (version self)))

(define-pserver-request ("Entry" entry)
  (let ((e (string-split entry #\/)))
    (dm 707 "Entry ~s" e)
    (if (and (>= (length e) 3)
             (> (string-length (cadr e)) 0))
        (table-insert! (get-inventory self)
                       (relative-file self (cadr e))
                       (make <inventory-item>
                             name: (cadr e)
                             version: (caddr e)
                             status: #f)))
    (values)))

(define-pserver-request ("Kopt")
  (values))

(define-pserver-request ("Checkin-time")
  (values))

(define-pserver-request ("Modified" file mode len)
  (dm 713 "Modified ~s ~s" file mode)
  (let ((f (get-inventory-file self file)))
    (set-property! f 'content (read-file-contents self len))
    (set-property! f 'mode mode)
    (set-status! f 'modified)
    (values)))

(define-pserver-request ("Is-modified" file)
  (dm 712 "Is modified ~s" file)
  (let ((f (get-inventory-file self file)))
    (set-status! f 'modified)
    (set-property! f 'content #f)
    (values)))

(define-pserver-request ("Unchanged" file)
  (dm 711 "Unchanged ~s" file)
  (let ((f (get-inventory-file self file)))
    (set-status! f 'unchanged)
    (values)))

(define-pserver-request ("UseUnchanged")
  (values))

(define-pserver-request ("Notify")
  (values))

(define-pserver-request ("Questionable")
  (values))

(define-pserver-request ("Case")
  (values))

(define-pserver-request ("Argument" arg)
  (set-arg-accum! self (cons arg (arg-accum self)))
  (values))

(define-pserver-request ("Argumentx" arg)
  (if (null? (arg-accum self))
      (em 802 "Argumentx with no current argument"))
  ;;
  (set-car! (arg-accum self)
            (string-append (car (arg-accum self)) "\n" arg))
  (values))

(define-pserver-request ("Global_option" arg)
  (set-global-options! self (cons arg (global-options self)))
  (values))

;Gzip-stream
;Kerberos-encrypt
;Gssapi-encrypt
;Gssapi-authenticate

(define-pserver-request ("Set")
  (values))

#|
(define *module-aliases*
  '(("foo" ("a/x" "a/y"))
    ("bar" ("a/y" "b/z"))))
|#

(define-pserver-request ("expand-modules")
  (let ((a (consume-args))
        (root (get-property self 'root)))
    ;;
    (for-each
     (lambda (d)
       (let ((r (fs-append-path (path-to-top root) d)))
         (dm "module expansion: ~a (~a)" d r)
         (response "Module-expansion ~a\n" d)))
     (enumerate-subdirs (filesystem root) 
                        (map (lambda (d)
                               (fs-append-path (path-to-top root)
                                               (string->fs-path d)))
                             a)))
    ;;
    (ok)))

(define-pserver-request ("diff")
  (values))

(define-pserver-request ("tag")
  (values))

(define-pserver-request ("status")
  (values))

(define-pserver-request ("admin")
  (values))

(define-pserver-request ("history")
  (values))

(define-pserver-request ("watchers")
  (values))

(define-pserver-request ("editors")
  (values))

(define-pserver-request ("annotate")
  (values))

(define-pserver-request ("log")
  (values))




(define (directories-to-clear files)
  (let ((q (make-dequeue))
        (tbl (make-string-table)))
    ;;
    (define (clear (p <directory-name>))
      (let ((k (pathname->os-path p)))
        (if (not (table-lookup tbl k))
            (begin
              (dequeue-push-back! q k)
              (table-insert! tbl k #t)))))
    ;;
    (for-each
     (lambda (f)
       (for-each clear
                 (dir-parents
                  (file-directory (string->file (car f))))))
     files)
    ;;
    (vector->list (dequeue-state q))))

(define (bit-permissions p)
  (string-append
   (if (eq? (bitwise-and p #b100) #b000) "" "r")
   (if (eq? (bitwise-and p #b010) #b000) "" "w")
   (if (eq? (bitwise-and p #b001) #b000) "" "x")))

(define-pserver-fn (co-update item)
  (let* ((node (get-property item 'node))
         (data (content->string (contents node))))
    ;;
    (response "Mod-time ~a\n"
              (time->string (modification-time node)
                            "%d %b %Y %H:%M:%S -0000" 
                            #f))
    (response "Update-existing ~a\n" 
              (cvs-pathname self (get-property item 'path)))
    (response "/~a/~a///\n" (name item) (version item))
    (let (((p <fixnum>) (permissions node)))
      (response "u=~a,g=~a,o=~a\n"
                (bit-permissions (bitwise-and (logical-shift-right p 6) #b111))
                (bit-permissions (bitwise-and (logical-shift-right p 3) #b111))
                (bit-permissions (bitwise-and p #b111))))
    (response "~d\n~a"
              (string-length data)
              data)))

(define-pserver-fn (co-create item)
  (let* ((node (get-property item 'node))
         (data (content->string (contents node))))
    ;;
    (response "Mod-time ~a\n"
              (time->string (modification-time node)
                            "%d %b %Y %H:%M:%S -0000" 
                            #f))
    (response "Created ~a\n" (cvs-pathname self (get-property item 'path)))
    (response "/~a/~a///\n" (name item) (version item))
    (let (((p <fixnum>) (permissions node)))
      (response "u=~a,g=~a,o=~a\n"
                (bit-permissions (bitwise-and (logical-shift-right p 6) #b111))
                (bit-permissions (bitwise-and (logical-shift-right p 3) #b111))
                (bit-permissions (bitwise-and p #b111))))
    (response "~d\n~a"
              (string-length data)
              data)))
  

(define-pserver-fn (update-engine args root checkout-mode?)
  (for-each
   (lambda ((p <inventory-item>))
     (let ((path (get-property p 'path)))
       (case (status p)
         ((clear-static)
          (if checkout-mode?
              (begin
                (response "E DIR ~a\n" path)
                (response "Clear-static-directory ~a\n" 
                          (cvs-pathname self (get-property p 'path))))))
         ;;
         ;; a file is in the target tree but not in the client tree
         ;;
         ((create)
          (response "E CRE ~a\n" path)
          (if (can-modify-client? self)
              (co-create self p)))
         ;;
         ;; the client has modified the file relative to the current
         ;; version in the target tree
         ;;
         ((modified)
          (response "E MOD ~a\n" path))
         ;;
         ;; the client has modified the file based on something other
         ;; than the current version in the target tree
         ;;
         ((merge)
          (response "E CNF ~a\n" path))
         ;;
         ;; the client has not modified the file, and it is still the
         ;; same version in the target tree
         ;;
         ((noop)
          (response "E NOP ~a\n" path))
         ;;
         ;; the client has not modified the file, but a different version
         ;; is present in the target tree
         ;;
         ((update)
          (response "E UPD ~a\n" path))
         ;;
         (else
          (em 872 "bad status: ~s" (status p))))))
   ;;
   (generate-update-plan (get-inventory self)
                         (get-property self 'root)
                         args)))

(define-pserver-request ("update")
  (bind ((targets d? A? P? (cvs-getopt (consume-args)
                                       '(("-d" 0)
                                         ("-A" 0)
                                         ("-P" 0))))
         (root (get-property self 'root)))
    (dm 708 "update ~s" targets)
    (update-engine self targets root #f)
    (ok)))

(define-pserver-request ("co")
  (bind ((root (get-property self 'root))
         (targets N? (cvs-getopt (consume-args) '(("-N" 0)))))
    (dm 706 "check out ~s" targets)
    (update-engine self targets root #t)
    (ok)))

(define-pserver-request ("ci")
  (bind ((args (consume-args))
         (root (get-property self 'root))
         (targets comment (cvs-getopt args '(("-m" 1)))))
    ;;
    (dm 720 "Check in ~s" targets)
    (dm 721 "Comment ~s" comment)
    ;;
    (table-for-each
     (get-inventory self)
     (lambda (h (path <string>) (item <inventory-item>))
       (let* ((fs (filesystem root))
              (fspr (steps->fs-path (cdddr (steps (string->fs-path path)))))
              (fsp (fs-append-path $root-path fspr))
              (user (get-property self 'user)))
         (dm 722 "FS ~a -- file ~a" fs fsp)
         (node-lock fs fsp user)
         (let ((v (file-delta fs fsp user
                              (string->content (get-property item 'content))
                              '()                ; reasons
                              comment
                              #f                 ; mtime
                              '()                ; other-fss
                              #f)))              ; diverge?
           (set-property! item 'node v)
           (set-property! item 'path fspr)
           (set-version! item (to-string (version-tag v)))
           (co-update self item)))))
    ;;
    (ok)))

(define (cvs-getopt args opts)
  (let ((result (make-vector (length opts) #f)))
    ;;
    (define (done a)
      (list->values (cons a (reverse! (vector->list result)))))
    ;;
    (let loop ((a args))
      (if (null? a)
          (done '())
          (if (and (> (string-length (car a)) 0)
                   (char=? (string-ref (car a) 0) #\-))
              (let ((o (assoc (car a) opts)))
                (if (not o)
                    (em 403 "Unknown option: ~s" (car a)))
                (let ((k (length (cdr (memq o opts)))))
                  (if (eq? (cadr o) 0)
                      (begin
                        (vector-set! result k #t)
                        (loop (cdr a)))
                      (begin
                        (vector-set! result k (cadr a))
                        (loop (cddr a))))))
              (done a))))))
                         

#|
                           
    (for-each (lambda (d)
              dirs)
    ;;
    (for-each 
     (lambda (d)
       (response "E processing: ~s\n" d)
       (response "Mod-time ~a\n" (time->string (caddr f)
                                               
       (response "Created ~a/\n/rsfam/foo/~a\n" 
                 (pathname->os-path (file-directory (string->file (car f))))
                 (car f))
       (response "/~a/1.1///\n" (car f))
       (response "u=rw,g=rw,o=rw\n")
       (response "~d\n~a" (string-length (cadr f)) (cadr f)))
     *files*)
    (ok)
    (values)))
|#

(define-pserver-request ("export")
  (values))

(define-pserver-request ("rannotate")
  (values))

(define-pserver-request ("rdiff")
  (values))

(define-pserver-request ("rlog")
  (values))

(define-pserver-request ("rtag")
  (values))

(define-pserver-request ("init")
  (values))

(define-pserver-request ("import")
  (values))

(define-pserver-request ("add")
  (values))

(define-pserver-request ("remove")
  (values))

(define-pserver-request ("watch-on")
  (values))

(define-pserver-request ("watch-off")
  (values))

(define-pserver-request ("watch-add")
  (values))

(define-pserver-request ("watch-remove")
  (values))

(define-pserver-request ("release")
  (values))

(define-pserver-request ("noop")
  (values))

;gzip-file-contents
;wrapper-sendme-rcsOptions

(define-pserver-request ("version")
  (values))

