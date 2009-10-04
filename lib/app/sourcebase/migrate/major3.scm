,(use tables regex)


(define (base64-gzip-encode x)
  (with-module
      rs.net.pem
    (with-module
        rs.sys.compression
      (let ((o (open-output-string))
            (s (pem-encode (compress x))))
        (let loop ((i 0))
          (if (< i (string-length s))
              (let ((j (min (string-length s) (+ i 72))))
                (write-string o (substring s i j))
                (write-string o "\n")
                (loop j))
              (close-output-port o)))))))


(define-thread-var *action-time* #f)
(define-thread-var *action-user* #f)

(define $no-time (string->time "1995-08-14 00:00:00 CDT"))

(define-syntax (no-audit . body)
  (thread-let ((*action-time* $no-time)
               (*action-user* "?"))
    (begin . body)))
  
(define-syntax (with-user u . body)
  (thread-let ((*action-user* u))
    (begin . body)))

(define-syntax (with-time u . body)
  (thread-let ((*action-time* u))
    (begin . body)))

(define-syntax (with-audit audit . body)
  (thread-let ((*action-time* (timestamp audit))
               (*action-user* (if (string? (user audit))
                                  (user audit)
                                  (if (user audit)
                                      (name (user audit))
                                      "?"))))
    (begin . body)))

(define *global-count* 0)
(define *short-port* (current-output-port))
(define *short-data-backing* (make-string-table))

(define (save-short-data-backing file)
  (let ((ps (create-persistent-store file)))
    (commit ps *short-data-backing*)
    (close-persistent-store ps)))

(define (make-emitter scope)
  (let ((q 0))
    (lambda (action . args)
      (if (eq? q 0)
          (begin
            (set! *global-count* (+ *global-count* 1))))
      (let* ((key (~ "~a~03d.~02d"
                     (integer->char (+ (char->integer #\A)
                                       (quotient *global-count* 1000)))
                     (modulo *global-count* 1000)
                     q)) 
             (uid (to-string (transient->persistent scope)))
             (short-entry (list (list key *global-count* q)
                                uid
                                *action-time*
                                *action-user*
                                action
                                args)))
        ;;
        (if *short-data-backing*
            (table-insert! *short-data-backing* key short-entry))
        ;;
        (write-string *short-port*
                      (string-append
                       (if (eq? *action-time* $no-time)
                           "? "
                           "  ")
                       (string-join #\| (saved-short-entry->all-fields
                                         short-entry))
                       "\n"))
        (set! q (+ q 1))))))


(load "~/p/axis/src/replay-format.scm")

(define emit #f)

(define-syntax (with-scope-and-audit item . body)
  (with-scope 
   item
   (let ((l (audit-log item)))
     (if (pair? l)
         (with-audit (last l) (begin . body))
         (no-audit (begin . body))))))

(define-syntax (with-scope scope . body)
  (set! emit (make-emitter scope))
  (begin . body))

(define-method blowout ((self <snapshot>))
  ;;
  (define (force-list x)
    (if (pair? x)
        x
        (list x)))
  
  (with-scope
   self
   (let ((created (get-property self 'created))
         (committed (get-property self 'committed #f))
         (state (get-property self 'state))
         (basis (get-property self 'basis #f)))
     (with-audit 
      created
      (emit 'mksnap
            (name (versioned-object self))
            (name self)
            (if basis (name basis) "?")))
     ;;
     (let ((xset (get-property self 'extend '())))
       (if (pair? xset)
           (for-each
            (lambda (cr)
              (let* ((cr-list (force-list cr))
                     (tlist (map (lambda (cr)
                                   (let ((qual (select (lambda (x)
                                                         (and (instance? x <fs-change>)
                                                              (eq? (file-system x) 
                                                                   (versioned-object self))))
                                                       (history cr))))
                                     (if (null? qual)
                                         (begin
                                           (format #t "Snapshot fails to qualify: ~s in ~s\n"
                                                   (id cr)
                                                   self)
                                           $no-time)
                                         (timestamp (close-audit-entry (car qual))))))
                                 cr-list)))
                (with-audit 
                 created
                 (with-time
                  (car (sort tlist time>?))
                  (emit 'snapx
                        (name (versioned-object self))
                        (name self)
                        (string-join #\space (map (lambda (cr)
                                                    (to-string (id cr)))
                                                  cr-list)))))))
            xset)))
     ;;
     (if committed
         (with-audit
          committed
          (emit 'closnap
                (name (versioned-object self))
                (name self)))))))


(define-method blowout ((self <file-system>))
  (with-scope
   self
   ;;
   (no-audit
    (with-user
     (name (owner self))
     (emit 'mkfs 
           (name self)
           (name (group self))
           (~ "#~d" (id (root-directory self))))))
   ;;
   (for-each blowout
             (value-sequence (snapshot-table self)))))

#|

(string->snapshot (string->filesystem "rs-0.6") "1.4")
(node-script* %)

(blowout (string->filesystem "rs-0.6"))

|#

(define-method blowout ((self <user>))
  (with-scope-and-audit
   self
   (emit 'mkuser
         (name self)
         (full-name self)
         (email-addr self))))

(define-method blowout ((self <group>))
  (if (pair? (parent-groups self))
      (with-scope-and-audit
       self
       (emit 'mkdom
             (name self)
             (name (car (parent-groups self)))
             (name (owner self))
             (~ "?Group ~s from SourceBase" (name self))))))

;;;

(define-method blowout/h ((self <comment-request>))
  (if (not (eq? self (last (history (base-request self)))))
      (emit 'note
            (to-string (id (base-request self)))
            (trim-whitespace (comment self)))))

(define-method blowout/h ((self <integration-request>))
  (values))

(define-method blowout/a ((self <work-request>))
  (values))

(define *new-state-map* (make-object-table))    ; <work-item> => <symbol>

(define (build-new-state-map)
  (let ((tbl (make-object-table)))
    ;;
    (define (evolvelist s lst)
      (let loop ((s s)
                 (l lst))
        (if (null? l)
            s
            (if (instance? (car l) <work-request>)
                (begin
                  (table-insert! tbl (car l) s)
                  (loop (state (car l)) (cdr l)))
                (loop s (cdr l))))))
    ;;
    (define (scan-cr (self <change-request>))
      (evolvelist
       (evolvelist (state self) (active-items self))
       (history self)))
    ;;
    (for-each scan-cr (value-sequence (change-request-table *application*)))
    ;;
    tbl))

(define-method blowout/h ((self <work-request>))
    (let ((ns (table-lookup *new-state-map* self)))
    ;;
    (define (any-fs-changes?)
      (or (any? (lambda (wi)
                  (instance? wi <fs-change>))
                (active-items (base-request self)))
          (any? (lambda (wi)
                  (instance? wi <fs-change>))
                (history (base-request self)))))
    ;;
    (if (not (eq? (state self) ns))       ; null transition
        (let ((subj (to-string (id (base-request self))))
              (s (case ns
                        ((check-off)
                         (if (any-fs-changes?)
                             ;; a transition to the check-off (aka COMPLETE)
                             ;; state is automatic when the last FSC is
                             ;; completed
                             #f
                             ;; but if there are none, then it's an explicit
                             ;; transition
                             'complete))
                        ;;
                        ((fixing)
                         (if (any-fs-changes?)
                             ;; a transition to the fixing (aka FIX) state
                             ;; is implicit when a change is done against
                             ;; the CR
                             #f
                             ;; but if there are no FS changes, go to
                             ;; the research state instead
                             'research))
                        ;;
                        ((cancelled)    'canceled)
                        ((integration)  'integrate)
                        ((closed returned research) ns))))
          (if s
              (begin
                ;; some transitions need an intermediate state in Axis
                (if (and (memq (state self) '(research fixing open))
                         (eq? s 'closed))
                    (emit 'chstate subj "complete"))
                (emit 'chstate subj (to-string s))))))))

(define-method blowout/h ((self <code-review>))
  (let* ((subj (to-string (id (base-request self))))
         (open-msg (~ "Please review changes in '~a'" (name (group self))))
         (close-msg (~ "Changes to '~a' reviewed" (name (group self)))))
    ;;
    (if (and (close-audit-entry self)
             (eq? (timestamp (close-audit-entry self))
                  (timestamp (activate-audit-entry self))))
        (with-audit
         (activate-audit-entry self)
         (emit 'note subj close-msg))
        (begin
          (with-audit
           (activate-audit-entry self)
           (emit 'rfc subj (name (owner self)) open-msg))
          ;;
          (if (close-audit-entry self)
              (with-audit
               (close-audit-entry self)
               (emit 'rfca subj (name (owner self)) close-msg)))))))

(define-method blowout/h ((self <property-add>))
  (emit 'addprop
        (to-string (id (base-request self)))
        (name (the-property self))
        (name (new-value self))))

(define-method blowout/h ((self <property-change>))
  (emit 'chprop
        (to-string (id (base-request self)))
        (name (the-property self))
        (name (new-value self))))

(define-method blowout/h ((self <title-change>))
  (emit 'chtitle
        (to-string (id (base-request self)))
        (new-title self)))

(define-method blowout/a ((self <fs-change>))
  (values))

(define-method blowout/h ((self <fs-change>))
  (with-audit
   (close-audit-entry self)
   (emit 'done
         (to-string (id (base-request self)))
         (name (file-system self))
         (name (owner self)))))


;;;

(define-method blowout/a ((self <work-item>))
  ;; default to using blowout/h
  (blowout/h self))

(define (cr-plugins (self <change-request>))
  (let ((p '()))
    (define (piv k v)
      (set! p (cons (~ "~a=~a" k v) p)))

    (for-each
     (lambda (prop)
       (let ((k (car prop))
             (v (cdr prop)))
         (if (symbol? k)
             (case k
               ((requestor) (values))     ; handled elsewhere
               ((reference) (piv 'reference v))
               (else (piv k (name v)))))))
     (properties self))
    ;;
    (string-join #\; (reverse p))))

(define-method blowout ((self <change-request>))
  (with-scope
   self
   (with-time
    (timestamp (activate-audit-entry (last (history self))))
    (with-user 
     (name (get-property self 'requestor))
     (let ((l (last (history self))))
       (emit 'open
             (to-string (id self))
             (name (group self))
             (title self)
             (cr-plugins self)
             (if (instance? l <comment-request>)
                 (comment l)
                 ""))))))
  (for-each (lambda (w)
              (with-scope
               w
               (with-audit 
                (activate-audit-entry w)
                (blowout/h w))))
            (history self))
  (for-each (lambda (w)
              (with-scope
               w
               (with-audit
                (activate-audit-entry w)
                (blowout/a w))))
            (active-items self)))


(define (reason-id-list lst)
  (string-join #\space 
               (map (lambda ((wi <fs-change>))
                      (to-string (id (base-request wi))))
                    (select (lambda (x)
                              (instance? x <fs-change>))
                            lst))))

(define (with-node-version* self thunk)
  (with-scope
   self
   (if (pair? (change-items self))
       (let ((ci (car (change-items self))))
         (if (instance? ci <fs-change>)
             (with-user
              (name (owner ci))
              (with-time
               (modification-time self)
               (thunk)))
             (with-audit
              ci
              (thunk))))
       (with-user
        "?"
        (with-time
         (modification-time self)
         (thunk))))))

(define-syntax (with-node-version self . body)
  (with-node-version* self (lambda () (begin . body))))

(define-method blowout ((self <directory-version>))
  (with-node-version
   self
   (let ((fs (case (length (change-items self))
               ((0) #f)
               ((1) (name (file-system (car (change-items self)))))
               ((2) (error "an excess of riches in ~s" self)))))
     (if (previous-version self)
         (blowout-dir/subseq self (previous-version self) (versioned-object self) fs)
         (blowout-dir/first self (versioned-object self) fs)))))

(define (blowout-dir/first (self <directory-version>) node fs)
  (let ((fli (first-linked (versioned-object self))))
    (if (not (instance? fli <file-system>))
        (emit 'mkdir
              (~ "#~d" (id (versioned-object self)))
              (or fs
                  (if fli
                      (~ "?#~d:" (id (versioned-object (cdr fli))))
                      "?"))
              (if fli
                  (~ "#~d:~a" (id (versioned-object (cdr fli))) (car fli))
                  "?")
              (name (group (versioned-object self)))
              (reason-id-list (change-items self))
              (number->string (permissions self) 8)
              (or (comment self) "")))))
  
(define (dir-changes old new)
  (let ((oname (make-object-table))
        (nname (make-object-table)))
    ;;
    (for-each (lambda (entry)
                (table-insert! oname (cdr entry) (car entry)))
              old)
    (for-each (lambda (entry)
                (table-insert! nname (cdr entry) (car entry)))
              new)
    ;;
    (bind ((unchanged removed added (outer-join-eq (map cdr old)
                                                   (map cdr new)))
           (r-add (map (lambda (n)
                         (cons (table-lookup nname n) n))
                       added))
           (r-remove (map (lambda (n)
                            (cons (table-lookup oname n) n))
                          removed))
           (r-rename (select
                      (lambda (p)
                        (not (string=? (car p) (cadr p))))
                      (map (lambda (n)
                             (list (table-lookup oname n)
                                   (table-lookup nname n)
                                   n))
                           unchanged))))
      ;;
      #|
      (if (null? (append r-add r-remove r-rename))
          (begin
            (format #t "-----------------------------\n")
            (format #t "old: ~s\n" old)
            (format #t "new: ~s\n" new)
            (format #t "unchanged: ~s\n" unchanged)))
      |#
      (values r-add r-remove r-rename))))

(define (outer-join-eq a b)
  (let loop ((a a)
             (both '())
             (aonly '())
             (bonly b))
    (if (null? a)
        (values both aonly bonly)
        (let ((bx (memq (car a) bonly)))
          (if bx
              (loop (cdr a)
                    (cons (car a) both)
                    aonly
                    (delq (car bx) bonly))
              (loop (cdr a)
                    both
                    (cons (car a) aonly)
                    bonly))))))

(define (first-version (self <node>))
  (value (find-leaf (versions self) '(1 1))))

(define *deferred-links* (make-object-table))

(define (blowout-dir/subseq (self <directory-version>) pred node fs)
  ;; There is a previous version, so there are just
  ;; some changes being made...
  (bind ((ln unlink rename (dir-changes (contents pred) 
                                        (contents self)))
         (pre (~ "#~d" (id node))))
    ;;
    (for-each
     (lambda (link1)
       (bind ((targ (cdr link1))                ; the thing being linked into this directory
              (name (car link1)))               ; the name it will have in this directory
         ;;
         ;;  In general, every node that is created will show up in one directory's
         ;;  "linked in" list.  Hence, we will only emit these if there are more
         ;;  than one globally (then the user has to sort out which is associated
         ;;  with the creation operation)
         ;;
         (table-insert!
          *deferred-links*
          targ
          (cons
           (let ((saved-time *action-time*)
                 (saved-user *action-user*))
             (lambda ()
               (thread-let ((*action-time* saved-time)
                            (*action-user* saved-user))
                 (emit 'ln
                       ;;
                       ;; indicate the destination (new) name for the given object
                       pre (or fs (~ "?#~d:" (id node))) (~ "#~d:~a" (id node) name)
                       ;;
                       ;; indicate the source (old) name for the given object
                       (~ "#~d" (id targ)) (~ "?#~d:" (id targ)) (~ "#~d::" (id targ))
                       ;;
                       (reason-id-list (change-items self))
                       (or (comment self) "")))))
           (or (table-lookup *deferred-links* targ) '())))))
     ln)
    ;;
    (for-each
     (lambda (unlink1)
       (bind ((targ (cdr unlink1))
              (name (car unlink1)))
         (emit (if (instance? targ <directory>)
                  'rmdir
                  'rm)
               ;; indicate the old name of the object being deleted
              pre (or fs (~ "?#~d:" (id targ))) (~ "#~d::" (id targ))
              (reason-id-list (change-items self))
              (or (comment self) ""))))
     unlink)
    ;;
    (for-each
     (lambda (rename1)
       (let ((targ (caddr rename1))
             (old-name (car rename1))
             (new-name (cadr rename1)))
         (emit 'mv
               ;; indicate the source (old) name for the object being 
               ;; renamed within this directory
               pre (or fs (~ "?#~d:" (id targ))) (~ "#~d:~a" (id node) old-name)
               ;; indicate the new name
               (~ "#~d:~a" (id node) new-name)
               ;;
               (reason-id-list (change-items self))
               (or (comment self) ""))))
     rename)
    ;;
    (values)))

(define-method blowout ((self <file-version>))
  (let* ((pred (previous-version self))
         (node (versioned-object self)))
    ;;
    (with-node-version
     self
     ;;
     (bind ((ver (to-string (version-tag self)))
            (data (content->string (contents self)))
            (dataz (list (string-length data)
                         (base64-gzip-encode data))))
       ;;
       (if pred
           (emit 'edit
                 (~ "#~d" (id (versioned-object self)))
                 (~ "?#~d:" (id (versioned-object self)))
                 (~ "#~d::" (id (versioned-object self)))
                 ver
                 (reason-id-list (change-items self))
                 (comment self)
                 (number->string (permissions self) 8)
                 dataz)
           (let ((fli (first-linked (versioned-object self))))
             (emit 'creat
                   (~ "#~d" (id (versioned-object self)))
                   (if fli
                       (if (instance? fli <file-system>)
                           (name fli)
                           (~ "?#~d:" (id (versioned-object (cdr fli)))))
                       "?")
                   (if fli 
                       (if (instance? fli <file-system>)
                           "?"
                           (~ "#~d:~a" 
                              (id (versioned-object (cdr fli)))
                              (car fli)))
                       "?")
                   (name (group (versioned-object self)))
                   (reason-id-list (change-items self))
                   (comment self)
                   (number->string (permissions self) 8)
                   dataz)))))))
               

(define-method blowout ((self <file>))
  (for-each-version
   (versions self)
   (lambda (path vleaf)
     (blowout (value vleaf)))))
              
(define-method blowout ((self <directory>))
  (for-each-version
   (versions self)
   (lambda (path vleaf)
     (blowout (value vleaf)))))
  
(define (flush-deferred-links)
  (format #t "~d entries in deferred link table\n" (table-size *deferred-links*))
  (let ((n 0))
    (table-for-each
     *deferred-links*
     (lambda (h k v)
       (if (pair? (cdr v))
           (begin
             (set! n (+ n 1))
             (with-scope k (for-each (lambda (thunk) (thunk)) v))))))
    (format #t "~d survived\n" n)))

(define (go1)
  ;;
  (set! *global-count* 0)
  (set! *short-data-backing* (make-string-table))
  ;;
  (call-with-output-file
      "migrate/to-axis/replay.unsort"
    (lambda (port)
      (set! *short-port* port)
      (format port "#|0| Axis Replay (short/-Z mode) --*-axis-replay-*--\n")
      (let ((a *application*))
        (for-each blowout (value-sequence (file-system-table a)))
        (for-each blowout (value-sequence (change-request-table a)))
        (table-for-each
         (get-property *application* 'migrate:master-node-index)
         (lambda (h k v)
           (blowout v)))
        (for-each blowout (value-sequence (group-table a)))
        (for-each blowout (value-sequence (user-table a)))
        (flush-deferred-links))))
  ;;
  (set! *short-port* (current-output-port)))

(define first-linked (lambda (node)
                       #f))

(define (node->hash (self <node>)) (persistent-hash self))

(set! $app-classes (vector-append $app-classes (vector node->hash)))


(define (go00)
  (rsfam)
  (set! first-linked (let ((tbl (get-property *application* 'migrate:first-linked-in))
                           (roots (let ((t (make-object-table)))
                                    (for-each 
                                     (lambda (fs)
                                       (table-insert! t (root-directory fs) fs))
                                     (value-sequence (file-system-table *application*)))
                                    t)))
                       (lambda (node)
                         (or (table-lookup tbl node)
                             (table-lookup roots node))))) 
  (build-new-state-map)
  (go1)
  (save-short-data-backing "migrate/to-axis/replay.sto"))

#|
...post-repairs...

rsf -q sourcebase.scm rsfam.scm -c.repl /tmp/a.fas
rsf -qimage /tmp/a.fas migrate/major3.scm -e '(go00)' -exit

(rsfam)

(go1)
(save-short-data-backing "migrate/to-axis/replay.sto")

... over in axis ...

lssctl -ob .test/test1/repository.axis 361 -.

(define ff (string->filesystem "doc-0.6"))
(root-directory ff)
(define cv1 (current-version (root-directory ff)))
(define ln1 (first-version (cdar (contents cv1))))

|#

#|
(rsfam)
(define fff (string->filesystem "rs-0.7"))

(define (show-current-versions fs)
  
  (define (rec nv path)
    (format #t "#~d|~a|~a|~a\n" 
            (id (versioned-object nv))
            (to-string (version-tag nv))
            (name fs)
            path)
    ;;
    (if (instance? nv <directory-version>)
        (for-each (lambda (sub)
                    (rec (current-version (cdr sub))
                         (if (string=? path "/")
                             (car sub)
                             (string-append path "/" (car sub)))))
                  (contents nv))))
  ;;
  (rec (current-version (root-directory fs)) "/"))

(with-output-to-file
    "/tmp/currentver.dat"
  (lambda ()
    (for-each
     show-current-versions
     (value-sequence (file-system-table *application*)))))

|#

