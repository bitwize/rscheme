;;;
;;;   Overall Output Pipeline
;;;
;;;   (1) Generate export nodes
;;;   (2) Refine their dependencies, if necessary
;;;   (3) Sort them topologically (using time to sort within a tie?)
;;;   (4) Evolve FS/file names in sort order
;;;   (5) Output the export nodes
;;;

  
(define (PAUSE)
  (with-module
      repl
    (cmd-loop *self* "pause[~d]=>>")))

,(use util.xml)

(load "to-axis-util.scm")

(define *special-audit-table* (make-object-table))

(define (init-special-audits)
  (table-insert! *special-audit-table*
                 (string->filesystem "rs-0.5")
                 (make <audit-log-entry>
                       user: "donovan"
                       operation: 'migrate
                       arg-list: '#()
                       result: #f
                       info: '()
                       timestamp: (string->time "1995-08-14 16:49:29 CDT")))
  (values))

(define (special-audit-check item regular-thunk)
  (or (table-lookup *special-audit-table* item)
      (regular-thunk)))

(define (nop))

(define-method owner ((self <audit-log-entry>))
  (user self))

(define (alloc-export-id)
  (table-size *export-database*))

(define-class <export-node> (<object>)
  (name type: <list>)
  (tie)
  (id type: <fixnum> init-function: alloc-export-id)
  (audit type: <audit-log-entry>)
  (prereqs type: <list> init-value: '())
  (preflight init-value: #f)    ; optional function for preflighting
  (sxml-generator type: <function>)
  (export-checker type: <function>)
  (description init-value: #f))

(define-method write-object ((self <export-node>) port)
  (format port "#[X #~s <~a>]" (id self) (timestamp (audit self))))

(define *export-database* (make-fixnum-table))
(define *export-item-map* (make-object-table))
(define *export-name-map* (make-table))

(define *new-state-map* (make-object-table))    ; <work-item> => <symbol>

;(define (node-version->hash (self <node-version>))  (transient->hash self))
;(define (make-node-version-table)(make-table eq? node-version->hash))

(define *root-dir->fs* (make-node-table))
(define *first-linked-in* (make-node-table))
(define *global-current-contents-map* (make-node-table))

;; thank the maker there are no circular structures in
;; rsfam or Xynthesis's sysfam...
(define *global-ref-count* (make-node-table))

(define *deletions* (make-node-table))
(define *master-node-index* (make-node-table))
(define *everlink-index* (make-node-table))

(define (reinit-simulation-tables)
  (set! *global-current-contents-map* (make-object-table))
  (set! *deletions* (make-object-table))
  ;;
  (table-for-each
   *master-node-index*
   (lambda (h k v)
     (table-insert! *global-ref-count* k 0))))

(define (reinit-export-tables)
  (set! *export-item-map* (make-object-table))
  (set! *export-database* (make-fixnum-table))
  (set! *export-name-map* (make-table))
  ;;
  ;; these are set during the repair phase
  (define (prop k)
    (get-property *application* k))
  ;;
  (set! *first-linked-in* (prop 'migrate:first-linked-in))
  (set! *master-node-index* (prop 'migrate:master-node-index))
  (set! *master-node-id-index* (prop 'migrate:master-node-id-index))
  (set! *everlink-index* (everlink))
  ;;
  (set! *new-state-map* (build-new-state-map))
  ;;
  (set! *root-dir->fs* (make-node-table))
  (for-each
   (lambda (fs)
     (table-insert! *root-dir->fs* (root-directory fs) fs))
   (value-sequence (file-system-table *application*))))

;;;==========================================================================
;;; Manage the global version map

(define-syntax (get-current-contents node)
  (or (table-lookup *global-current-contents-map* node)
      (error "No current contents for ~s!" node)))


(define-method in-effect ((self <directory>) contents)
  (assert (null? contents))     ; else, need to update *global-ref-count*
  (table-insert! *global-current-contents-map* 
                 self
                 contents))

(define-method in-effect ((self <directory-version>))
  (in-effect (versioned-object self) (contents self)))

(define (effect-link (self <directory>) (name <string>) (to <node>))
  (let ((cc (get-current-contents self)))
    ;;
    (if (assoc name cc)
        (error "link: Name already present in ~s: ~s" self name))
    ;;
    (add-ref to)
    (table-insert! *global-current-contents-map*
                   self
                   (cons (cons name to) cc))))

(define (add-ref (item <node>))
  (let ((n (add1 (table-lookup *global-ref-count* item))))
    (if (= n 1)
        (format #t "*** Node is initially created: ~s\n" item)
        (format #t "*** new link: Node ~s ref count = ~d\n" item n))
    (table-insert! *global-ref-count* item n)
    ;; subitems get bumped, too (we'd better, because subitems get 
    ;; decremented later!)
    (if (instance? item <directory>)
        (for-each
         (lambda (sub)
           (format #t "  hence, bump ref on ~s (~s)\n"
                   (car sub)
                   (cdr sub))
           (add-ref (cdr sub)))
         (get-current-contents item)))))


(define (remove-ref (item <node>))
  (let ((n (sub1 (table-lookup *global-ref-count* item))))
    (format #t "remove-ref ~s" item)
    (assert (>= n 0))
    (table-insert! *global-ref-count* item n)
    (if (= n 0)
        (begin
          (format #t " *** node is finally deleted\n")
          (if (instance? item <directory>)
              (for-each (lambda (sub)
                          (format #t "  hence, unlink ~s (~s)\n"
                                  (car sub)
                                  (cdr sub))
                          (remove-ref (cdr sub)))
                        (get-current-contents item))))
        (format #t " ... ref count = ~d\n" n))))

(define (effect-unlink (self <directory>) (name <string>) (to <node>) keep)
  (let* ((cc (get-current-contents self))
         (a (assoc name cc)))
    ;;
    (format #t "unlink from ~s: ~s (~s) (one of ~d)\n" 
            self
            to name (length cc))
    ;;
    (if (not a)
        (error "unlink: Name not present in ~s: ~s" self name))
    ;;
    (remove-ref to)
    ;;
    (if keep
        (table-insert! *deletions* 
                       (cdr a)
                       (cons keep
                             (or (table-lookup *deletions* (cdr a))
                                 '()))))
    ;;
    (table-insert! *global-current-contents-map*
                   self
                   (delq a cc))))


;;; Find a fileset and path that names the given node under
;;; the current set of node versions

(define-class <could-not-find-path> (<condition>)
  node
  (behalf init-value: #f))

(define-method consideration ((self <could-not-find-path>) port)
  (let ((d #f))
    (with-output-to-port
        port
      (lambda ()
        (set! d (track-down (node self)))))
    (format port "********************\n")
    (if (and d (behalf self))
        (begin
          (define (name-of n)
            (name (table-lookup *export-item-map* n)))
          (let ((c `(add-explicit-deps 
                     '((,(name-of d) ,(name-of (behalf self)))))))
            (format port "Consider:\n")
            (format port "  ~s\n" c)
            (format port "********************\n")
            c))
        #f)))

(define-method display-object ((self <could-not-find-path>) port)
  (format port "\n\n************ Could not find a path to ~s\n" 
          (node self))
  (if (behalf self)
      (format port "             on behalf of ~s\n" (behalf self)))
  ;;
  (consideration self port))

(define (identify-fs-path-to (self <node>) behalf)
  (let ((w self))
    (call-with-current-continuation
     (lambda (exit)
       ;;
       (define (scanfs fs node path)
         (if (eq? node self)
             (exit fs (reverse path))
             (if (instance? node <directory>)
                 (for-each
                  (lambda (sub)
                    (scanfs fs (cdr sub) (cons (car sub) path)))
                  (get-current-contents node)))))
       ;;
       (for-each
        (lambda (fs)
          (let ((r (root-directory fs)))
            (if (table-lookup *global-current-contents-map* r)
                (scanfs fs r '()))))
        (if (pair? behalf)
            behalf
            (value-sequence *root-dir->fs*)))
       ;;
       ;;
       (set! *cnf-error* (make <could-not-find-path>
                               node: self
                               behalf: behalf))
       (signal *cnf-error*)))))

(define *cnf-error* #f)

;;;==========================================================================
;;        
                            

(define-method initialize ((self <export-node>))
  (if (table-lookup *export-name-map* (name self))
      (error "Duplicate export node name: ~s" (name self)))
  ;;
  (table-insert! *export-name-map* (name self) self)
  (table-insert! *export-database* (id self) self)
  (let ((tied (table-lookup *export-item-map* (tie self))))
    (if (and tied (not (eq? tied '#undef)))
        (error "Duplicate tie ~s: ~s = ~s\n" (tie self) (name self) self)))
  (table-insert! *export-item-map* (tie self) self))

(define (exported-item self)
  (format #t "X ~50s" self)
  (cond
   ((table-lookup *export-item-map* self)
    => (lambda (m)
         (if (eq? m '#undef)
             (begin
               (format #t "  = (circular)\n")
               (error "Circularity at ~s" self))
             (begin
               (format #t "  = [~d] ~s\n" (id m) (name m))
               m))))
   (else
    (table-insert! *export-item-map* self '#undef)
    (format #t "  !\n")
    (let (((x <export-node>) (export-item self)))
      (format #t "< ~50s  * [~d] ~s\n" self (id x) (name x))
      x))))

(define (user-prereq u)
  (if (instance? u <user>)
      (list (exported-item u))
      '()))

(define-method genesis ((self <group>))
  (exported-item self))

(define-method genesis ((self <user>))
  (exported-item self))

(define *blank* 
  (delay (make <audit-log-entry>
               user: "admin"
               operation: 'migrate
               arg-list: '#()
               result: #f
               info: '()
               timestamp: (creation-time *application*))))

(define (last-or-blank l)
  (if (pair? l)
      (last l)
      (force *blank*)))

(define-method export-item ((self <group>))
  (make <export-node>
        tie: self
        name: `(group ,(name self))
        audit: (last-or-blank (audit-log self))
        prereqs: (append
                  (case (length (parent-groups self))
                    ((0) '())
                    ((1) (list (genesis (car (parent-groups self)))))
                    (else (error "Can't export multiple group parents")))
                  (user-prereq (owner self)))
        export-checker: nop
        description: (if (null? (parent-groups self))
                         #f
                         (~ "mkdom ~s" (name self)))
        sxml-generator: (lambda ()
                          (if (null? (parent-groups self))
                              ;; "world" already exists
                              `(NOP (why (root-domain)))
                              `(axis:create-domain
                                (name ,(name self))
                                (description
                                 ,(~ "Group ~s from SourceBase" 
                                     (name self)))
                                (in 
                                 ,(name (car (parent-groups self))))
                                (owner ,(name (owner self))))))))

(define-method export-item ((self <user>))
  (define (new)
    `(begin
       (axis:create-user
        (name ,(name self))
        (full-name ,(full-name self))
        (email-addr ,(email-addr self))
        ;; everybody is in "world" because SB
        ;; has no concept of a user's group
        (domain "world"))
       ;; automatically give everyone direct access, because we
       ;; are going to execute everything on their behalf using
       ;; that access method
       (axis:create-direct-access
        (subject ,(name self)))
       ;; also, make sure everyone has some serious authority, 
       ;; because it may be needed as well -- since SB never did
       ;; really implement authority, we have to assume everyone
       ;; has high auth.
       (axis:add-user-authority
        (subject ,(name self))
        (domain "world")
        (authority "domain-lead"))))
  ;;
  ;; forget about the host data; they'll be rebuilt manually
  ;; forget about the checkouts; they're all bogus at this point
  (make <export-node>
        tie: self
        name: `(user ,(name self))
        description: (~ "mkuser ~s" (name self))
        audit: (last-or-blank (audit-log self))
        prereqs: '()
        export-checker: nop
        sxml-generator: new))

;;;

(define-method originator ((self <change-request>))
  (get-property self 'requestor))

;;

(define (memqi item lst)
  (let loop ((i 0)
             (l lst))
    (if (null? l)
        (values)
        (if (eq? (car l) item)
            (values i (cdr l))
            (loop (+ i 1) (cdr l))))))
;;

(define-method export-item ((self <work-item>))
  (bind ((cr (base-request self))
         (i0 rest0 (memqi self (active-items cr)))
         (i rest (if i0
                     (values i0 rest0)
                     (memqi self (history cr))))
         (pred (if (pair? rest0)
                   (exported-item (car rest0))
                   (if (pair? rest)
                       (exported-item (car rest))
                       (exported-item cr)))))
    ;;
    (if (not i) (error "unlinked ~s" self))
    ;;
    (bind (((pre <list>)
            (gen <function>) 
            desc
            (export-work-item self (if i0 #t #f) i rest)))
      (make <export-node>
            tie: self
            name: `(cr ,(id cr) ,(if i0 'A 'H) ,i)
            description: (~ "editcr ~s ~a" (id cr) desc)
            audit: (if i0
                       (activate-audit-entry self)
                       (close-audit-entry self))
            prereqs: (cons pred (append (user-prereq (owner self)) pre))
            export-checker: nop
            sxml-generator: gen))))

(define-method export-work-item ((self <comment-request>) active? i rest)
  (if active? (error "Can't handle active comment requests"))
  ;;
  (values '()
          (if (null? rest)
              (lambda ()
                `(NOP (implicit-in-cr-create)))
              (lambda ()
                `(axis:add-comment
                  (owner ,(name (owner self)))
                  (subject ,(to-string (id (base-request self))))
                  (comment ,(comment self)))))))

(define-method export-work-item ((self <work-request>) active? i rest)
  (let ((ns (table-lookup *new-state-map* self)))
    ;;
    (define (any-fs-changes?)
      (find-first (lambda (wi)
                    (instance? wi <fs-change>))
                  (append (active-items 
                           (base-request self))
                          (history 
                           (base-request self)))))
    ;;
    (values 
     '()
     (if active?
         (lambda ()
           `(NOP (implicitly-after
                  (from ,(to-string (state self))))))
         (if (eq? (state self) ns)
             (lambda ()
               `(NOP
                 (why (null-transition ,(to-string (state self))))))
             (let ((s (case ns
                        ((check-off)
                         (if (any-fs-changes?)
                             (lambda ()
                               ;; a transition to the check-off (aka COMPLETE)
                               ;; state is automatic when the last FSC is
                               ;; completed
                               `(NOP (why (via-last-fs-change-close))))
                             ;; but if there are none, then it's an explicit
                             ;; transition
                             'complete))
                        ;;
                        ((fixing)
                         (if (any-fs-changes?)
                             (lambda ()
                               ;; a transition to the fixing (aka FIX) state
                               ;; is implicit when a change is done against
                               ;; the CR
                               `(NOP (why (implicit-fixing-via-checkin))))
                             ;; but if there are no FS changes, go to
                             ;; the research state instead
                             'research))
                        ;;
                        ((cancelled)    'canceled)
                        ((integration)  'integrate)
                        ((closed returned research) ns))))
               ;;
               (cond
                ((symbol? s)
                 ;; some transitions need an intermediate state in Axis
                 (if (and (memq (state self) '(research fixing open))
                          (eq? s 'closed))
                     (lambda ()
                       `(begin
                          (axis:modify-change-request
                           (subject ,(to-string (id (base-request self))))
                           (state "complete"))
                          (axis:modify-change-request
                           (subject ,(to-string (id (base-request self))))
                           (state "closed"))))
                     (lambda ()
                       `(axis:modify-change-request
                         (subject ,(to-string (id (base-request self))))
                         (state (@ (was ,(to-string (state self))))
                                ,(to-string s))))))
                ((procedure? s)
                 s)
                (else
                 (error "Can't handle new state '~s' => '~s'" ns s)))))))))

(define-method export-work-item ((self <integration-request>) active? i rest)
  (values '()
          (lambda ()
            `(NOP (implicity-after (fsc))))))

(define-method export-work-item ((self <code-review>) active? i rest)
  (values '()
          (if active?
              (lambda ()
                `(axis:request-comment
                  (subject ,(to-string (id (base-request self))))
                  (owner ,(name (owner self)))
                  (comment ,(~ "Please review '~a' changes" 
                               (name (group self))))))
              (lambda ()
                `(axis:add-comment
                  (subject ,(to-string (id (base-request self))))
                  (owner ,(name (owner self)))
                  (comment ,(~ "Changes to '~a' reviewed"
                               (name (group self)))))))))

(define-method export-work-item ((self <property-add>) active? i rest)
  (let ((key (name (the-property self)))
        (new-value (~ "XXX--~a--" (to-string (new-value self)))))
    (values '()
            (lambda ()
              `(axis:modify-change-request
                (subject ,(to-string (id (base-request self))))
                (plugin-add-attr
                 (item (@ (key ,key)) ,new-value)))))))
                
              

(define-method export-work-item ((self <property-change>) active? i rest)
  (let ((key (name (the-property self)))
        (new-value (~ "XXX--~a--" (to-string (new-value self)))))
    (values '()
            (lambda ()
              `(axis:modify-change-request
                (subject ,(to-string (id (base-request self))))
                (plugin-modify-attr
                 (item (@ (key ,key)) ,new-value)))))))

(define-method export-work-item ((self <title-change>) active? i rest)
  (values '()
          (lambda ()
            `(axis:modify-change-request
              (subject ,(to-string (id (base-request self))))
              (title ,(new-title self))))))

(define-method export-work-item ((self <fs-change>) active? i rest)
  (values
   (map exported-item (new-versions self))
   (if active?
       (lambda ()
         `(NOP (why (implicit))))
       (lambda ()
         `(axis:complete-fileset-changes
           (subject ,(to-string (id (base-request self))))
           (in (fileset ,(name (file-system self)))
               (snap "HEAD"))
           (owner ,(name (owner self))))))))

(define-method export-item ((self <change-request>))
  (let ((h (last (history self))))
    ;;
    (make <export-node>
          tie: self
          name: `(cr ,(id self))
          description: (~ "mkcr ~s" (id self))
          audit: (activate-audit-entry (last (history self)))
          prereqs: (list (genesis (group self))
                         (genesis (originator self)))
          export-checker: nop
          sxml-generator: (lambda ()
                            `(axis:create-change-request
                              (name ,(to-string (id self)))
                              (title ,(title self))
                              (comment ,(if (instance? h <comment-request>)
                                            (comment h)
                                            (title self)))
                              (domain ,(name (group self)))
                              (originator ,(name (originator self)))
                              ;; XXX plugin values
                              )))))

;;;

(define (topological-sort-export-nodes lst)
  (if (null? lst)
      '()
      (let* ((h (car lst)))
        (if (any? (lambda (n)
                    (memq h (prereqs n)))
                  (cdr lst))
            (topological-sort-export-nodes (append (cdr lst) (list (car lst))))
            (cons h (topological-sort-export-nodes (cdr lst)))))))
  
#|
(define (export-to-axis)
  ;;
  (define (export-index tbl)
    (for-each exported-item (value-sequence tbl)))
  ;;
  (reinit-export-tables)
  ;;
  (export-index (user-table *application*))
  (export-index (group-table *application*))
  (export-index (change-request-table *application*))
  ;;
  (values))
|#

;;;
;;;  Build a table which maps <work-request> work items to the
;;;  "new" state value

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

;;;
;;;  Find all nodes, and also return a list of all directory nodes
;;;

(define (find-all-nodes)
  (let ((tbl (make-node-table))
        (dirs '())
        (q (make-dequeue)))
    ;;
    (define (schedule-node n)
      (if (not (table-lookup tbl n))
          (begin
            (dequeue-push-back! q n)
            (table-insert! tbl n #t))))
    ;;
    (define (visit-node n)
      (table-insert! tbl n n)
      (if (instance? n <directory>)
          (begin
            (set! dirs (cons n dirs))
            (for-each-version
             (versions n)
             (lambda (path vleaf)
               (for-each (lambda (c)
                           (schedule-node (cdr c)))
                         (contents (value vleaf))))))))
    ;;
    (for-each
     (lambda (fs)
       (dequeue-push-back! q (root-directory fs))
       (values))
     (value-sequence (file-system-table *application*)))
    ;;
    (let loop ()
      (if (dequeue-empty? q)
          (values tbl dirs)
          (begin
            (visit-node (dequeue-pop-front! q))
            (loop))))))


;;;

(define (first-version (self <node>))
  (value (find-leaf (versions self) '(1 1))))

(define-method export-item ((self <file-system>))
  (let ((fv (first-version (root-directory self))))
    (make <export-node>
          tie: self
          name: `(fs ,(name self))
          audit: (special-audit-check
                  self
                  (lambda () (node-version-audit fv)))
          prereqs: (list (exported-item (group self))
                         (exported-item (owner self)))
          export-checker: (lambda ()
                            (in-effect fv)
                            (add-ref (root-directory self)))
          sxml-generator: (lambda ()
                            ;; add the root dir to the global map
                            (in-effect fv)
                            (add-ref (root-directory self))
                            `(axis:create-fileset
                              (name ,(name self))
                              (domain ,(name (group self)))
                              (owner ,(name (owner self)))
                              ;; we have to create the FS as uncontrolled
                              ;; because we can't tell when, if ever, it
                              ;; transitions to CONTROLLED state :-(
                              (type "uncontrolled"))))))

(define (explain (self <node-version>))
  (let ((why (map (lambda (fsc)
                    `(item ,(to-string (id (base-request fsc)))))
                  (select (lambda (w)
                            (instance? w <fs-change>))
                          (change-items self)))))
    (append
     (if (null? why)
         '()
         `((reasons ,@why)))
     (if (comment self)
         `((comment ,(comment self)))
         '()))))
  
(define (describe-dir-changes (self <directory-version>) ln unlink rename)
  (call-with-output-string
   (lambda (port)
     (format port "edit;")
     (if (pair? unlink)
         (begin
           (format port " rm")
           (for-each 
            (lambda (e)
              (format port " ~a" (car e)))
            unlink)
           (format port ";")))
     (if (pair? ln)
         (begin
           (format port " ln")
           (for-each 
            (lambda (e)
              (format port " ~a" (car e)))
            ln)
           (format port ";")))
     (if (pair? rename)
         (begin
           (format port " mv")
           (for-each 
            (lambda (e)
              (format port " ~a,~a" (car e) (cadr e)))
            rename))))))
     
     
(define (effect-dir-changes (self <directory-version>) ln unlink rename)
  (bind ((snode (versioned-object self))
         (fs path (identify-fs-path-to snode self)))
    ;;
    (for-each 
     (lambda (e)
       (effect-unlink snode (car e) (cdr e) self))
     unlink)
    ;;
    (for-each 
     (lambda (e)
       (effect-link snode (car e) (cdr e)))
     ln)
    ;;
    (for-each 
     (lambda (e)
       (effect-link snode (cadr e) (caddr e))
       (effect-unlink snode (car e) (caddr e) #f)
       )
     rename)))
    ;;

(define (gen-dir-changes (self <directory-version>) ln unlink rename)
  ;; (1) all the creation action will be
  ;;     handled by the first versions of
  ;;     the prereq node
  ;; (2) unlink and rename action is handled
  ;;     here
  (bind ((q (make-dequeue))
         (snode (versioned-object self))
         (fs path (identify-fs-path-to snode self))
         (pathitems (map (lambda (i)
                           `(item ,i))
                         path))
         (expl (explain self)))
    ;;
    (define (app item)
      (dequeue-push-back! q item)
      (values))
    ;;
    (for-each 
     (lambda (e)
       (effect-unlink snode (car e) (cdr e) self)
       (app
        `(,(cond
            ((instance? (cdr e) <file>) 'axis:delete-file)
            ((instance? (cdr e) <directory>) 'axis:delete-directory)
            (else (error "huh?")))
          ;;
          (in (fileset ,(name fs)) (snap "HEAD"))
          (path ,@pathitems (item ,(car e)))
          ,@expl)))
     unlink)
    ;;
    (for-each 
     (lambda (e)
       (effect-link snode (car e) (cdr e))
       (app
        `(,(cond
            ((instance? (cdr e) <file>) 'axis:link-file)
            ((instance? (cdr e) <directory>) 'axis:link-directory)
            (else (error "huh?")))
          ;;
          (in (fileset ,(name fs)) (snap "HEAD"))
          (path ,@pathitems (item ,(car e)))
          ,@expl)))
     ln)
    ;;
    (for-each 
     (lambda (e)
       (effect-unlink snode (car e) (caddr e) #f)
       (effect-link snode (cadr e) (caddr e))
       (app
        `(axis:rename-node
          ;;
          (in (fileset ,(name fs)) (snap "HEAD"))
          (path ,@pathitems (item ,(car e)))
          (new-path ,@pathitems (item ,(cadr e)))
          ,@expl)))
     rename)
    ;;
    (if (dequeue-empty? q)
        `(NOP (nothing-left-to-do))
        `(begin ,@(vector->list (dequeue-state q))))))
  
;;;
;;;  Find all the nodes that will become detached if we unlink
;;;  the given node
;;;

(define (find-detaches-from-unlink (in <directory>) 
                                   (name <string>)
                                   (node <node>))
  (let ((refs *global-ref-count*)
        (r '()))
    (define (rm n)
      (let ((k (sub1 (table-lookup refs n))))
        (if (= k 0)
            (begin
              (set! r (cons n r))
              (if (instance? n <directory>)
                  (for-each
                   (lambda (sub)
                     (rm (cdr sub)))
                   (get-current-contents n)))))))
    (rm node)
    r))

(define *internal/unlink-preflight-failures* #f)
(define *internal/unlink-preflight-verbose?* #f)

(define (make-unlinking-preflighter (in <directory-version>) unlink)
  (let ((inn (versioned-object in)))
    (lambda (past)
      (let ((ok #t)
            (why (make-dequeue)))
        (set! *internal/unlink-preflight-failures* why)
        (for-each
         (lambda (ul)
           (let* ((name (car ul))
                  (node (cdr ul))
                  (will-detach (find-detaches-from-unlink inn name node)))
             (if *internal/unlink-preflight-verbose?*
                 (format #t "unlinking ~s => ~s will detach:\n" name node))
             (for-each
              (lambda ((n <node>))
                (if *internal/unlink-preflight-verbose?*
                    (format #t "  will detach ~s\n" n))
                (for-each
                 (lambda ((p <pair>))
                   (let ((node-version (car p))
                         (name (cdr p)))
                     (if *internal/unlink-preflight-verbose?*
                         (format #t "  : ~s in ~s" name node-version))
                     (let ((x (table-lookup *export-item-map* node-version)))
                       (if (and x (table-lookup past (id x)))
                           (if *internal/unlink-preflight-verbose?*
                               (format #t "  [handled]\n"))
                           (begin
                             (if *internal/unlink-preflight-verbose?*
                                 (format #t "  <=== NOT YET\n"))
                             (dequeue-push-back! why node-version)
                             (set! ok #f))))))
                 (or (table-lookup *everlink-index* n)
                     (error "Not in Everlink Index: ~s" n))))
              will-detach)))
         unlink)
        ok))))

(define (export-dir-version/subseq (self <directory-version>) pred node preq)
  ;; There is a previous version, so there are just
  ;; some changes being made...
  (bind ((ln unlink rename (dir-changes (contents pred) 
                                        (contents self)))
         (link-old (select (lambda (l)
                             (not (eq? (cdr (table-lookup *first-linked-in*
                                                          (cdr l)))
                                       self)))
                           ln)))
    (format #t "     ==============\n" self)
    (format #t "     link ~s\n" link-old)
    (format #t "     unlink ~s\n" unlink)
    (format #t "     rename ~s\n" rename)
    #|
    ;;
    (if (null? (append link-old unlink rename))
        (format #t "Nothing to do for ~s => ~a\n" pred (version-tag self)))
    |#
    ;;
    ;; Hmm.  If there is an UNLINK operation that removes the
    ;;       last link to a node (version), it should prereq
    ;;       at least one LINK operation, if there are any...
    ;;       Otherwise, the object will be inaccessible after
    ;;       the unlink...  That's what renames across directories
    ;;       will look like...
    ;;
    (let ((a (node-version-audit self)))
      (make <export-node>
        tie: self
        name: `(node ,(id (versioned-object self)) 
                     ,(to-string (version-tag self)))
        preflight: (if (pair? unlink)
                       (make-unlinking-preflighter self unlink)
                       #f)
        description: (describe-dir-changes self link-old unlink rename)
        audit: a
        prereqs: (append (list preq)
                         (user-prereq (user a))
                         ;; this one depends on all the nodes
                         ;; for which we are not the first-linking
                         ;; version
                         (map
                          (compose exported-item first-version cdr) 
                          link-old)
                         (map
                          (compose exported-item first-version cdr)
                          unlink)
                         (map
                          (compose exported-item first-version caddr) 
                          rename))
        export-checker: (lambda ()
                          (effect-dir-changes self
                                              link-old
                                              unlink
                                              rename))
        sxml-generator: (lambda ()
                          (gen-dir-changes self
                                           link-old
                                           unlink
                                           rename))))))
(define (export-dir-version/first (self <directory-version>) node preq fli)
  ;;
  ;; No previous version of this directory, but it is
  ;; linked into some other directory...
  ;;
  (let ((a (node-version-audit self)))
    (make <export-node>
      tie: self
      name: `(node ,(id (versioned-object self)) 
                   ,(to-string (version-tag self)))
      audit: a
      prereqs: (cons* preq
                      (genesis (group (versioned-object self)))
                      (append
                       (user-prereq (user a))
                       (nv-cr-prereqs self (cdr fli))))
      export-checker: (lambda ()
                        (bind ((fs path 
                                   (identify-fs-path-to 
                                    (versioned-object (cdr fli))
                                    self)))
                          (in-effect self)
                          (effect-link
                           (versioned-object (cdr fli))
                           (car fli)
                           (versioned-object self))))
      sxml-generator: (lambda ()
                        (bind ((fs path (identify-fs-path-to 
                                         (versioned-object (cdr fli))
                                         self)))
                          
                          ;; add our 1st v to the global map
                          (in-effect self)
                          ;; record the link in parent dir
                          (effect-link
                           (versioned-object (cdr fli))
                           (car fli)
                           (versioned-object self))
                          ;;
                          `(axis:create-directory
                            (in (fileset ,(name fs))
                                (snap "HEAD"))
                            (path ,@(map (lambda (p)
                                           `(item ,p))
                                         path)
                                  (item ,(car fli)))
                            (mode ,(to-string
                                    (permissions self)))
                            (domain
                             ,(name
                               (group
                                (versioned-object self))))
                            ,@(explain (cdr fli))))))))

(define (export-dir-version/root (self <directory-version>) pred node preq)
  ;; No previous version, and it is the root directory.  Do nothing,
  ;; but depend on the FS object itself (which is what preq is)
  ;;
  (make <export-node>
    tie: self
    name: `(node ,(id node)
                 ,(to-string (version-tag self)))
    audit: (node-version-audit self)
    prereqs: (list preq)
    export-checker: nop
    sxml-generator: (lambda ()
                      `(NOP
                        (why 
                         (root ,(name (table-lookup *root-dir->fs* node))))))))

(define-method export-item ((self <directory-version>))
  (let* ((pred (previous-version self))
         (node (versioned-object self))
         (preq (exported-item (or pred node))))
    ;;
    (format #t "  ~s => ~s (~s)\n" self pred preq)
    ;;
    (cond
     ;;
     (pred
      (export-dir-version/subseq self pred node preq))
     ;;
     ((table-lookup *first-linked-in* node)
      => (lambda (fli)
           (export-dir-version/first self node preq fli)))
     ;;
     (else
      (export-dir-version/root self pred node preq)))))

(define (node-version-audit (self <node-version>))
  (make <audit-log-entry>
        timestamp: (modification-time self)
        operation: 'checkin
        arg-list: '#()
        result: #f
        info: '()
        user: (if (pair? (change-items self))
                  (owner (car (change-items self)))
                  "admin")))

(define-method export-item ((self <node>))
  (let ((fli (table-lookup *first-linked-in* self))
        (g (genesis (group self))))
    (make <export-node>
          tie: self
          name: `(node ,(id self))
          audit: (node-version-audit (first-version self))
          export-checker: nop
          sxml-generator: (lambda ()
                            `(NOP
                              (why (node ,(to-string (id self))))))
          prereqs: (if fli
                       (list (exported-item (cdr fli)) g)
                       (cond
                        ((table-lookup *root-dir->fs* self)
                         => (lambda (fs)
                              (list (exported-item fs) g)))
                        (else
                         (error "~s: no FLI and not a root" self)))))))

(define (nv-cr-prereqs . nvlist)
  (let ((t (make-object-table)))
    (for-each
     (lambda ((self <node-version>))
       (for-each
        (lambda (ch)
          ;; not only do we want to skip the occasional <audit-log-entry>
          ;; (e.g., see #[<file-version> 2627#1.9] in rsfam), but we
          ;; also get to eliminate duplicates
          (if (instance? ch <fs-change>)
              (table-insert! t (base-request ch) #t)))
        (change-items self)))
     nvlist)
    ;;
    (map exported-item (key-sequence t))))

(define-method export-item ((self <file-version>))
  (let* ((pred (previous-version self))
         (node (versioned-object self))
         (fli (table-lookup *first-linked-in* node))
         (preq (exported-item (or pred node))))
    ;;
    (define (checkin new? fs path)
      (let ((loc `((in (fileset ,(name fs)) (snap "HEAD"))
                   (path ,@(map (lambda (p)
                                  `(item ,p))
                                path))))
            (v (to-string (version-tag self)))
            (data (content->string (contents self))))
        `(begin
           ;;
           ;; step 1, create or lock the file 
           ;;
           (,(if new?
                 'axis:create-file
                 'axis:lock-file)
            ,@(if new?
                  `((domain ,(name (group (versioned-object self)))))
                  '())
            ,@loc)
           ;;
           ;; step 2, arrange for the upload of data
           ;;
           (axis:upload-file-content
            ,@loc
            (version ,v)
            (size ,(to-string (string-length data))))
           ;;
           ;; step 3. retire the lock
           ;;
           (axis:retire-lock
            ,@loc
            (version ,(to-string (version-tag self)))
            ,@(explain self)
            (mode ,(to-string (permissions self)))
            (version ,v)
            (evidence
             (body (@ (enc "base64/gzip"))
                   ,(base64-gzip-encode data)))))))
    ;;
    (let ((a (node-version-audit self)))
      (if pred
          (make <export-node>
                tie: self
                name: `(node ,(id (versioned-object self))
                             ,(to-string (version-tag self)))
                audit: a
                export-checker: (lambda ()
                                  (identify-fs-path-to
                                   (versioned-object self)
                                   self))
                sxml-generator: (lambda ()
                                  (bind ((fs path (identify-fs-path-to
                                                   (versioned-object self)
                                                   self)))
                                    (checkin #f fs path)))
                prereqs: (cons* preq
                                (genesis (group (versioned-object self)))
                                (append
                                 (user-prereq (user a))
                                 (nv-cr-prereqs self))))
          (make <export-node>
                tie: self
                name: `(node ,(id (versioned-object self))
                             ,(to-string (version-tag self)))
                audit: a
                export-checker: (lambda ()
                                  (bind ((fs path (identify-fs-path-to
                                                   (versioned-object
                                                    (cdr fli))
                                                   self)))
                                    (effect-link
                                     (versioned-object (cdr fli))
                                     (car fli)
                                     (versioned-object self))))
                sxml-generator: (lambda ()
                                  (bind ((fs path (identify-fs-path-to
                                                   (versioned-object
                                                    (cdr fli))
                                                   self)))
                                    (effect-link
                                     (versioned-object (cdr fli))
                                     (car fli)
                                     (versioned-object self))
                                    (checkin #t fs (append path 
                                                           (list (car fli))))))
                prereqs: (cons* preq
                                (exported-item (group (versioned-object self)))
                                (append
                                 (user-prereq (user a))
                                 (nv-cr-prereqs self (cdr fli)))))))))

;;;        

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

;;;

(define (tt)
  (reinit-export-tables)
  (let* ((fs (string->filesystem "rs-0.7"))
         (n (find-node fs (string->fs-path "/modules"))))
    ;;
    (export-item n)))

(define (ttt)
  (export-to-axis))

(define (export-to-axis)
  (reinit-export-tables)
  ;; going beyond 7 and 1...
  (with-output-to-file
      "/tmp/prescan.log"
    (lambda ()
      (for-each
       (lambda (cr)
         (for-each exported-item (history cr))
         (for-each exported-item (active-items cr)))
       (sort
        (value-sequence (change-request-table *application*))
        (lambda (a b)
          (< (id a) (id b)))))
      ;;
      (for-each
       exported-item
       (value-sequence (user-table *application*)))
      ;;
      (for-each
       exported-item
       (value-sequence (group-table *application*)))
      ;;
      (for-each
       (lambda (n)
         (exported-item n)
         (exported-item (current-version n)))
       (sort
        (value-sequence *master-node-id-index*)
        (lambda (a b)
          (< (id a) (id b))))))))

;;;

(define (node-user (self <export-node>))
  (audit-user (audit self)))

(define (audit-user (self <audit-log-entry>))
  (let ((u (user self)))
    (cond
     ((string? u) 
      (if (string=? u "sysadm")
          "admin"
          u))
     ((instance? u <user>) (name u))
     (else
      (error "Can't interpret audit user = ~s" u)))))

(define (timestr t)
  (time->string t "%Y-%m-%d %H:%M:%S %Z"))

(define (audit-time a)
  (timestr (timestamp a)))

(define (node-time n)
  (audit-time (audit n)))

(define (make-axis-transaction (n <export-node>) what)
  `(axis-transaction
    (who (@ (name ,(node-user n))))
    (how (@ (type "direct")))
    (when (@ (unix ,(~ "~.0f" (time->epoch-seconds (timestamp (audit n))))))
          ,(node-time n))
    (what
     ,@(if (eq? (car what) 'begin)
           (cdr what)
           (list what)))))

;;;
;;;  Sort the export nodes into time order, preserving the
;;;  dependencies

(define-syntax (del-best! lst valid? getter better?)
  (let (((lst <pair>) lst))
    (let loop ((l lst)
               (p #f)
               (bestt #f)
               (bestp #f))
      ;(format #t "~s ~s  best: ~s ~s\n" l p bestt bestp)
      (if (null? l)
          (if bestt
              (if bestp
                  (let ((x (cadr bestp)))
                    (set-cdr! bestp (cddr bestp))
                    (values x lst))
                  (values (car lst) (cdr lst)))
              (values))
          (let ((t (getter (car l))))
            (if (and (or (not bestt)
                         (better? t bestt))
                     (valid? (car l)))
                (loop (cdr l) l t p)
                (loop (cdr l) l bestt bestp)))))))
              
(define (del-earliest-time (lst <pair>) past)
  (del-best! lst
             (lambda (x) 
               (let ((pre (preflight x)))
                 (if pre
                     (pre past)
                     #t)))
             (lambda (x) (timestamp (audit x)))
             time<?))

(define (del-earliest-time/no-preflight (lst <pair>) past)
  (del-best! lst
             (lambda (x)
               #t)
             (lambda (x) (timestamp (audit x)))
             time<?))
  
(define *internal/past* #f)
(define *internal/pending* #f)
(define *internal/ready* #f)
(define *internal/output* #f)

(define (in-topological-time-order proc seed)
  (let ((past (make-fixnum-table))
        (pending (make-fixnum-table))   ; key=waiting node (id)
                                        ; value=nodes waiting on it to unblock
        (ready '())
        (output (make-dequeue)))
    ;;
    (set! *internal/past* past)
    (set! *internal/output* output)
    (set! *internal/pending* pending)
    (set! *internal/ready* (lambda () ready))
    ;;
    ;; initialize the pending index and ready list
    ;;
    (for-each 
     (lambda (n)
       (let ((p (prereqs n)))
         (if (null? p)
             (begin
               (format #t "is ready:   ~20s ~s\n" (name n) n)
               (set! ready (cons n ready)))
             (let ((k (id (car p))))
               ;(format #t "is pending: ~20s ~s on ~s\n" (name n) n k)
               (table-insert! pending 
                              k 
                              (cons (cons n (cdr p))
                                    (or (table-lookup pending k)
                                        '())))))))
     (value-sequence *export-item-map*))
    ;;
    ;(format #t "Initially\n   ready = ~s\n======\n" (map id ready))
    ;(print pending)
    ;;
    (let loop ((x seed))
      (if (null? ready)
          (if (> (table-size pending) 0)
              (let ((o (make-object-table)))
                ;;
                (table-for-each
                 past
                 (lambda (h k v)
                   (table-insert! o (table-lookup *export-database* k) #t)))
                ;;
                ;;(print pending)
                (for-each
                 (lambda (still)
                   (let ((x (table-lookup *export-database* still)))
                     (format #t "--- PENDING: ~s ---\n" x)
                     (print-prereq-tree x o)))
                 (key-sequence pending))
                ;;
                (error "Still ~s pending ?!?" (table-size pending)))
              x)
          (bind ((save (append ready '()))
                 (n now-ready (del-earliest-time ready past)))
            ;;
            (if (not n)
                (begin
                  ;;(print save)
                  ;;(error "Nothing ready in ready list!")
                  (bind ((n1 nr1 (del-earliest-time/no-preflight ready 
                                                                 past)))
                    (format #t "****** Nothing is ready, nominating ~s *****\n"
                            n1)
                    (set! n n1)
                    (set! now-ready nr1))))
            ;;
            (if (not (= (+ 1 (length now-ready)) (length save)))
                (begin
                  (format #t "woops!\n")
                  (format #t "   ready => (~d) ~s\n" (length save) save)
                  (format #t "   now-ready => (~d) ~s\n" (length now-ready) now-ready)
                  (format #t "   top => ~s\n" n)
                  (error "woops")))

            ;(format #t "s = ~#*@50s\n" (map id s))
            (set! ready now-ready)
            ;;
            ;(format #t "~s : ~s\n" (timestamp (audit n)) n)
            (table-insert! past (id n) n)
            (dequeue-push-back! output n)
            ;;
            (for-each 
             (lambda ((waiter <pair>))
               ;; car == <export-node>
               ;; cdr == list of prereqs still to check
               (let wloop ((w (cdr waiter)))
                 (if (null? w)
                     ;; it isn't waiting on anything else that isn't past...
                     ;; release it
                     (set! ready (cons (car waiter) ready))
                     (let ((k (id (car w))))
                       (if (table-lookup past k)
                           ;; this prereq is satisified already
                           (wloop (cdr w))
                           ;; it is waiting on something else (#k)
                           (begin
                             (set-cdr! waiter (cdr w))
                             (table-insert! pending
                                            k
                                            (cons waiter
                                                  (or (table-lookup pending k)
                                                      '())))))))))
             (or (table-remove! pending (id n)) '()))
            (loop (proc n x)))))))
    
(define (in-topological-order proc seed)
  (let ((past (make-fixnum-table))
        (pending (make-fixnum-table))
        (ready '()))
    ;;
    ;; initialize the pending index and ready list
    ;;
    (for-each 
     (lambda (n)
       (let ((p (prereqs n)))
         (if (null? p)
             (begin
               ;(format #t "is ready: ~s\n" n)
               (set! ready (cons n ready)))
             (let ((k (id (car p))))
               ;(format #t "is pending: ~s on ~s\n" n k)
               (table-insert! pending 
                              k 
                              (cons (cons n (cdr p))
                                    (or (table-lookup pending k)
                                        '())))))))
     (value-sequence *export-item-map*))
    ;;
    ;(format #t "Initially\n   ready = ~s\n======\n" (map id ready))
    ;(print pending)
    ;;
    (let loop ((x seed))
      (if (null? ready)
          (if (> (table-size pending) 0)
              (begin
                (print pending)
                (error "Still ~s pending ?!?" (table-size pending)))
              x)
          (let* ((n (car ready)))
            ;(format #t "s = ~#*@50s\n" (map id s))
            (set! ready (cdr ready))
            ;;
            ;(format #t "~s : ~s\n" (timestamp (audit n)) n)
            (table-insert! past (id n) n)
            ;;
            (for-each 
             (lambda ((waiter <pair>))
               ;; car == <export-node>
               ;; cdr == list of prereqs still to check
               (let wloop ((w (cdr waiter)))
                 (if (null? w)
                     ;; it isn't waiting on anything else that isn't past...
                     ;; release it
                     (set! ready (cons (car waiter) ready))
                     (let ((k (id (car w))))
                       (if (table-lookup past k)
                           ;; this prereq is satisified already
                           (wloop (cdr w))
                           ;; it is waiting on something else (#k)
                           (begin
                             (set-cdr! waiter (cdr w))
                             (table-insert! pending
                                            k
                                            (cons waiter
                                                  (or (table-lookup pending k)
                                                      '())))))))))
             (or (table-remove! pending (id n)) '()))
            (loop (proc n x)))))))
;;

(define (check-exports #optional time?)
  ;; like print-exports, but just keeps track of the order
  ;; and doesn't output anything
  (let ((q (make-dequeue)))
    ;;
    (reinit-simulation-tables)
    ;;
    ((if time? 
         in-topological-time-order
         in-topological-order)
     (lambda (n j)
       (let ((i (id n)))
         ;(format #t "~05d: ~23s\n" i (name n))
         (dequeue-push-back! q n)
         (let ((x ((export-checker n))))
           (+ j 1))))
     0)
    (dequeue-state q)))

(define-method node-script* ((self <object>))
  (error "no node-script for: ~s" self))

(define (sh-escape s)
  s)

(define (reason-id-list lst)
  (string-join #\space 
               (map (lambda ((wi <fs-change>))
                      (to-string (id (base-request wi))))
                    (select (lambda (x)
                              (instance? x <fs-change>))
                            lst))))
   
(define (sh . args)
  (bind ((r (memq 'reasons: args))
         (args rlist (if r
                         (values
                          (reverse (cdr (memq 'reasons: (reverse args))))
                          (map (lambda ((wi <fs-change>))
                                 (to-string (id (base-request wi))))
                               (cadr r)))
                         (values args '()))))
    (if (pair? rlist)
        (string-join " " (map sh-escape
                              (append (list (car args)
                                            (cadr args)
                                            "-y"
                                            (string-join " " rlist))
                                      (cddr args))))
        (string-join " " (map sh-escape args)))))

(define psx (lambda (type subj details sh #key 
                          (user default: "nobody") 
                          (time default: "1990-01-01 00:00:00 CST"))
              (values)))

(define (node-script (self <export-node>))
  (node-script* (tie self)))

(define nonsql-pattern (reg-expr->proc '(+ (not (or alpha digit #\space
                                                    #\! #\# #\$ #\% #\&
                                                    #\( #\) #\* #\+ #\,
                                                    #\- #\. #\/ #\: #\;
                                                    #\< #\= #\> #\? #\@
                                                    #\[ #\] #\^ #\_ #\`
                                                    #\{ #\} #\~)))))

(define (cleanup-detail-str str)
  (let ((x (trim-whitespace
            (string-join #\space (string-split str nonsql-pattern)))))
    (if (> (string-length x) 99)
        (substring x 0 99)
        x)))

(define (encode-arg-str s)
  (let ((plain? (not (or (nonsql-pattern s) 
                         (> (string-length s) 99)))))
    (if plain?
        s
        (string-append (trim-whitespace (cleanup-detail-str s)) "+"))))

(define *short-data-backing* (make-string-table))
(define *short-data-backing-ps* #f)

(define (load-short-data-backing file)
  (set! *short-data-backing-ps* (open-persistent-store file))
  (set! *short-data-backing* (root-object *short-data-backing-ps*)))

(define (save-short-data-backing file)
  (let ((ps (create-persistent-store file)))
    (commit ps *short-data-backing*)
    (close-persistent-store ps)))

(define (strip-extended-kwd-arg args key default-thunk)
  (let ((p (memq key args)))
    (if p
        (values
         (append (reverse (cdr (memq key (reverse args))))
                 (cddr p))
         (cadr p))
        (values args (default-thunk)))))

(define (print-short-export port (n <export-node>) (j <fixnum>))
  ;;
  (let ((q 0))
    (set! psx 
          (lambda (type #rest args)
            (bind ((args user (strip-extended-kwd-arg args
                                                      'user:
                                                      (lambda ()
                                                        (node-user n))))
                   (args time (strip-extended-kwd-arg args
                                                      'time:
                                                      (lambda ()
                                                        (timestamp 
                                                         (audit n)))))
                   (uid (to-string (transient->persistent (tie n))))
                   (key (~ "~a~03d.~02d"
                           (integer->char (+ (char->integer #\A)
                                             (quotient j 1000)))
                           (modulo j 1000)
                           q))
                   (short-entry (list (list key j q (id n) (name n))
                                       uid
                                       user
                                       time
                                       type
                                       args)))
              ;;
              ;;
              (if *short-data-backing*
                  (table-insert! *short-data-backing* key short-entry))
              ;;
              (write-string port
                            (string-append
                             " "
                             (string-join #\| (saved-short-entry->all-fields
                                               short-entry))
                             "\n"))
              #|
              (format port "~a|~a|~a|~a|~a|~a\n"
                      key
                      uid
                      user
                      (timestr time)
                      type
                      (string-join #\| (map encode-arg-str 
                                            (select (lambda (a)
                                                      (not (pair? a)))
                                                    args))))
              |#
              (set! q (+ q 1)))))
    ;;
    (node-script n)))

(define (saved-short-entry->all-fields saved)
  (let ((key (car (list-ref saved 0)))
        (uid (list-ref saved 1))
        (user (list-ref saved 2))
        (time (list-ref saved 3))
        (type (list-ref saved 4))
        (args (list-ref saved 5)))
    ;;
    (append 
     (list key
           ;uid
           user
           (timestr time)
           (symbol->string type))
     (map encode-arg-str 
          (select (lambda (a)
                    (not (pair? a)))
                  args)))))



(define (print-exports)
  (reinit-simulation-tables)
  ;;
  (call-with-output-file
      "/tmp/exports.dat"
    (lambda (dport)
      (call-with-output-file
          "/tmp/exports.xml"
        (lambda (port)
          (in-topological-time-order
           (lambda (n j)
             (let ((i (id n)))
               ;;
               (print-short-export dport n j)
               (flush-output-port dport)
               (format port "\n<!-- <~d> ~05d: ~s -->\n" j i (name n))
               (format #t "~05d: ~23s " i (name n))
               (flush-output-port (current-output-port))
               (let ((x ((sxml-generator n))))
                 (format #t "~#@*90s\n" x)
                 #|
                 (if (not (eq? (car x) 'NOP))
                     (write-sxml
                      (pretty-printify-xml (make-axis-transaction n x))
                      port))
                 (newline port)
                 (flush-output-port port)
                 |#
                 (+ j 1))))
           0)
          ;;
          (format port "\n<!-- now we can commit it all -->\n\n")
          (write-sxml
           (pretty-printify-xml
            `(axis-transaction
              (who (@ (name "admin")))
              (how (@ (type "direct")))
              (when (@ (unix ,(~ "~.0f" (time->epoch-seconds (time))))))
              (what
               (axis:commit))))
           port)
          (format port "\n\n<!-- END -->\n"))))))


(define *master-node-id-index* (make-fixnum-table))

(define (add-explicit-deps lst)
  (let ((nmap *export-name-map*))
    #|(make-table)
    (table-for-each
     *export-database*
     (lambda (h k v)
       (table-insert! nmap (name v) v)))
    |#
    ;;
    (for-each (lambda (l)
                (let ((src (table-lookup nmap (car l)))
                      (dst (table-lookup nmap (cadr l))))
                  (or src (error "Cannot find source ~s\n" (car l)))
                  (or dst (error "Cannot find dest ~s\n" (cadr l)))
                  (set-prereqs! src (cons dst (prereqs src)))))
              lst)))

(define (again)
  (load "migrate/to-axis-topo.scm")
  (ttt)
  (add-explicit-deps '(((node 1 "1.12") (node 1942 "1.1"))))
  ;;
  (print-exports))

(define (occurs-in-dirs node)
  (let ((r '()))
    (table-for-each
     *global-current-contents-map*
     (lambda (h k v)
       (if (memq node (map cdr v))
           (set! r (cons k r)))))
    r))

(define (track-down node)
  (format #t "Tracking down: ~s\n" node)
  (let ((del #f))
    ;;
    (define (foccur n depth)
      (let ((o (occurs-in-dirs n)))
        (if (pair? o)
            (begin
              (format #t "~a~s occurs in:\n" (make-string depth #\space) n)
              (for-each (lambda (s)
                          (foccur s (+ depth 4)))
                        o))
            (let ((d (table-lookup *deletions* n)))
              (if d
                  (begin
                    (if (not del)
                        (set! del (car d)))
                    (format #t "~a~s unlinked in:\n~a    ~s\n" 
                            (make-string depth #\space)
                            n
                            (make-string depth #\space)
                            del))
                  (format #t "~ano trace of ~s\n"
                          (make-string depth #\space)
                          n))))))
    ;;
    (foccur node 4)
    del))



(define (check-export-cycle)
  (with-output-to-file
      "/tmp/cycle.log"
    (lambda ()
      (handler-case
       (begin
         (check-exports)
         #f)
       ((<could-not-find-path> condition: e)
        (cond
         ((consideration e (current-output-port))
          => (lambda (c)
               (eval c)
               c))
         (else
          (signal e))))))))

(define (check-export-cycle* use-time?)
  (handler-case
   (begin
     (check-exports use-time?)
     #f)
   ((<could-not-find-path> condition: e)
    (cond
     ((consideration e (current-output-port))
      => (lambda (c)
           (eval c)
           c))
     (else
      (signal e))))))

(define (keep-checking #optional use-time?)
  (let loop ()
    (let* ((t0 (time))
           (c (check-export-cycle* use-time?))
           (t1 (time)))
      (if c
          (begin
            (format #t ">>> ~s ; ~a\n" c (time-time t1 t0))
	    (let ((f (open-output-append-file "migrate/more-disco.scm")))
              (write c f)
              (newline f)
              (close-output-port f))
            (loop))))))


;;;

(define (sorted-exports)
  (call-with-output-file
      "/tmp/exports.sort"
    (lambda (port)
      (in-topological-time-order
       (lambda (n j)
         (format port "~50a ; ~a\n" 
                 (~ "(~10d ~s)" j (name n))
                 (time->string (timestamp (audit n))
                               "%Y-%m-%d  %H:%M:%S"))
         (+ j 1))
       0))))

;;;

(define (check-exports-from-file)
  ;; like check-exports, but loads the order from /tmp/exports.sort
  ;; and doesn't output anything
  (reinit-simulation-tables)
  ;;
  (call-with-input-file
      "/tmp/exports.sort"
    (lambda (port)
      (let loop ()
        (let* ((tuple (read port))
               (n (table-lookup *export-name-map* (cadr tuple))))
          ((export-checker n))
          (loop))))))

(define (print-prereq-tree* at path vis depth)
  (let ((pre (make-string (* depth 2) #\space)))
    (if (null? path)
        (format #t "~a~s" pre at)
        (format #t "~a~d. ~s" pre (car path) at))
    (let ((z (table-lookup vis at)))
      (cond
       ((not z)
        (newline)
        (table-insert! vis at path)
        (for-each
         (lambda (p i)
           (print-prereq-tree* p (cons i path) vis (+ depth 1)))
         (prereqs at)
         (range (length (prereqs at)))))
       ((eq? z #t)
        (format #t " [ok]\n"))
       ((pair? z)
        (format #t "*\n"))
       (else
        (format #t "?\n"))))))

(define (print-prereq-tree at #optional tbl)
  (print-prereq-tree* at '() (if tbl
                                 (hash-table-copy tbl)
                                 (make-object-table))
                      0))

(define (flipper)
  (let ((flipt (make-object-table)))
    ;;
    (table-for-each
     *export-item-map*
     (lambda (h k v)
       (table-insert! flipt v k)))
    ;;
    (lambda (x)
      (table-lookup flipt x))))
  
(define (as-digraph)
  (let ((flip (flipper))
        (omit (make-object-table)))
    ;;
    (define (omit? k)
      (table-key-present? omit k))
    ;;
    (table-for-each
     *export-database*
     (lambda (h k v)
       (case (car (name v))
         ((user group)
          (table-insert! omit v #t)))))
    ;;
    ;;
    (call-with-output-file
        "/tmp/deps.dot"
      (lambda (port)
        (format port "digraph {\n")
        (format port "  size = \"8,10\" ;\n")
        (format port "  rotate = 90 ;\n")
        (format port "  rankdir = \"LR\" ;\n")
        (format port "  ranksep = 1.5 ;\n")
        (format port "  node [ fontname = \"Helvetica\", fontsize = 24 ] ;\n")
        (format port "\n")
        ;;
        (table-for-each
         *export-database*
         (lambda (h k v)
           (if (not (omit? v))
               (begin
                 (case (car (name v)) 
                   ((fs)
                    (format port "  ~d [shape=box];\n" k))
                   ((group)
                    (format port "  ~d [shape=triangle];\n" k))
                   ((node)
                    (if (or (instance? (flip v) <directory-version>)
                            (instance? (flip v) <directory>))
                        (format port "  ~d [peripheries=2];\n" k))
                    (if (and (pair? (cddr (name v)))
                             (string=? (caddr (name v)) "1.1"))
                        (format port "  ~d [style=filled];\n" k)
                        (if (null? (cddr (name v)))
                            (format port "  ~d [color=red];\n" k)))))
                 ;;
                 (for-each
                  (lambda (p)
                    (if (not (omit? p))
                        (format port "  ~d -> ~d ;\n" k (id p))))
                  (prereqs v))))))
        ;;
        (format port "}\n")))))

;;;

(define *current-name-map* (make-node-table))

(define (current-name-map)
  (let ((tbl (make-node-table)))
    ;;
    (define (scan path node)
      (table-insert! tbl node path)
      (if (instance? node <directory>)
          (for-each
           (lambda (sub)
             (scan (cons (car sub) path) (cdr sub)))
           (contents (current-version node)))))
    ;;
    (for-each
     (lambda (fs)
       (scan (list (string-append (name fs) "::")) (root-directory fs)))
     (value-sequence (file-system-table *application*)))
    ;;
    (set! *current-name-map* tbl)
    tbl))

(define-method fname ((self <node>))
  (cond
   ((table-lookup *current-name-map* self)
    => (lambda (p)
         (string-join "/" (reverse p))))
   (else
    "?")))

      

(define (exm index (p <directory-version>))
  (bind ((ln unlink rename (dir-changes (contents (previous-version p)) 
                                        (contents p))))
    (for-each
     (lambda (l)
       (table-insert! index 
                      (cdr l) 
                      (cons (cons p (car l))
                            (or (table-lookup index (cdr l))
                                '()))))
     ln)))

(define (everlink)
  (let ((index (make-node-table)))
    (table-for-each
     *master-node-id-index*
     (lambda (h k v)
       (if (instance? v <directory>)
           (for-each-version
            (versions v)
            (lambda (path vleaf)
              (let ((nv (value vleaf)))
                (if (previous-version nv)
                    (exm index nv))))))))
    index))

(define (X (k <fixnum>))
  (table-lookup *export-database* k))

(define (Contents (x <directory>))
  (get-current-contents x))

(define (Node (k <fixnum>))
  (table-lookup *master-node-id-index* k))

(define (ByName n)
  (table-lookup *export-name-map* n))

(define (dump-ordering-plan)
  (call-with-output-file
      "/tmp/iplan.dat"
    (lambda (port)
      ;;
      (format port "; past\n")
      (vector-for-each
       (lambda (x)
         (format port "~25s ; ~a\n" (name x) (or (description x) "")))
       (dequeue-state *internal/output*))
      ;;
      (format port "; ready\n")
      (for-each
       (lambda (x)
         (format port "~25s ; ~a\n" (name x) (or (description x) "")))
       (*internal/ready*))
      ;;
      (format port "; pending\n")
      (table-for-each
       *internal/pending*
       (lambda (h k v)
         (for-each
          (lambda (y)
            (let ((x (car y)))
              (format port "~25s ; ~a\n" (name x) (or (description x) ""))))
          v))))))

(define (flip-pending)
  (let ((pending (make-fixnum-table)))
    (table-for-each
     *internal/pending*
     (lambda (h k v)
       (for-each
        (lambda (p)
          (table-insert! pending 
                         (id (car p)) 
                         (cons (table-lookup *export-database* k)
                               (cdr p))))
        v)))
    pending))

(define (whynotrec (self <export-node>))
  (let ((ready (*internal/ready*))
        (pending (flip-pending))
        (c (make-dequeue)))
    ;;
    (define (conclude lst)
      (dequeue-push-back! c lst))
    ;;
    (define (deeper node rest)
      (for-each (lambda (p)
                  (chase p rest))
                (prereqs node)))
    ;;
    (define (chase node path)
      (format #t "~a~s" (make-string (* 2 (length path)) #\space) node)
      (let ((r (cons node path)))
        (if (memq node ready)
            (begin
              (format #t "  C\n")
              (conclude (reverse r)))
            (if (table-lookup pending (id node))
                (begin
                  (format #t "  +\n")
                  (deeper node r))
                (begin
                  (format #t "  k\n")
                  (assert (table-lookup *internal/past* (id node))))))))
    ;;
    (deeper self (list self))
    ;;
    (vector-for-each
     (lambda (path)
       (format #t "======> path length ~d <======\n" (length path))
       (print path))
     (dequeue-state c))
    c))
       
(define (why-not? (self <export-node>))
  (format #t "------------------------------------\n")
  (format #t "  ~s\n" self)
  (if (description self)
      (format #t "  Desc: ~a\n" (description self)))
  (format #t "\n")
  (format #t "prereqs (~d total):\n" (length (prereqs self)))
  (let ((ready (*internal/ready*))
        (pending (flip-pending)))
    ;;
    (for-each
     (lambda (i p)
       (format #t "  ~d. ~40s ~a\n"
               i
               p
               (cond
                ((table-lookup *internal/past* (id p)) "done with")
                ((memq p ready) "ready")
                ((table-lookup pending (id p))
                 => (lambda (l)
                      (~ "pending on ~d others" (length l)))))))
     (range (length (prereqs self)))
     (prereqs self)))
  ;;
  (if (preflight self)
      (begin
        (format #t "preflight using ~s\n" (preflight self))
        (set! *internal/unlink-preflight-verbose?* #t)
        (set! *internal/unlink-preflight-failures* #f)
        (let* ((x ((preflight self) *internal/past*))
               (f *internal/unlink-preflight-failures*))
          (if f
              (begin
                (newline)
                (for-each
                 (lambda (i x)
                   (format #t "  ~d. ~s" i x)
                   (cond
                    ((table-lookup *export-item-map* x)
                     => (lambda (n)
                          (format #t " => ~s" n))))
                   (newline))
                 (range (dequeue-count f))
                 (vector->list (dequeue-state f)))
                (newline)))
          (if x
              (format #t "** preflight passed\n")
              (format #t "** PREFLIGHT FAILED\n")))))
  ;;
  (format #t "------------------------------------\n")
  (values))
  
