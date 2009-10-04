
(define (collect-applicable-filesets (self <node-version>))
  (map file-system (select (lambda (i)
                             (instance? i <fs-change>))
                           (change-items self))))
  
(define (identify-fs-path-or-punt node (rel <node-version>))
  (bind ((behalf (collect-applicable-filesets rel))
         (fs path (handler-case
                   (identify-fs-path-to node behalf)
                   ((<could-not-find-path>)
                    (if (pair? behalf)
                        (handler-case
                         (identify-fs-path-to node #f)
                         ((<could-not-find-path>)
                          (values)))
                        (values))))))
    (if fs
        (values (name fs) (string-join #\/ path))
        (values "?" (~ "?#~d" (id node))))))

(define-method node-script* ((self <user>))
  (psx 'mkuser
       (name self)
       (full-name self)
       (email-addr self)))
       

(define-method node-script* ((self <group>))
  (if (pair? (parent-groups self))
      (psx 'mkdom
           (name self)
           (name (car (parent-groups self)))
           (name (owner self))
           (~ "Group ~s from SourceBase" (name self)))))

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

(define-method node-script* ((self <change-request>))
  (let ((h (last (history self))))
    (psx 'open
         (to-string (id self))
         (name (group self))
         (title self)
         (cr-plugins self)
         (if (instance? h <comment-request>)
             (comment h)
             "")
         user: (name (originator self)))))

(define-method node-script* ((self <file-version>))
  (let* ((pred (previous-version self))
         (node (versioned-object self))
         (fli (table-lookup *first-linked-in* node))
         (preq (exported-item (or pred node))))
    ;;
    (bind ((a (node-version-audit self))
           (fs path (identify-fs-path-or-punt 
                     (if pred
                         (versioned-object self)
                         (versioned-object (cdr fli)))
                     self))
           (path (if pred
                     path
                     (if (string=? path "")
                         (car fli)
                         (string-append path "/" (car fli)))))
           (ver (to-string (version-tag self)))
           (data (delay (content->string (contents self)))))
      ;;
      (if pred
          (psx 'edit
               fs
               path
               ver
               (reason-id-list (change-items self))
               (comment self)
               (number->string (permissions self) 8)
               (list (string-length (force data))
                     (base64-gzip-encode (force data))))
          (psx 'creat
               fs
               path
               (name (group (versioned-object self)))
               (reason-id-list (change-items self))
               (comment self)
               (number->string (permissions self) 8)
               (list (string-length (force data))
                     (base64-gzip-encode (force data))))))))
               
(define-method node-script* ((self <file-system>))
  (psx 'mkfs
       (name self)
       (name (group self))
       user: (name (owner self))))

(define-method node-script* ((self <work-request>))
  (let ((ns (table-lookup *new-state-map* self))
        (active? (and (memq self (active-items (base-request self))) #t)))
    ;;
    (define (any-fs-changes?)
      (find-first (lambda (wi)
                    (instance? wi <fs-change>))
                  (append (active-items 
                           (base-request self))
                          (history 
                           (base-request self)))))
    ;;
    (if (and (not active?)                      ; still pending
             (not (eq? (state self) ns)))       ; null transition
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
                    (psx 'chstate subj "complete"))
                (psx 'chstate subj (to-string s))))))))
        

(define-method node-script* ((self <integration-request>))
  (values))

(define-method node-script* ((self <property-add>))
  (psx 'addprop
       (to-string (id (base-request self)))
       (name (the-property self))
       (name (new-value self))))

(define-method node-script* ((self <property-change>))
  (psx 'chprop
       (to-string (id (base-request self)))
       (name (the-property self))
       (name (new-value self))))

(define-method node-script* ((self <title-change>))
  (psx 'chtitle
       (to-string (id (base-request self)))
       (new-title self)))

(define-method node-script* ((self <fs-change>))
  (let ((active? (and (memq self (active-items (base-request self))) #t)))
    (if (not active?)
        (psx 'done
             (to-string (id (base-request self)))
             (name (file-system self))
             (name (owner self))))))

(define-method node-script* ((self <file>))
  (values)#|XXX|#)

(define-method node-script* ((self <directory>))
  (values)#|XXX|#)

(define-method node-script* ((self <comment-request>))
  (let ((subj (to-string (id (base-request self)))))
    ;; if we are the last work item for this cr, then we
    ;; would have been handled by the 'open' handler
    (if (not (eq? self (last (history (base-request self)))))
        (psx 'note subj (trim-whitespace (comment self))))))

  
(define-method node-script* ((self <code-review>))
  (let* ((subj (to-string (id (base-request self))))
         (open-msg (~ "Please review changes in '~a'" (name (group self))))
         (close-msg (~ "Changes to '~a' reviewed" (name (group self)))))
    ;;
    (psx 'rfc
         subj
         (name (owner self))
         open-msg
         user: (audit-user (activate-audit-entry self))
         time: (timestamp (activate-audit-entry self)))
    ;;
    (if (close-audit-entry self)
        (psx 'rfca
             subj
             (name (owner self))
             close-msg
             time: (timestamp (close-audit-entry self))
             user: (audit-user (close-audit-entry self))))))
          


(define (node-script*/subseq (self <directory-version>) pred node preq)
  ;; There is a previous version, so there are just
  ;; some changes being made...
  (bind ((ln unlink rename (dir-changes (contents pred) 
                                        (contents self)))
         (link-old (select (lambda (l)
                             (not (eq? (cdr (table-lookup *first-linked-in*
                                                          (cdr l)))
                                       self)))
                           ln))
         (fs path (identify-fs-path-or-punt (versioned-object self) self)))
    ;;
    (define (path+ suffix)
      (if (string=? path "")
          suffix
          (string-append path "/" suffix)))
    ;;
    (for-each
     (lambda (link1)
       (bind ((targ (cdr link1))
              (name (car link1))
              (tfs tpath (identify-fs-path-or-punt targ self)))
         (psx 'ln
              fs
              (path+ name)
              tfs
              tpath
              (reason-id-list (change-items self))
              (or (comment self) ""))))
     link-old)
    ;;
    (for-each
     (lambda (unlink1)
       (bind ((targ (cdr unlink1))
              (name (car unlink1)))
         (psx (if (instance? targ <directory>)
                  'rmdir
                  'rm)
              fs
              (path+ name)
              (reason-id-list (change-items self))
              (or (comment self) ""))))
     unlink)
    ;;
    (for-each
     (lambda (rename1)
       (let ((targ (caddr rename1))
             (old-name (car rename1))
             (new-name (cadr rename1)))
         (psx 'mv
              fs
              (path+ old-name)
              new-name
              (reason-id-list (change-items self))
              (or (comment self) ""))))
     rename)
    ;;
    (values)))

;;;

(define (node-script*/first (self <directory-version>) node preq fli)
  (bind ((fs path (identify-fs-path-or-punt (versioned-object (cdr fli))
                                            self))
         (path (if (string=? path "")
                   (car fli)
                   (string-append path "/" (car fli)))))
    (psx 'mkdir
         fs
         path
         (name (group (versioned-object self)))
         (reason-id-list (change-items self))
         (number->string (permissions self) 8)
         (or (comment self) ""))))

(define-method node-script* ((self <directory-version>))
  (let* ((pred (previous-version self))
         (node (versioned-object self))
         (preq (exported-item (or pred node))))
    ;;
    (cond
     ;;
     (pred
      (node-script*/subseq self pred node preq))
     ;;
     ((table-lookup *first-linked-in* node)
      => (lambda (fli)
           (node-script*/first self node preq fli))))))

;;;

(define (short-script->xaction-file source dest #key 
                                    (first default: #f)
                                    (last default: #f))
  (call-with-input-file
      source
    (lambda (sport)
      (call-with-output-file
          dest
        (lambda (dport)
          ;;
          (define (do key line l)
            (format dport "\n<!-- ~d: [~a] -->\n" line key)
            (let ((x (line->xaction l)))
              (write-sxml (pretty-printify-xml x) dport)
              (newline dport)
              (flush-output-port dport)))
          ;;
          (let ((include? (not first)))
            (let loop ((line 1))
              (let ((l (read-line sport)))
                (if (string? l)
                    (let ((key (substring l 1 8)))
                      (if (and first (string=? key first))
                          (set! include? #t))
                      (if include?
                          (do key line (substring l 1)))
                      (if (and last (string=? key last))
                          (values)
                          (loop (+ line 1)))))))))))))

;;;

(define (lookup-saved-or-fake key fields)
  (let ((s (table-lookup *short-data-backing* key)))
    (if s
        (let ((expected (saved-short-entry->all-fields s)))
          (if (not (equal? fields expected))
              (format #t "~a: fields changed : ~s\n"
                      key
                      (select identity
                              (map
                               (lambda (i f x)
                                 (if (string=? f x)
                                     #f
                                     (+ i 1)))
                               (range (min (length fields) (length expected)))
                               fields
                               expected))))
          s)
      (begin
        (format #t "*** Could not find saved entry ~s\n" key)
        (list #f #f #f #f
              (string->symbol (list-ref fields 3))
              (list-tail fields 4))))))

(define (line->xaction line)
  (let* ((fields (string-split line #\|))
         (key (car fields))
         (saved (lookup-saved-or-fake key fields)))
    ;;
    (let* ((user (list-ref fields 1))
           (time (string->time (list-ref fields 2)))
           (body (apply
                 ;;
                 (case (list-ref saved 4)
                   ;;------------------------------
                   ;; misc...
                   ((mkuser) line->xaction/mkuser)
                   ((mkfs) line->xaction/mkfs)
                   ((mkdom) line->xaction/mkdom)
                   
                   ;; cr ops
                   ((open) line->xaction/open)
                   ((note) line->xaction/note)
                   ((rfc) line->xaction/rfc)
                   ((rfca) line->xaction/rfca)
                   ((done) line->xaction/done)
                   ((addprop) line->xaction/addprop)
                   ((chprop) line->xaction/chprop)
                   ((chstate) line->xaction/chstate)
                   ((chtitle) line->xaction/chtitle)

                   ;; file ops
                   ((creat) line->xaction/creat)
                   ((edit) line->xaction/edit)
                   ((ln) line->xaction/ln)
                   ((mkdir) line->xaction/mkdir)
                   ((mv) line->xaction/mv)
                   ((rm) line->xaction/rm)
                   ((rmdir) line->xaction/rmdir)
                   ;;------------------------------
                   (else 
                    (error "unsupported xaction type: ~s" (list-ref saved 4))))
                 ;;
                 user
                 time
                 (import-args 
                  (list-ref saved 5) 
                  (list-tail fields 4)))))
      ;;
      `(axis-transaction
        (who (@ (name ,user)))
        (how (@ (type "direct")))
        (when (@ (unix ,(~ "~.0f" (time->epoch-seconds time))))
              ,(timestr time))
        (what ,@body)))))

(define (import-args saved fields)
  (if (null? saved)
      '()
      ;; "hidden" args are always supplied and don't eat a field
      (if (pair? (car saved))
          (cons (if (null? (cdar saved))
                    (caar saved)
                    (car saved))
                (import-args (cdr saved) fields))
          (let (((nf <string>) (car fields)))
            (if (string=? nf "")
                (cons "" (import-args (cdr saved) (cdr fields)))
                ;; if the field has a "+" at the end, take the saved value
                (if (char=? (string-ref nf (- (string-length nf) 1)) #\+)
                    (cons (car saved) (import-args (cdr saved) (cdr fields)))
                    ;; otherwise, take the given value
                    (cons nf (import-args (cdr saved) (cdr fields)))))))))

(define (line->xaction/note user time subj comment)
  `((axis:add-comment
     (owner ,user)
     (subject ,subj)
     (comment ,comment))))

(define (line->xaction/mkuser user time name full email)
  `((axis:create-user
     (name ,name)
     (full-name ,full)
     (email-addr ,email)
     ;; everybody is in "world" because SB
     ;; has no concept of a user's group
     (domain "world"))
    ;; automatically give everyone direct access, because we
    ;; are going to execute everything on their behalf using
    ;; that access method
    (axis:create-direct-access
     (subject ,name))
    ;; also, make sure everyone has some serious authority, 
    ;; because it may be needed as well -- since SB never did
    ;; really implement authority, we have to assume everyone
    ;; has high auth.
    (axis:add-user-authority
     (subject ,name)
     (domain "world")
     (authority "domain-lead"))))

(define (line->xaction/mkdom user time 
                             domain-name parent-domain-name owner desc)
  `((axis:create-domain
     (name ,domain-name)
     (description ,desc)
     (in ,parent-domain-name)
     (owner ,owner))))
                    
(define (line->xaction/chstate user time cr state)
  `((axis:modify-change-request
     (subject ,cr)
     (state ,state))))

(define (line->xaction/chtitle user time cr title)
  `((axis:modify-change-request
     (subject ,cr)
     (title ,title))))

(define (line->xaction/addprop user time cr key value)
  `((axis:modify-change-request
     (subject ,cr)
     (plugin-add-attr
      ,(plugin-pair->xml (string->symbol key) value)))))

(define (line->xaction/chprop user time cr key value)
  `((axis:modify-change-request
     (subject ,cr)
     (plugin-modify-attr
      ,(plugin-pair->xml (string->symbol key) value)))))

(define (why->xreasons why)
  (if (string=? why "")
      '()
      `((reasons
         ,@(map (lambda (i)
                  `(item ,i))
                (string-split why #\space))))))

(define (path->xsteps path)
  (map (lambda (step)
         `(item ,step))
       (string-split path #\/)))

(define (line->xaction/mkdir user time fs path domain why mode comment)
  `((axis:create-directory
     (in (fileset ,fs) (snap "HEAD"))
     (path ,@(path->xsteps path))
     (domain ,domain)
     (mode ,(to-string (string->number mode 8)))
     ,@(why->xreasons why)
     ,@(if (string=? comment "")
           '()
           `((comment ,comment))))))


(define (line->xaction/creat user time fs path domain why comment mode zdata)
  (let ((loc `((in (fileset ,fs) (snap "HEAD"))
               (path ,@(path->xsteps path)))))
    `((axis:create-file (domain ,domain) ,@loc)
      ;;
      (axis:upload-file-content 
       ,@loc  
       (version "1.1")
       (size ,(to-string (car zdata))))
      ;;
      (axis:retire-lock
       ,@loc
       (version "1.1")
       (comment ,comment)
       ,@(why->xreasons why)
       (mode ,(to-string (string->number mode 8)))
       (evidence 
        (body (@ (enc "base64/gzip")
                 (mode "incore")) 
              ,(cadr zdata)))))))

(define (line->xaction/edit user time fs path ver why comment mode zdata)
  (let ((loc `((in (fileset ,fs) (snap "HEAD"))
               (path ,@(path->xsteps path)))))
    `((axis:lock-file 
       ,@loc
       (new-version ,ver))
      ;;
      (axis:upload-file-content 
       ,@loc  
       (version ,ver)
       (size ,(to-string (car zdata))))
      ;;
      (axis:retire-lock
       ,@loc
       (version ,ver)
       (comment ,comment)
       ,@(why->xreasons why)
       (mode ,(to-string (string->number mode 8)))
       (evidence 
        (body (@ (enc "base64/gzip")
                 (mode "incore")) 
              ,(cadr zdata)))))))

(define (line->xaction/rm user time fs path why comment)
  `((axis:delete-file
     (in (fileset ,fs) (snap "HEAD"))
     (path ,@(path->xsteps path))
     ,@(why->xreasons why)
     ,@(if (string=? comment "")
           '()
           `((comment ,comment))))))

(define (line->xaction/rmdir user time  fs path why comment)
  `((axis:delete-directory
     (in (fileset ,fs) (snap "HEAD"))
     (path ,@(path->xsteps path))
     ,@(why->xreasons why)
     ,@(if (string=? comment "")
           '()
           `((comment ,comment))))))

(define (line->xaction/done user time cr fs owner)
  `((axis:complete-fileset-changes
     (subject ,cr)
     (in (fileset ,fs) (snap "HEAD"))
     (owner ,owner))))

(define (line->xaction/mkfs user time fs domain)
  `((axis:create-fileset
     (name ,fs)
     (domain ,domain)
     (owner ,user)
     ;; we have to create the FS as uncontrolled
     ;; because we can't tell when, if ever, it
     ;; transitions to CONTROLLED state :-(
     (type "uncontrolled"))))

(define (line->xaction/ln user time fs path src-fs src-path why comment)
  (if (string-search src-path #\?)
      (error "ln: fs ~s path ~s not yet determined" src-fs src-path))
  ;;
  `((axis:link-node
     (source (fileset ,src-fs) (snap "HEAD"))
     (source-path ,@(path->xsteps src-path))
     ;;
     (dest (fileset ,fs) (snap "HEAD"))
     (dest-path ,@(path->xsteps path))
     ;;
     ,@(if (string=? comment "")
           '()
           `((comment ,comment)))
     ,@(why->xreasons why))))

(define (line->xaction/mv user time fs path new-name why comment)
  (if (string-search path #\?)
      (error "mv: fs ~s path ~s not yet determined" fs path))
  (if (string-search new-name #\?)
      (error "mv: fs ~s path ~s new name ~s not yet determined" 
             fs path new-name))
  ;;
  `((axis:rename-node
     (in (fileset ,fs) (snap "HEAD"))
     (path ,@(path->xsteps path))
     (new-path ,@(path->xsteps
                  (with-module
                      paths
                    (pathname->os-path
                     (append-path
                      (file-directory (string->file path))
                      (string->file new-name))))))
     ,@(if (string=? comment "")
           '()
           `((comment ,comment)))
     ,@(why->xreasons why))))
     
     

(define (line->xaction/open user time cr domain title plugins comment)
  (let ((pivs (if (string=? plugins "")
                  '()
                  (map (lambda (x)
                         (let ((eq (string-search x #\=)))
                           (cons (string->symbol (substring x 0 eq))
                                 (substring x (+ eq 1)))))
                       (string-split plugins #\;)))))
  `((axis:create-change-request
     (name ,cr)
     (title ,title)
     ,@(if (string=? comment "")
           '()
           `((comment ,comment)))
     (domain ,domain)
     (originator ,user)
     ;; --- plugin values ---
     ,@(if (null? pivs)
           '()
           `((plugin ,@(build-plugin-xml-list pivs))))))))

#|
(define (all-properties)
  (apply append
         (map properties
              (value-sequence (change-request-table *application* )))))
|#

(define (plugin-pair->xml k v)
  (case k
    ((severity)
     `(item (@ (key "com.xynthesis.crups.severity"))
            (value (@ (enum ,v)))))
    ((answer)
     `(item (@ (key "org.rscheme.misc.answer"))
            (value (@ (enum ,(string-join
                              #\- 
                              (string-split v #\_)))))))
    ((prefix)
     `(item (@ (key "org.rscheme.misc.type"))
            (value (@ (enum ,v)))))
    ((reference)
     `(item (@ (key "org.rscheme.misc.reference"))
            (value ,v)))
    (else
     (error "don't know how to handle plugin: ~s" k))))
  
(define (build-plugin-xml-list pivs)
  (map (lambda (piv)
         (plugin-pair->xml (car piv) (cdr piv)))
       pivs))

(define (line->xaction/rfc user time cr owner msg)
  `((axis:request-comment
     (subject ,cr)
     (owner ,owner)
     (comment ,msg))))

(define (line->xaction/rfca user time cr owner msg)
  `((axis:add-comment
     (subject ,cr)
     (owner ,owner)
     (comment ,msg))))

(define (short-backup key)
  (let* ((uid (cadr (table-lookup *short-data-backing* key)))
         (fl (string-split uid #\+))
         (page (string->number (car fl) 16))
         (offset (string->number (cadr fl) 16)))
    (persistent->transient
     (parts->persistent *pstore* page #b101 offset))))
    



#|

(define nvlist
  (let ((q (make-dequeue)))
    (table-for-each
     *master-node-index*
     (lambda (h k v)
       (for-each-version
        (versions v)
        (lambda (path vleaf)
          (dequeue-push-back! q (value vleaf))))))
    (vector->list (dequeue-state q))))

(select (lambda (a) (> (length (change-items a)) 1)) nvlist )
|#
