;;;

(define *application-version* "0.1")

(define-class <information-base> (<application-root>)
  (global-item-index init-function: make-string-table)
  (root-issue)
  (app-version))

(define-class <ibase-transaction> (<transaction>))

(define (current-information-base)
  (current-application-root-object))

;;;

(define (string->priviledge-group str)
  (table-lookup (priviledge-index (current-information-base)) str))

(define (string->domain str)
  (table-lookup (domain-index (current-information-base)) str))

(define (string->user str)
  (table-lookup (user-index (current-information-base)) str))

;;;

(define (with-information-base (ibase <information-base>) thunk)
  (with-transient-system-state ibase thunk))

(define (with-transaction-for-user (ibase <information-base>)
                                   (user <string>)
                                   thunk)
  (with-information-base
   ibase
   (lambda ()
     (with-transaction
      (make <ibase-transaction>
        user: (or (table-lookup (user-index ibase) user)
                  (error "User ~s not defined in information base" user)))
      thunk))))

;;;

(define-class <component> (<domain>)
  (owner type: <user>)
  (description type: <string>))

(define-class <item> (<object>) :abstract               ; inherit from p-object
  (id type: <fixnum> init-value: -1)
  (parent init-value: #f)
  (next-sub-id type: <fixnum> init-value: 1)
  (properties init-value: '#())
  (history init-value: '())
  (create-xaction init-function: current-transaction)
  (modify-xaction init-function: current-transaction)
  (delete-xaction init-value: #f)
  (originator init-function: current-user)              ; may get changed
  (owner init-function: current-user)
  (content type: <string>)
  (title type: <string>)
  (relations init-value: '#()))

(define-method component ((self <item>))
  (root-domain (current-information-base)))             ; XXX not really...

(define (item-not-deleted? (self <item>))
  (not (delete-xaction self)))

(define-method write-object ((self <item>) port)
  (format port "#[~a ~a /~04x~04x]" 
          (name (object-class self))
          (global-id self)
          (obj-high-bits self)
          (obj-low-bits self)))

(define (alloc-next-sub-id! self)
  (let ((n (next-sub-id self)))
    (set-next-sub-id! self (+ n 1))
    n))

(define-method assign-id! ((self <item>))
  (set-id! self (if (parent self)
                    (alloc-next-sub-id! (parent self))
                    (alloc-item-id))))

(define-method reset-parent! ((self <item>) new-parent)
  (if (eq? (parent self) new-parent)
      (values)
      (let ((ix (global-item-index (current-information-base)))
            (id0 (global-id self)))
        (table-remove! ix id0)
        (set-parent! self new-parent)
        (assign-id! self)
        (table-insert! ix (global-id self) self)
        (dm 9101 "Global id of a ~s changed from ~s => ~s" 
            (name (object-class self))
            id0
            (global-id self)))))

(define-method initialize ((self <item>))
  (if (< (id self) 0)
      (assign-id! self))
  (table-insert! (global-item-index (current-information-base))
                 (global-id self)
                 self))

(define-class <note> (<item>))
(define-class <issue> (<item>))
(define-class <theme> (<issue>))
(define-class <position> (<item>))
(define-class <argument> (<item>) :abstract)
(define-class <affirmative> (<argument>))
(define-class <negative> (<argument>))
(define-class <audit-note> (<note>)
  (audit-entries init-value: '()))

(define (issue? x) (instance? x <issue>))

(define *item-class-list* (list (cons <note> 'note)
                                (cons <issue> 'issue)
                                (cons <theme> 'theme)
                                (cons <position> 'position)
                                (cons <argument> 'argument)))

(define (item-class->type-spec c)
  (cdr (assq c *item-class-list*)))

(define (item-type-spec->class ts)
  (case ts
    ((item) <item>)
    ((note) <note>)
    ((issue) <issue>)
    ((theme) <theme>)
    ((position) <position>)
    ((argument) <argument>)
    (else (error "Unknown item type spec `~s'" ts))))
  

;;;

(define-class <index-entry> (<object>)
  item-set)

(define-class <index> (<object>)
  word-index)

;;;

(define *default-priviledge-groups*
  ;; first one must be administrator
  '(("administrator" login read write manage)
    ("viewer" login read)
    ("author" login read write)))

(define (make-information-base #key admin-user)
  (with-information-base
   (make <information-base>
         app-version: *application-version*
         root-domain: #f
         root-issue: #f)
   (lambda ()
     (set-property! (current-information-base) 'version $current-model-version)
     (let* ((root (make <user> 
                        name: admin-user
                        fullname: "Infobase Administrator"
                        super-user?: #t))
            (universe (make <component>
                            name: "universe"
                            description: "The universe of discourse"
                            owner: root))
            (ib (current-information-base))
            (admin #f))
       (table-insert! (user-index ib) (name root) root)
       (table-insert! (domain-index ib) (name universe) universe)
       ;;
       (for-each (lambda (ent)
                   (let ((pg (make <priviledge-group>
                                   name: (car ent)
                                   priviledges: (list->vector (cdr ent)))))
                     (table-insert! (priviledge-index ib)
                                    (name pg)
                                    pg)
                     (if (not admin) (set! admin pg))))
                 *default-priviledge-groups*)
       ;;
       (set-root-domain! ib universe)
       ;;
       (with-transaction
        (make <ibase-transaction>
              user: root)
        (lambda ()
          ;; let the first user connect from localhost
          (add-access-record admin-user admin-user "127.0.0.1")
          ;; grant the first user admin access, so they don't have
          ;; to invoke superuser authority right away
          (set-authority-vector! root
                                 (vector
                                  (make <authority>
                                        owner: admin-user
                                        with-respect-to: universe
                                        priviledge-group: admin
                                        access-type: 1)))
          (set-root-issue! 
           (current-information-base)
           (make <theme>
                 content: "What is the answer to all our questions?"
                 title: "What is the answer?"))))
       ;;
       (current-information-base)))))

(define (add-access-record ownern user host #optional (options default: '()))
  (let ((rec (make <access>
                   owner: (string->user ownern)
                   user: user
                   host: host
                   options: (list->vector options))))
    (set-access-host-list! (owner rec)
                           (cons rec (access-host-list (owner rec))))
    rec))
                  

(define-relation responds-to ((response <position>)
                              (issue <issue>))
  ((issue)))

(define-relation supports ((argument <argument>)
                           (position <position>))
  ((argument)))

(define-relation objects-to ((argument <argument>)
                             (position <position>))
  ((argument)))

(define-relation specializes ((fine <issue>)
                              (coarse <issue>))
  ((fine)))

(define-relation questions ((issue <issue>)
                            (item <item>))
  ((issue item)))

(define-relation discusses ((note <note>)
                            (item <item>))
  ((note item)))

;; (responds-to position issue)         ; position is a response to issue
;; (questions issue item)               ; issue is raised by item
;; (supports argument position)         ; argument supports position
;; (objects-to argument position)       ; argument objects to position
;; (specializes fine coarse)            ; fine is more detailed than coarse
;; (discusses note item)                ; note is about item
;;
;; (generalizes issue-1 issue-2)        ; issue-1 is more general than issue-2
;; (replaces item-1 item-2)             ; item-1 replaces item-2
;; refers-to 

;;;


;;;

(define-method global-id ((self <argument>))
  (format #f "~d.~d.~d" 
          (id (parent (parent self)))
          (id (parent self))
          (id self)))

(define-method global-id ((self <position>))
  (format #f "~d.~d" 
          (id (parent self))
          (id self)))

(define-method global-id ((self <note>))
  (format #f "~a/~d"
          (global-id (parent self))
          (id self)))

(define-method global-id ((self <issue>))
  (number->string (id self)))

(define (display-id self)
  (get-property self 'tag (global-id self)))

;;;-----------------------------------------------------------------------
;;;
;;;   Item Hierarchy Traversal
;;;   ------------------------
;;;
;;;   Notes are not considered child-items directly; use the
;;;   `notes' method to traverse the notes tree attached to an item
;;;
;;;   child-items are just those in the Issue -> Position -> Argument
;;;   chain
;;;
;;;   sub-issues are Issues associated with any given item
;;;   (which are specializations of Issues, or questioning other Items)
;;;
;;;   Hence, the complete tree is traversed using the transitive
;;;   closure of (child-items | sub-issue) from the root issue
;;;   (with notes off of each resulting item)

(define-method child-items ((self <item>))
  '())

(define-method child-items ((self <issue>))
  (select item-not-deleted?
          (query-relation-pick 'responds-to 'response issue: self)))

(define-method child-items ((self <position>))
  (append (select item-not-deleted?
                  (query-relation-pick 'supports 'argument position: self))
          (select item-not-deleted?
                  (query-relation-pick 'objects-to 'argument position: self))))

;;;

(define-method link-child ((self <item>) child)
  (service-error 703 "Cannot link child to ~s" self))

(define-method link-child ((self <issue>) (child <position>))
  (establish-relation 'responds-to child self)
  (reset-parent! child self)
  (values))

(define-method link-child ((self <position>) (child <argument>))
  (if (instance? child <affirmative>)
      (establish-relation 'supports child self)
      (establish-relation 'objects-to child self))
  (reset-parent! child self)
  (values))

(define-method link-sub-issue ((self <item>) (sub <issue>))
  (establish-relation 'questions sub self))

(define-method link-sub-issue ((self <issue>) (sub <issue>))
  (establish-relation 'specializes sub self))

;;;

(define-method unlink-child ((self <issue>) item)
  (service-error 703 "Cannot unlink child from ~s" self))

(define-method unlink-child ((self <issue>) (item <position>))
  ; note that we don't clear `parent'; that will happen when
  ; we get relinked.  THIS IS IMPORTANT, BECAUSE RELINKING
  ; REQUIRES KNOWING THE GLOBAL-ID, WHICH DEPENDS ON THE PARENT!
  ;(set-parent! item #f)
  (relation-delete 'responds-to item self))

(define-method unlink-child ((self <position>) (item <argument>))
  ; note that we don't clear `parent'; that will happen when
  ; we get relinked.  THIS IS IMPORTANT, BECAUSE RELINKING
  ; REQUIRES KNOWING THE GLOBAL-ID, WHICH DEPENDS ON THE PARENT!
  ;(set-parent! item #f)
  (if (instance? item <affirmative>)
      (relation-delete 'supports item self)
      (relation-delete 'objects-to item self)))

(define-method unlink-sub-issue ((self <item>) (item <issue>))
  (relation-delete 'specializes item self))

(define-method unlink-sub-issue ((self <issue>) (item <issue>))
  (relation-delete 'questions self item))
  
;;;

(define-method parent-items ((self <item>))
  '())

(define-method parent-items ((self <issue>))
  (append (query-relation-pick 'questions 'item issue: self)
          (query-relation-pick 'specializes 'coarse fine: self)))

(define-method parent-items ((self <position>))
  (query-relation-pick 'responds-to 'issue response: self))

(define-method parent-items ((self <argument>))
  (append (query-relation-pick 'supports 'position argument: self)
          (query-relation-pick 'objects-to 'position argument: self)))

;;;

(define-method sub-issues ((self <item>))
  (select item-not-deleted? 
          (query-relation-pick 'questions 'issue item: self)))

(define-method sub-issues ((self <issue>))
  (select item-not-deleted? 
          (query-relation-pick 'specializes 'fine coarse: self)))

;;;

(define-method notes ((self <item>))
  (query-relation-pick 'discusses 'note item: self))

;;;-----------------------------------------------------------------------

(define-method lookup-item ((tag <string>))
  (table-lookup (global-item-index (current-information-base)) tag))

(define-method lookup-item ((self <item>))
  self)

;;;
