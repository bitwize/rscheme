
;;
;;  <work-item> objects are associated with users
;;  and change requests, and record the queue of work 
;;  for the user
;;
;;  they are also the atomic units of work, and are
;;  immutable in owner
;;
;;  when a change request's list of active work items
;;  becomes empty, it goes into its next state
;;  

(define-class <work-item> (<object>) :abstract
  (owner type: <user>)
  (base-request type: <change-request>)
  (activate-audit-entry init-value: #f)
  (close-audit-entry init-value: #f))

;; these methods allow us to use `cr' and `title' in a --format clause

(define-method cr ((self <work-item>))
  (base-request self))

(define-method title ((self <work-item>))
  (title (base-request self)))

(define-method open-time ((self <work-item>))
  (open-time (base-request self)))

;;
;;  a code review is posted to a group lead when a
;;  change is made to code owned by the group, if the
;;  group requests code review for filesystem
;;

(define-class <code-review> (<work-item>)
  (group type: <group>))

;;
;;  a work request is posted to a user when a change
;;  request is pending new examination, e.g., when the
;;  previous set of work items have been resolved and
;;  a new phase is required
;;
;;  the target is the <change-request>
;;

(define-class <work-request> (<work-item>)
  ;; `state' is the state which the system is requesting work
  ;; to move out of.  e.g., the initial <work-request> has a 
  ;; state of `open'
  state)

(define-class <fs-change> (<work-item>)
  (file-system type: <file-system>)
  (new-versions type: <list> init-value: '()))

(define-method fs-change-items ((self <node-version>))
  (select (lambda (ci)
            (instance? ci <fs-change>))
          (change-items self)))

(define-method audit-change-item ((self <node-version>))
  (let loop ((i (change-items self)))
    (if (null? i)
        #f
        (if (instance? (car i) <audit-log-entry>)
            (car i)
            (loop (cdr i))))))

(define-method write-object ((self <work-request>) port)
  (format port "#[<work-request> ~d, ~a, ~a]" 
          (id (base-request self))
          (name (owner self))
          (state self)))
  
(define-method write-object ((self <fs-change>) port)
  (format port "#[<fs-change> ~d, ~a, ~a]" 
          (id (base-request self))
          (name (owner self))
          (name (file-system self))))

;;
;;  a <comment-request> can be used to solicit a comment,
;;  or simply to attach a comment (in which case it is closed
;;  at the same time it is opened)

(define-class <comment-request> (<work-item>)
  (comment init-value: #f))

(define-class <title-change> (<work-item>)
  (old-title init-value: #f)
  (new-title init-value: #f))

(define-class <summary-change> (<work-item>)
  (old-summary init-value: #f)
  (new-summary init-value: #f))

(define-class <property-change> (<work-item>)
  (the-property init-value: #f)
  (old-value init-value: #f)
  (new-value init-value: #f))

(define-class <property-add> (<work-item>)
  (the-property init-value: #f)
  (new-value init-value: #f))

(define-class <integration-request> (<work-item>)
  (file-system type: <file-system>)
  (snapshots type: <list> init-value: '()))

;; the method allows us to use `snapshot' in a --format clause

(define-method snapshot ((self <integration-request>))
  (string-join #\space (map name (snapshots self))))
