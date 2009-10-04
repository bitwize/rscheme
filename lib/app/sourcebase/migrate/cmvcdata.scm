
(define-class CMVCObject (<object>)
    forward)

(define-class Access (CMVCObject)
    component-id
    user-id
    authority-name
    authority-type)

(define-class Approval (CMVCObject)
    track-id
    user-id
    state
    add-date
    last-update)

(define-class Approver (CMVCObject)
    release-id
    user-id)

(define-class Authority (CMVCObject)
    name
    action)

(define-class Cfgcomproc (CMVCObject)
    name
    config)

(define-class Cfgrelproc (CMVCObject)
    name
    config)

(define-class Change (CMVCObject)
    path-id
    track-id
    file-id
    version-id
    level-id
    type
    user-id)

(define-class CompMember (CMVCObject)
    parent-id
    child-id
    rank
    reference)

(define-class Component (CMVCObject)
    id
    name
    user-id
    description
    add-date
    drop-date
    last-update
    process
    feature-dsr
    feature-verify
    defect-dsr
    defect-verify)

(define-class Config (CMVCObject)
    type
    name
    default
    value-1
    value-2
    description)

(define-class Coreq (CMVCObject)
    track-id
    group-id)

(define-class Defect (CMVCObject)
    id
    type
    prefix
    name
    component-id
    env-name
    state
    severity
    abstract
    reference
    answer
    level-name
    last-update
    add-date
    assign-date
    response-date
    end-date
    originator-id
    owner-id
    age
    duplicate
    release-id)

(define-class Environment (CMVCObject)
    name
    release-id
    user-id)

(define-class File (CMVCObject)
    id
    component-id
    source-id
    path-id
    new-path-id
    version-id
    new-version-id
    add-date
    new-add-date
    drop-date
    new-drop-date
    last-update
    base-name
    type
    release-id
    user-login
    file-mode)

(define-class FileOut (CMVCObject)
    file-id
    version-id
    user-id
    checkout-date
    new-sid)

(define-class Fix (CMVCObject)
    track-id
    user-id
    component-id
    state
    add-date
    last-update)

(define-class History (CMVCObject)
    defect-id
    user-id
    action
    add-date)

(define-class Host (CMVCObject)
    user-id
    name
    login
    address)

(define-class Interest (CMVCObject)
    name
    action)

(define-class LevelMember (CMVCObject)
    level-id
    track-id)

(define-class Level (CMVCObject)
    id
    name
    release-id
    user-id
    add-date
    commit-date
    last-update
    state
    type
    level-size)

(define-class Note (CMVCObject)
    defect-id
    user-id
    action
    add-date
    remarks)

(define-class Notification (CMVCObject)
    component-id
    user-id
    interest-name)

(define-class Path (CMVCObject)
    id
    name)

(define-class Release (CMVCObject)
    id
    name
    component-id
    binding
    description
    user-id
    add-date
    drop-date
    last-update
    release-size
    process
    track-subprocess?
    approve-subprocess?
    fix-subprocess?
    level-subprocess?
    test-subprocess?)

(define-class Sequence (CMVCObject)
    name
    last-serial)

(define-class Size (CMVCObject)
    defect-id
    component-id
    user-id
    add-date
    last-update
    state
    sizing
    release-id)

(define-class Test (CMVCObject)
    track-id
    env-name
    user-id
    state
    add-date
    last-update)

(define-class Track (CMVCObject)
    id
    release-id
    defect-id
    user-id
    state
    target
    actual
    add-date
    last-update)

(define-class User (CMVCObject)
    id
    login
    name
    superuser
    area
    address
    add-date
    drop-date
    last-update)

(define-class Verify (CMVCObject)
    defect-id
    user-id
    type
    state
    add-date
    last-update)

(define-class Version (CMVCObject)
    id
    previous-id
    source-id
    user-id
    raw-lines
    code-lines
    comment-lines
    remarks
    change-date
    s-id
    ver-size)

;;
(define $cmvc-classes
   (vector
    Access
    Approval
    Approver
    Authority
    Cfgcomproc
    Cfgrelproc
    Change
    CompMember
    Component
    Config
    Coreq
    Defect
    Environment
    File
    FileOut
    Fix
    History
    Host
    Interest
    LevelMember
    Level
    Note
    Notification
    Path
    Release
    Sequence
    Size
    Test
    Track
    User
    Verify
    Version))

;; redefine `setup-app-indirect-pages' to define our classes, too

(define (setup-app-indirect-pages ps)
    (setup-indirect-page ps 1 $app-classes)
    (setup-indirect-page ps 2 $cmvc-classes))

(define-method write-object ((self CMVCObject) port)
  (let ((m (find-method id (list self))))
    (if m
	(format port "#[~a ~s]" (class-name (object-class self)) (m self))
	(format port "#[~a]" (class-name (object-class self))))))

	