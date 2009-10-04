
;;
;;  change requests are the focus of changes to the code base
;;  they may report observed or hypothesized defects or deficiencies,
;;  or they may advocate a new feature
;;
;;  an non-closed change request always has at least one active
;;  work item associated with it, and it is said to be in the
;;  "queue" of the owners of those work items
;;

(define-class <change-request> (<object>)
    (id type: <fixnum>)
    (title type: <string>)
    (group type: <group>)
    (summary type: <string>)
    (history type: <list> init-value: '())              ; (list-of <work-item>)
    (interest type: <list> init-value: '())             ; (list-of <user>)
    (waiting-items type: <list> init-value: '())        ; (list-of <work-item>)
    (active-items type: <list> init-value: '())         ; (list-of <work-item>)
    (state type: <symbol> init-value: 'open)            ; (union-of 'open ...)
    (properties type: <list> init-value: '()))

(define-method write-object ((self <change-request>) port)
    (format port "#[<change-request> id: ~d]" (id self)))

(define-method display-object ((self <change-request>) port)
    (format port "~d" (id self)))

(define-method open-time ((self <change-request>))
  (timestamp (activate-audit-entry
	      (last (if (null? (history self))
			(active-items self)
			(history self))))))

(define-method open-user ((self <change-request>))
  (user (activate-audit-entry
         (last (if (null? (history self))
                   (active-items self)
                   (history self))))))
