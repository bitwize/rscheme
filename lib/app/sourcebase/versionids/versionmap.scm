;;;
;;;  A version map is a set of leafs and branches, with each
;;;  leaf having an associated value
;;;

(define-class <version-map> (<object>)
    latest-root-branch)
    
(define-class <version-tree-node> (<object>) :abstract
    (predecessor init-value: #f) ;; one of the same type or #f
    (tag type: <fixnum> init-value: 1)
    parent ;; one of the other type, or a <version-map>
    (latest-child init-value: #f)) ;; one of the other type, or #f
    
(define-class <version-tree-branch> (<version-tree-node>))

(define-class <version-tree-leaf> (<version-tree-node>)
    value)
