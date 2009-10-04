
(define-class <group> (<object>)
    (name type: <string>)
    (id type: <integer>)	;; this id is for "tar" and VFS's
    (parent-groups type: <list> init-value: '())
    (child-groups type: <list> init-value: '())
    (owner type: <user>)
    (audit-log type: <list>)
    (properties type: <list> init-value: '())
    (super-groups-cache init-value: #f))  ;; self and all parents

(define (super-groups (g <group>))
   (or (super-groups-cache g)
       (let ((c (cons g (apply unionq (map super-groups (parent-groups g))))))
          (set-super-groups-cache! g c)
	  c)))

(define-method write-object ((self <group>) port)
   (format port "#[<group> ~a]" (name self)))

(define-method display-object ((self <group>) port)
  (write-string port (name self)))

