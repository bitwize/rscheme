(define-class <vsh-state> (<object>)
    (current-filespace init-value: #f) ;; #f or a <file-space>
    (current-path  init-value: #f) ;; #f or an <fs-absolute-path>
    (variables type: <string-table>)
    (user type: <user>)
    (group type: <group>)
    (current-local-top type: <directory-name>)
    (reasons type: <list> init-value: '()))

(define-method current-filesystem ((self <vsh-state>))
    (if (instance? (current-filespace self) <file-system>)
        (current-filespace self)
	(error "~a#~a: read-only filespace (snapshot)" 
		(name (versioned-object (current-filespace self)))
		(name (current-filespace self)))))

(define (make-vsh-state (user <user>))
   (make <vsh-state>
	 variables: (make-table string=? string->hash)
	 group: (world-group *application*)
	 user: user
	 current-local-top: (string->dir (os-getwd))))
	 
(define *vsh-state* #f)
