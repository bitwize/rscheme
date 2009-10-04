
(define-class <file-space> (<object>) :abstract
   (node->path-cache init-value: #f))

(define-class <file-system> (<file-space>)
   (name type: <string>)
   (snapshot-table type: <string-table>)
   root-directory
   (owner type: <user>)
   (group type: <group>)
   (audit-log type: <list>)
   (properties type: <list> init-value: '()))

(define-class <snapshot> (<file-space>)
   (name type: <string>)
   (versioned-object type: <file-system>)
   (node-version-map type: <hash-integer-table>)
   (properties type: <list> init-value: '()))

(define (committed? (fsv <snapshot>))
   (if (assq 'committed (properties fsv))
       #t
       #f))

(define-method write-object ((self <file-system>) port)
    (format port "#[<file-system> ~a]" (name self)))

(define-method write-object ((self <snapshot>) port)
    (format port "#[<snapshot> ~a#~a]" 
    	(name (versioned-object self))
    	(name self)))

(define-method display-object ((self <file-system>) port)
    (write-string port (name self)))

(define-method display-object ((self <snapshot>) port)
  (format port "~a~c~a"
	  (name (versioned-object self))
	  $version-delim
	  (name self)))

