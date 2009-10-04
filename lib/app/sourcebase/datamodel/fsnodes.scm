(define-class <node> (<object>) :abstract
    id
    current-version
    (committed-version init-value: #f)
    (versions type: <version-map>)
    (active-checkout init-value: #f)
    group
    (stable-properties type: <list> init-value: '()))


(define-class <file> (<node>))

(define-class <directory> (<node>))

(define-class <node-version> (<object>) :abstract
    versioned-object
    version-tag
    previous-version
    (modification-time type: <time>)
    (permissions type: <fixnum> init-value: #o644)
    ;;
    ;; version-properties may share structure with previous versions
    ;;
    (version-properties type: <list> init-value: '())
    (change-items type: <list> init-value: '())
    (comment init-value: #f)
    contents)

(define-class <file-version> (<node-version>))

(define-class <directory-version> (<node-version>))

;;
(define-method write-object ((self <node>) port)
    (format port "#[~a ~d]" (class-name (object-class self))
    			       (id self)))
			       
(define-method write-object ((self <node-version>) port)
    (format port "#[~a ~d#~a]" (class-name (object-class self))
    			       (id (versioned-object self))
			       (if (version-tag self)
			       	   (version-tag->string (version-tag self))
				   "#")))
			       

;;

(define-class <checkout> (<object>)
  (user type: <user>)              ;; who checked it out
  checked-out       ;; the <node-version> that this one is based on
  checkout-time     ;; when we checked it out
  (file-system type: <file-system>))      ;; the FS through which we
                                          ;; checked it out (note that the
                                          ;; node may be accessible from 
                                          ;; other FSs)

(define-class <dir-checkout> (<checkout>)
    (new-version type: <directory-version>)
    checked-out-shared-content)

(define-method checked-out ((self <node>))
    (and (active-checkout self)
	 (checked-out (active-checkout self))))

(define (current-dir-version (n <node>))
  (if (instance? n <directory>)
      (if (active-checkout n)
	  (new-version (active-checkout n))
	  (current-version n))
      (current-version n)))
