;;;
;;; note:  this implementation does not make use of standard
;;;        X resource stuff, but it is intended to be somewhat
;;;        compatible in the sense that the underlying implementation
;;;        could access an X resource database.
;;;
;;;        This is simply the application-level interface to accessing
;;;        and defining resources (esp. since there doesn't appear to
;;;        be any type information in the Xrdb stuff)
;;;
;;;        Not implemented here is the ability to have runtime-determined
;;;        resources (eg, the "complete resource path" might include a
;;;        window name).  I think this could be done by parameterizing
;;;        the accessor with the runtime-specific values, once an underlying
;;;        resource manager that supports it is in place (eg, CLX Resources)
;;;

(define-class <resource-descriptor> (<object>) :abstract
  (index type: <fixnum>)
  (name type: <symbol>)
  default-value)

(define-class <string-resource-descriptor> (<resource-descriptor>))
(define-class <color-resource-descriptor> (<resource-descriptor>))
(define-class <lt-color-resource-descriptor> (<color-resource-descriptor>))
(define-class <dk-color-resource-descriptor> (<color-resource-descriptor>))
(define-class <font-resource-descriptor> (<resource-descriptor>))

(define *app-resources* '())

(define-macro (define-X-resource name (type) default)
  (let* ((n (length *app-resources*))
	 (t (case type
	      ((<string>) <string-resource-descriptor>)
	      ((<color>) <color-resource-descriptor>)
	      ((<lt-color>) <lt-color-resource-descriptor>)
	      ((<dk-color>) <dk-color-resource-descriptor>)
	      ((<font>) <font-resource-descriptor>)
	      (else (em at: name 910 "unrecognized resource type: ~s" type)))))
    ;
    (set! *app-resources*
	  (vector-append *app-resources*
			 (vector (make <resource-descriptor>
				       name: name
				       resource-type: type
				       default-value: default
				       index: n))))
    ;
    `(define (,name #optional (client default: (current-client)))
       (get-resource client ,n))))

;;;

(define (get-resource client index)
  (let ((v (resource-vector client))
	(r (vector-ref *app-resources* index)))
    (if (< index (vector-length v))
	(vector-ref v index)
	(em 911 "dynamic resource definitions not supported: ~s" r))))

;;;

(define (bind-resources! (c <client>))
  (set-resource-vector! 
   c
   (vector-map
    (lambda (r)
      (alloc-resource r c))
    *app-resources*)))

;; default method simply returns the default value

(define-method alloc-resource ((self <resource-descriptor>) client)
  (default-value self))

(define-method alloc-resource ((self <font-resource-descriptor>) client)
  (open-font (on-display client) (next-method)))

(define-method alloc-resource ((self <color-resource-descriptor>) client)
  (let ((color-spec (next-method)))
    (handler-case
     (alloc-color (using-colormap client) color-spec)
     ((<x-error>)
      (dm at: (name self) "could not allocate color `~a'" color-spec)
      (cond
       ((instance? self <lt-color-resource-descriptor>)
	(screen-white-pixel (on-screen client)))
       ((instance? self <dk-color-resource-descriptor>)
	(screen-black-pixel (on-screen client)))
       (else
	(em at: (name self) "could not allocate required color `~a'"
	    color-spec)))))))

