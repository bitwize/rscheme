;;
;; checked-out directory nodes are stored inside the system, so when
;; it is locked (checked out), we create the new version on-the-fly
;; and store it in the <dir-checkout>
;;
(define-method klock-node ((self <directory>) 
			   (path <fs-absolute-path>) 
			   (user <user>)
			   fs)
    (let* (((old-v <directory-version>) (current-version self))
	   (new-v (make <directory-version>
			%alloc-area: (area-of self)
			versioned-object: self
			contents: (contents old-v)
			previous-version: old-v
			version-tag: #f
			permissions: (permissions old-v)
			version-properties: (version-properties old-v)
			modification-time: *timestamp*)))
      (let ((co (make <dir-checkout>
		      %alloc-area: (area-of self)
		      user: user
		      file-system: fs
		      checked-out: old-v
		      checkout-time: *timestamp*
		      checked-out-shared-content: (contents old-v)
		      new-version: new-v)))
	(set-active-checkout! self co)
	(set-check-outs! user (cons co (check-outs user)))
	co)))

;;
;; regular files are reserved by locking
;;

(define-method klock-node ((self <file>) 
			   (path <fs-absolute-path>) 
			   (user <user>)
			   fs)
  (let ((co (make <checkout>
		  checked-out: (current-version self)
		  user: user
		  file-system: fs
		  checkout-time: *timestamp*)))
    (set-active-checkout! self co)
    (set-check-outs! user (cons co (check-outs user)))
    co))
