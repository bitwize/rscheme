#|------------------------------------------------------------*-Scheme-*--|
 | File:    compiler/modules/bldctx.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.9
 | File mod date:    2005-04-08 17:43:36
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  (rsc)
 |
 `------------------------------------------------------------------------|#


;; a <build-context> is a compilation context, which includes
;; an underlying module being built


(define-class <build-context> (<object>)
    name   ;; the name of the module
    (part-names init-value: '())
    (file-names init-value: '())
    (public-interface-file init-value: #f)
    (private-interface-file init-value: #f)
    (base-filename init-value: #f)
    (root-variable-info init-value: #f)
    (c-files init-value: '())
    (h-files init-value: '())
    (other-libs init-value: '())
    (extern-h-files init-value: '())
    (public-h-files init-value: '())  ;; our `.h' files that are public
    (user-inc-path init-value: '())     ; pre -I- -I entries
    (system-inc-path init-value: '())   ; post -I- -I entries
    (parts init-value: '())
    (dest-dir init-value: '#uninit type: <directory-name>)
    (dest-dir-exists? init-value: #f)
    (image-dest-dir init-value: '#uninit type: <directory-name>)
    (building init-value: '#uninit type: <module>)
    (needs-c-context init-value: #f type: <boolean>)
    (rewriter-envt init-value: #f)
    (rewriter-cache init-value: #f)
    (top-level-icode init-value: '())
    (patch-time-only-icode init-value: '())
    (documentation-data init-value: '())
    (copied-files init-value: '())
    (makefile-name init-value: "Makefile")
    (fn-def-are-const? init-value: #f))  ;; function definitions are const?


(define (ensure-dest-dir (self <build-context>))
  (if (not (dest-dir-exists? self))
      (set-dest-dir-exists?!
       self
       (let ((path (pathname->string (dest-dir self))))
	 (if (file-exists? path)
	     #t
	     (mkdir path))))))

(define-method initialize ((self <build-context>))
  ;; compute a reasonable base file name from the module name
  (set-base-filename! self (link-name (building self)))
  (reserve-file-name self (base-filename self) "h")
  ;; check to make sure the directory exists
  (set-public-interface-file! self
			      (make <file-name>
				    filename: (base-filename self)
				    extension: "h"
				    file-directory: (dest-dir self)))
  (set-private-interface-file! self
			       (make <file-name>
				     filename: (alloc-file-name 
						self
						(string-append
						 (base-filename self)
						 "_p")
						"c")
				     extension: "h"
				     file-directory: (dest-dir self)))
  self)

;; allocate a new part name, starting from the
;; given base name

(define (alloc-part-name (m <build-context>) (basename <string>))
    (let ((n (if (member basename (part-names m))
		 (let loop ((i 1))
		    (let ((adjusted-name (format #f "~a~d" basename i)))
			(if (member adjusted-name (part-names m))
			    (loop (+ i 1))
			    adjusted-name)))
		 basename)))
      (set-part-names! m (cons n (part-names m)))
      n))

(define (form-file-name (basename <string>) uniquifier)
  (string-append basename 
		 (if uniquifier 
		     (number->string uniquifier) 
		     "")))

;;; mark a file name as reserved

(define (reserve-file-name (m <build-context>) (fn <string>) extn)
  (set-file-names! m (cons fn (file-names m)))
  fn)

;;; note that the returned base name does NOT include the `extn'
;;; OR the destination directory.  That would be nicer, but would
;;; be a fairly pervasive change

(define (alloc-file-name (m <build-context>) (basename <string>) extn)
  (let loop ((i #f))
    (let ((fn (form-file-name basename i)))
      (if (or (member fn (file-names m))
	      (file-exists? (pathname->os-path
			     (make <file-name>
				   filename: fn
				   extension: extn
				   file-directory: (dest-dir m)))))
	  (loop (+ (or i 0) 1))
	  (begin
	    (set-file-names! m (cons fn (file-names m)))
	    fn)))))
