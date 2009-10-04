#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/mlink/patch.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.8
 | File mod date:    1998-05-23 20:29:02
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  mlink
 |
 | Purpose:          patch up a loaded module to refer to local objects
 `------------------------------------------------------------------------|#

;; a <patch> is something which both represents
;; an object to be linked to, and stores back
;; pointers to objects in the current image
;; which refer to it.
;; initially, the backpointers are created by
;; a traversal of the heap, looking for pointers
;; to <patch> instances.  Later, the backpointers
;; are maintained incrementally throughout the
;; linkage process

(define-class <patch> (<object>)
  description
  link-command
  (transforms init-value: '())  ;; an alist to keep track of which <patch>
                    ;; results from a transformation on this
                    ;; patch.  Valid keys are:
                    ;;   value
                    ;;   full-procedure-bdg
  (places init-value: '#()))


(mifio-class "<patch>" <patch>)

;; places is the actual record of pointers to this object
;; it can be implemented either as a list or a vector.

#|*************************************************************|#

;; the link-bdgs-merge function is called when some modules
;; are linked into one and more than one has a link-bdgs for
;; some other module not being bound

(define (link-bdgs-merge (link-bdgss <list>))
  (let ((t (make-table eq? symbol->hash)))
    (for-each
     ;; consider one of the source <link-bdgs>
     (lambda ((lb <link-bdgs>))
       ;; examine all of it's bindings
       (table-for-each
	(imported-bindings lb)
	(lambda (h k v)
	  ;; basically, this amounts to installing it
	  ;; in the destination table.  If something
	  ;; is already there, append them
	  (let ((p (table-lookup t k)))
	    (table-insert! t k (if p
				   (patch-vector-append p v)
				   v))))))
     link-bdgss)
    (make <link-bdgs> 
	  owner: #f
	  imported-bindings: t)))


;; note: you can't do a pass-through 
;; (ie, can't export the same bdg you import)
;; I consider this a bug

(define-method execute-link-cmd ((self <link-bdgs>) 
				 (in <module>)
				 (in-name <symbol>)
				 (target <module>)
				 (target-name <symbol>))
  (table-join (module-exports target)
	      (imported-bindings self)
	       ;; ignore extra stuff in exports table
	       #f  
	       ;; extra stuff in imports table is fatal
	       #f
	       #| it should be, anyway
	       (lambda (k v)
		 (format #t
			 "~s missing from ~s (required by ~s)\n"
			 k
			 target-name
			 in-name))
	       |#
	       ;; but the intersection means work --
	       ;; link 'em together
	       (lambda (exported-k 
			exported-v
			import-k
			import-v)
		 (if *verbose-link*
		     (format #t "    linking to ~s from ~s\n"
			     import-k
			     target-name))
		 (patch! import-v exported-v))))

(define-method execute-link-cmd ((self <link-value>) 
				 (in <module>)
				 (in-name <symbol>)
				 (for <module>)
				 (target-name <symbol>))
  (patch! (patch self) (value (actual-bdg (binding self)))))


(define-method execute-link-cmd ((self <link-method>) 
				 (in <module>) 
				 (in-name <symbol>)
				 (for <module>)
				 (target-name <symbol>))
  (let ((gf (value (actual-bdg (gf-bdg self)))))
    (for-each
     (lambda (m)
       (if *verbose-link*
	   (format #t "adding method ~s to gf ~s\n" m gf))
       (target-add-method gf m))
     (methods self))))

(define-method execute-link-cmd ((self <link-xform>)
				 (in <module>) 
				 (in-name <symbol>)
				 (for <module>)
				 (target-name <symbol>))
  (let ((r (apply-xform (actual-value (src-patch self)) (operation self))))
    (if *verbose-link*
	(format #t "\twill patch: (~s ~s) = ~s\n"
		(operation self)
		(src-patch self)
		r))
    (patch! (patch self) r)))
	  


(define (patch! (p <patch>) target)
  (let ((d (description p))
	(pv (places p)))
    (if *verbose-link*
	(begin
	  (if (pair? d)
	      (format #t "\tpatching: ~a in ~a" (car d) (cadr d))
	      (format #t "\tpatching: ~a" d))
	  (format #t " (~d patches)\n" (quotient (vector-length pv) 2))))
    (let ((n (vector-length pv)))
      (let loop (((i <fixnum>) 0))
	(if (< i n)
	    (begin
	      (gvec-set! (vector-ref pv i)
			 (vector-ref pv (+ i 1))
			 target)
	      (loop (+ i 2))))))))

(define (patch-vector-append (a <vector>)
			     (b <vector>))
  (list->vector
   (append (vector->list a)
	   (vector->list b))))

(define-class <imported-binding> (<patch>)
  name
  remote-binding
  ib-writable?
  ib-shared?)

(mifio-class "<imported-binding>" <imported-binding>)

(define-method write-object ((self <imported-binding>) port)
  (format port "#[ib ~s from ~s`~s]"
	  (name self)
	  (cadr (description self))
	  (car (description self))))


(define (add-patch! (p <patch>) x i)
  (set-places! p
	       (list->vector
		(append (vector->list (places p))
			(list x i)))))

;; crawl over an image, installing backpointers into
;; <patch> instances

(define (install-patch-back-pointers root)
  (let ((tbl (make-object-table)))
    (table-insert! tbl root 0)
    ;; pre-fill the table with the `mif' roots
    (for-each (lambda (r)
		(table-insert! tbl r -1))
	      (value-sequence (mif-load-defs)))
    ;;
    (let qloop ((q (cons root '()))
		(k 1))
      (if (null? q)
	  k
	  (let ((x (car q)))
	    (let loop (((i <fixnum>) (sub1 (gvec-length x)))
		       (q0 (cdr q)))
	      (if (eq? i -1)
		  (qloop q0 (add1 k))
		  (let ((p (gvec-ref x i)))
		    (if (instance? p <patch>)
			(begin
			  (add-patch! p x i)
			  (loop (sub1 i) q0))
			(loop (sub1 i)
			      (if (and (gvec? p)
				       (not (table-lookup tbl p)))
				  (begin
				    (table-insert! tbl p k)
				    (cons p q0))
				  q0)))))))))))

#|
(define (find-patches x (tbl <table>))
  ;; do we care about it at all?
  ;;   (ie, is it a gvec and have we not seen it before)
  (if (and (gvec? x)
	   (not (table-lookup tbl x)))
      (begin
	(table-insert! tbl x #t)
	(let loop ((i (- (gvec-length x) 1)))
	  (if (not (eq? i -1))
	      (let ((p (gvec-ref x i)))
		(if (instance? p <patch>)
		    (add-patch! p x i)
		    (find-patches p tbl))
		(loop (- i 1))))))))
|#

(define (actual-value self)
  (if (instance? self <imported-binding>)
      (actual-value (remote-binding self))
      (if (instance? self <patch>)
	  (let (((lc <link-xform>) (link-command self)))
	    (apply-xform (actual-value (src-patch lc))
			 (operation lc)))
	  self)))

(define (actual-bdg bdg)
  (if (instance? bdg <imported-binding>)
      (actual-bdg (remote-binding bdg))
      bdg))

(define (apply-xform target (op <symbol>))
  (case op
    ((full-procedure-bdg)
     (full-procedure-bdg (actual-value target)))
    ((value)
     ;(print target)
     (value (actual-value target)))
    (else
     (error "mlink: bad transformation: ~s to ~s\n" op target))))

;---------------------

;; transform the objects referred to by <patch>'s
;; (return a new <patch>)

(define (xform thing operation)
  (if (instance? thing <patch>)
      (xform-patch thing operation)
      (apply-xform (actual-value thing) operation)))

(define (xform-patch (self <patch>) (xform <symbol>))
  (let ((x (assq xform (transforms self))))
    (if x
	;; this transformation has already been applied
	;; before to this patch, so return the same 
	;; transformed patch
	(cdr x)
	;; this is a new transformation for this
	;; particular patch.  create a new patch
	;; for it and add it to the link-commands
	;; of the appropriate module
	(let* (((the-owner <imported-module>) (owner (link-command self)))
	       (new-lc (make <link-xform>
			     owner: the-owner
			     src-patch: self
			     patch: #f
			     operation: xform))
	       (new-patch (make-xform-patch self xform new-lc)))
	  (set-patch! new-lc new-patch)
	  (set-link-commands! the-owner
			      (append (link-commands the-owner)
				      (list new-lc)))
	  (set-transforms! self (cons (cons xform new-patch)
				      (transforms self)))
	  new-patch))))

(define (make-xform-patch (p <patch>) (xform <symbol>) (lc <link-xform>))
  (case xform
    ((full-procedure-bdg)
     (assert (instance? p <imported-binding>))
     (make <imported-binding>
	   name: (list xform (name p))
	   description: (list xform (description p))
	   link-command: lc
	   remote-binding: p
	   ib-writable?: #f
	   ib-shared?: #f))
    (else
     (make <patch>
	   description: (list xform (description p))
	   link-command: lc))))
