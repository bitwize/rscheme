#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/objsys/metaobj.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.18
 | File mod date:    2005-02-25 16:39:36
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  objsys
 |
 | Purpose:          internal class protocol implementation
 `------------------------------------------------------------------------|#

(define (compute-class-precedence-list (c <<class>>))
  (if (null? (superclasses c))
      (list c)
      (cons c (compute-class-precedence-list (car (superclasses c))))))


;; return a list of all the class's <slot-descriptor>'s (including inherited)

(define (slot-descriptors (c <<class>>))
  (if (null? (superclasses c))
      (class-direct-slots c)
      (append (slot-descriptors (car (superclasses c)))
	      (class-direct-slots c))))

(define (class-superclasses (class <<class>>))            (gvec-ref class 3))
;
(define (class-instance-size (class <<standard-class>>))  (gvec-ref class 9))
(define (class-direct-slots (class <<standard-class>>))   (gvec-ref class 7))

;;;

(define-method name ((self <<class>>))
  (class-name self))

(define (tclass-supers c)
  (class-superclasses c))

(define (tclass-slots c)
  (slot-descriptors c))

;; find the <slot-descriptor> for a particular slot (by slot index)
;; in a particular class
;;
;; returns #f if the <slot-descriptor> is not found

(define (slot-descriptor-for-class-slot (self <<standard-class>>)
					(slot <fixnum>))
  (let loop ((a (direct-slots self)))
    (if (null? a)
	(if (pair? (superclasses self))
	    (slot-descriptor-for-class-slot (car (superclasses self)) slot)
	    #f)
	(if (eq? (gvec-ref (car a) 4) slot)
	    (car a)
	    (loop (cdr a))))))

(define (slot-descriptor-by-index (self <<standard-class>>) 
				  (slot <fixnum>))
  (slot-descriptor-for-class-slot self slot))

(define (slot-descriptor-by-name (self <<standard-class>>) 
				 (slot-name <symbol>))
  (let loop ((a (direct-slots self)))
    (if (null? a)
	(if (pair? (superclasses self))
	    (slot-descriptor-by-name (car (superclasses self)) slot-name)
	    #f)
	(if (eq? (gvec-ref (car a) 0) slot-name)
	    (car a)
	    (loop (cdr a))))))

;; returns, computing if necessary, the list of all slot descriptors
;; for the given class

(define (class-compute-slots (class <<standard-class>>))
  (or (all-slots class)
      (let* ((sup (superclasses class))
	     (s (append (direct-slots class)
			(if (null? sup)
			    '()
			    (class-compute-slots (car sup))))))
	(set-all-slots! class s)
	s)))

;;

(define-method slot-type ((self <slot-descriptor>))
  (type-restriction self))

(define-method slot-value ((self <slot-descriptor>) item)
  (if (memq self (slot-descriptors (object-class item)))
      (gvec-ref item (index self))
      (error "slot-value: slot ~s not present in ~s" (name self) item)))

(define-method set-slot-value! ((self <slot-descriptor>) item value)
  (if (memq self (slot-descriptors (object-class item)))
      (if (instance? value (type-restriction self))
	  (begin
	    (gvec-set! item (index self) value)
	    (values))
	  (error "set-slot-value!: value ~s for slot ~s in ~s is\nnot compatible with the type restriction"
		 value (name self) item))
      (error "set-slot-value!: slot ~s not present in ~s" (name self) item)))



(define-method create-getter-method ((self <slot-descriptor>) 
                                     (for <<class>>))
  (make <getter>
        template: getter-template
        environment: (make-gvec <binding-envt>
                                #f
                                (index self))
        slot-descriptor: self
        type-restriction: (type-restriction self)
        index: (index self)
        function-specializers: (list for)))

(define-method create-setter-method ((self <slot-descriptor>)
                                     (for <<class>>))
  (let ((check? (eq? (type-restriction self) <object>)))
    (make <setter>
          template: (if check?
                        restricted-setter-template
                        setter-template)
          environment: (if check?
                           (make-gvec <binding-envt>
                                      #f
                                      (index self)
                                      (type-restriction self))
                           (make-gvec <binding-envt>
                                      #f
                                      (index self)))
          slot-descriptor: self
          type-restriction: (type-restriction self)
          index: (index self)
          function-specializers: (list for (type-restriction self)))))
