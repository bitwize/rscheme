
(define-module dv.ascii.steal ()
  (&module (import usual-inlines)
	   (export gvec-alloc 
		   bvec-copy
		   bvec-length
		   gvec-set!
		   with-objects-from-file
		   keyword-value-list->vector)))

,(use dv.ascii.steal)

,(use objsys)
,(use sort)
,(use calendar)
,(use regex)

;;;
;;;  Dump the object graph in an ascii format
;;;

(define-method first-class? ((self <time>)) #t)
(define-method first-class? ((self <dv-object>)) #t)
(define-method first-class? ((self <pair>)) #t)
(define-method first-class? ((self <vector>)) #t)

(define-method first-class? ((self <object>)) #f)


;;;
;;;  Pass I.  Collect and identify all objects in the model
;;;

(define-method object-model-pointers ((self <string>)) '#())
(define-method object-model-pointers ((self <time>)) '#())
(define-method object-model-pointers ((self <number>)) '#())

(define-method object-model-pointers ((self <rect>)) '#())
(define-method object-model-pointers ((self <point>)) '#())
(define-method object-model-pointers ((self <size>)) '#())
(define-method object-model-pointers ((self <transform>)) '#())
  
(define-method object-model-pointers ((self <pair>))
  (vector (car self) (cdr self)))

(define-method object-model-pointers ((self <vector>))
  self)

(define-method object-model-pointers ((self <dv-object>))
  (clone2 self <vector>))

(define (collect-all-objects root)
  (let ((tbl (make-object-table))
	(q (make-dequeue)))
    ;;
    (define (visit o)
      (if (and (first-class? o)
	       (not (table-lookup tbl o)))
	  (begin
	    (dequeue-push-back! q o)
	    (table-insert! tbl o (table-size tbl)))))
    ;;
    (define (scan o)
      (vector-for-each visit (object-model-pointers o)))
    ;;
    (visit root)
    (let loop ()
      (if (dequeue-empty? q)
	  tbl
	  (begin
	    (scan (dequeue-pop-front! q))
	    (loop))))))

;;;
;;;   Pass II.  Render the document objects
;;;

;;;  Atomic values

(define-method marshall-as-ascii ((self <string>) tbl) self)
(define-method marshall-as-ascii ((self <number>) tbl) self)
(define-method marshall-as-ascii ((self <empty-list>) tbl) ''())

;;;  Simple compound values

(define-method marshall-as-ascii ((self <rect>) tbl)
  (cons 'make-rect (values->list (rect->values self))))

(define-method marshall-as-ascii ((self <point>) tbl)
  (cons 'make-point (values->list (point->values self))))

(define-method marshall-as-ascii ((self <size>) tbl)
  (cons 'make-size (values->list (size->values self))))

(define-method marshall-as-ascii ((self <transform>) tbl)
  (cons 'make-transform (vector->list (matrix self))))

(define-method marshall-as-ascii ((self <time>) tbl)
  (list 'make-time (time->string self "%Y-%m-%d %H:%M:%S UTC" #f)))

;;;  Complex compound values

(define-method marshall-as-ascii ((self <pair>) tbl)
  (list 'cons
	(marshall-ref tbl (car self))
	(marshall-ref tbl (cdr self))))

(define-method marshall-as-ascii ((self <vector>) tbl)
  (cons 'vector
	(map (lambda (e)
	       (marshall-ref tbl e))
	     (vector->list self))))

(define-method marshall-as-ascii ((self <dv-object>) tbl)
  (cons* 'make-dv-object
	 (name (object-class self))
	 (apply append
		(map (lambda (s)
		       (list (with-module corelib (init-keyword s))
			     (marshall-ref tbl (slot-value s self))))
		     (class-compute-slots (object-class self))))))

;;;

(define (marshall-ref tbl x)
  (if (first-class? x)
      (list 'ref (or (table-lookup tbl x)
		     (em "didn't get collected: ~s" x)))
      (marshall-as-ascii x tbl)))

;;;
;;;


;;; eval-time procs

(define *ascii-tbl* #f)

(define (ref n)
  (or (table-lookup *ascii-tbl* n)
      (em "unknown ref: ~s" n)))

(define (make-dv-object class . inits)
  (let ((o (gvec-alloc class (instance-size class) #f))
	(v (keyword-value-list->vector inits)))
    (for-each
     (lambda (s)
       (initialize-slot! s o v))
     (class-compute-slots class))
    o))

(define (iso-time year month day hh mm ss tz)
  (let ((month (string->month month))
	(tzdelta (string->timezone tz))
	(year (string->number year))
	(day (string->number day)))
    (and month
	 tzdelta
	 (>= year 1969)
	 (< year 2038)
	 (day->time (date->day (ymd->date year month day))
		    (+ (* (string->number hh) 3600)
		       (* (string->number mm) 60)
		       (if ss (string->number ss) 0)
		       (- tzdelta))))))
  
(add-time-pattern!
 iso-time
 (reg-expr->proc
  '(entire (seq (save (seq digit digit digit digit))  ;; YYYY
		#\-
		(save (seq digit (? digit)))          ;; mm
		#\-
		(save (seq digit (? digit)))          ;; dd
		(+ space)
		(save (seq digit (? digit)))          ;; HH
		#\:
		(save (seq digit (? digit)))          ;; MM
		#\:
		(save (seq digit (? digit)))          ;; SS
		(+ space)
		(save (or (+ (range #\A #\Z))
			  ;; technically, the +/- is required
			  ;; but lets be slightly more forgiving...
			  (seq (? (or #\+ #\-))
			       digit digit digit digit)))))))


(define (make-time str)
  (string->time str))

(define (make-transform . opts)
  (concatenate-transform $identity-transform (list->vector opts)))

;;; load-time proc

(define (pre-alloc expr)
  (case (car expr)
    ((make-time) 
     (time))
    ((vector)
     (make-vector (length (cdr expr))))
    ((cons)
     (cons #f #f))
    ((make-dv-object) 
     (let ((class (eval (cadr expr))))
       (gvec-alloc class (instance-size class) #f)))
    (else
     (em "pre-alloc? ~s" expr))))

(define-method fill-in! ((self <pair>) p)
  (set-car! self (car p))
  (set-cdr! self (cdr p)))

(define-method fill-in! ((self <vector>) v)
  (for-each
   (lambda (i)
     (vector-set! self i (vector-ref v i))
     (values))
   (range (vector-length self))))

(define-method fill-in! ((self <time>) t)
  (bvec-copy self 0 t 0 (bvec-length self))
  (values))

(define-method fill-in! ((self <dv-object>) o)
  (for-each
   (lambda (i)
     (gvec-set! self i (gvec-ref o i))
     (values))
   (range (gvec-length self))))

;;;

(define (load-from-ascii file)
  (let ((tbl (make-fixnum-table)))
    ;; Pass I -- pre-allocation
    (with-objects-from-file
     file
     (lambda (e)
       (dm "load: ~#*@30s" e)
       (table-insert! tbl (car e) (pre-alloc (cadr e)))))
    ;; Pass II -- elaboration
    (set! *ascii-tbl* tbl)
    (with-objects-from-file
     file
     (lambda (e)
       (fill-in! (table-lookup tbl (car e)) (eval (cadr e)))))
    (set! *ascii-tbl* #f)
    (table-lookup tbl 0)))

(define (save-to-ascii doc file)
  (let* ((tbl (collect-all-objects doc))
	 (marsh (sort (map (lambda (k)
			     (list k
				   (table-lookup tbl k)
				   (marshall-as-ascii k tbl)))
			   (key-sequence tbl))
		      (lambda (a b)
			(< (cadr a) (cadr b))))))
    (dm "~a: ~d objects" file (table-size tbl))
    (with-output-to-file
	file
      (lambda ()
	(for-each
	 (lambda (m)
	   (format #t ";;; ~s = ~#@*65s\n" (cadr m) (car m))
	   (pp (cdr m))
	   (newline))
	 marsh)))))

;;;
;;;

(define (t)
  (save-to-ascii (underlying-object
		  (in-document (car (open-views (current-client)))))
		 "test.dva"))
