#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/compiler/icode.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.12
 | File mod date:    1999-01-23 12:10:02
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  compiler
 |
 | Purpose:          Intermediate code class accessors, initializers, and printers
 `------------------------------------------------------------------------|#

(define-generic-function first-return-type)

(define-generic-function gen-ctc)
(define (icode? thing)
  (instance? thing <icode>))

(define-method initialize ((self <icode>))
  (warning "Did not get initialized: ~s" self)
  self)

(define-method first-return-type ((self <expr-icode>))
  (if (null? (return-types self))
      <boolean> ;; ie, #f
      (car (return-types self))))

(define-method initialize ((self <ic-cast>))
  (set-does-lambda?! self (does-lambda? (expr self)))
  (set-does-save?! self (does-save? (expr self)))
  (set-vars-referenced! self (vars-referenced (expr self)))
  (set-vars-captured! self (vars-captured (expr self)))
  self)

(define-method return-types ((self <ic-jump>))
  '())

(define-method initialize ((self <ic-call>))
  (set-does-lambda?! self (or (does-lambda? (function self))
			      (does-lambda? (args self))))
  (set-does-save?! self (or (does-save? (args self))
			    (does-save? (function self))
			    (not (eq? (mode self) 'tail))))
  (set-vars-referenced! self (append (vars-referenced (function self))
				     (vars-referenced (args self))))
  (set-vars-captured! self (append (vars-captured (function self))
				   (vars-captured (args self))))
  self)

(define-method initialize ((self <ic-call-prim>))
  (set-does-lambda?! self (does-lambda? (args self)))
  (set-does-save?! self (does-save? (args self)))
  (set-vars-referenced! self (vars-referenced (args self)))
  (set-vars-captured! self (vars-captured (args self)))
  self)

(define-method initialize ((self <ic-tl-ref>))
  self)

(define-method initialize ((self <ic-lex-ref>))
  (set-vars-referenced! self (list (var self)))
  self)

(define-method initialize ((self <ic-tl-set>))
  (set-does-lambda?! self (does-lambda? (rhs self)))
  (set-does-save?! self (does-save? (rhs self)))
  (set-vars-referenced! self (vars-referenced (rhs self)))
  (set-vars-captured! self (vars-captured (rhs self)))
  self)

(define-method initialize ((self <ic-lex-set>))
  (set-does-lambda?! self (does-lambda? (rhs self)))
  (set-does-save?! self (does-save? (rhs self)))
  (set-vars-referenced! self (vars-referenced (rhs self)))
  (set-vars-captured! self (vars-captured (rhs self)))
  (set-ever-set?! (var self) #t)
  self)

;;;

(define-method initialize ((self <ic-if>))
  (set-does-lambda?! self (or (does-lambda? (condition self)) 
			      (does-lambda? (if-true self)) 
			      (does-lambda? (if-false self))))
  (set-does-save?! self (or (does-save? (condition self)) 
			    (does-save? (if-true self)) 
			    (does-save? (if-false self))))
  (set-vars-referenced! self (append (vars-referenced (condition self))
				     (vars-referenced (if-true self))
				     (vars-referenced (if-false self))))
  (set-vars-captured! self (append (vars-captured (condition self))
				   (vars-captured (if-true self))
				   (vars-captured (if-false self))))
  self)


(define-method initialize ((self <ic-const>))
  self)

(define (const-type value)
  (cond
   ((integer? value) '<fixnum>)
   ((string? value) '<string>)
   ((boolean? value) '<boolean>)
   ((symbol? value) '<symbol>)
   (else '<obj>)))

(define (make-const value mode)
  (make <ic-const>
	value: value
	mode: mode
	return-types: (list (const-type value))))

(define-method initialize ((self <ic-procedure>))
  (set-does-lambda?! self (does-lambda? (body self)))
  (set-does-save?! self (does-save? (body self)))
  (set-vars-referenced! self (vars-referenced (body self)))
  (set-vars-captured! self (vars-captured (body self)))
  self)

(define-method initialize ((self <ic-lambda>))
  (set-does-lambda?! self #t)
  (set-does-save?! self #f)
  (set-return-types! self (list '<function>))
  (set-vars-referenced! self (vars-referenced (proc self)))
  (set-vars-captured! self (vars-referenced (proc self)))
  self)


(define-method num-args ((self <ic-multi>))
  (length (arg-list self)))

(define-method initialize ((self <ic-multi>))
  (set-does-lambda?! self (any? does-lambda? (arg-list self)))
  (set-does-save?! self (any? does-save? (arg-list self)))
  (set-return-types! self (map first-return-type (arg-list self)))
  (set-vars-referenced! self (apply append
				    (map vars-referenced (arg-list self))))
  (set-vars-captured! self (apply append
				  (map vars-captured (arg-list self))))
  self)


(define-method initialize ((self <ic-seq>))
  (set-does-lambda?! self (any? does-lambda? (stmt-list self)))
  (set-does-save?! self (any? does-save? (stmt-list self)))
  (set-vars-referenced! self (apply append
				    (map vars-referenced (stmt-list self))))
  (set-vars-captured! self (apply append
				  (map vars-captured (stmt-list self))))
  self)


(define-method write-object ((self <ic-call>) port)
  (format port "[call ~s ~s]" (function self) (args self)))

(define-method write-object ((self <ic-call-prim>) port)
  (format port "[call-prim ~a ~s]" (function self) (args self)))

(define-method write-object ((self <ic-const>) port)
  (format port "'~s" (value self)))

(define-method write-object ((self <ic-multi>) port)
  (display "{multi" port)
  (let loop ((x (arg-list self)))
    (if (pair? x)
	(begin
	  (write-char #\space port)
	  (write (car x) port)
	  (loop (cdr x)))))
  (write-char #\} port))

(define-method write-object ((self <ic-bind>) port)
  (format port "{bind ~s=~s; ~s}" 
	  (map name (bindings (envt self)))
	  (inits self)
	  (body self)))

(define-method write-object ((self <ic-seq>) port)
  (display "{seq" port)
  (for-each
   (lambda (i)
     (write-char #\space port)
     (write i port))
   (stmt-list self))
  (write-char #\} port))

(define-method write-object ((self <ic-tl-ref>) port)
  (format port "[tlref ~a]" (name (var self))))

(define-method write-object ((self <ic-tl-set>) port)
  (format port "[tlset ~a=~s]" (name (var self)) (rhs self)))

(define-method write-object ((self <ic-lex-ref>) port)
  (format port "[lexref ~a]" (name (var self))))

(define-method write-object ((self <ic-lex-set>) port)
  (format port "[lexset ~a=~s]" (name (var self)) (rhs self)))


(define-method num-args ((self <ic-loop>))
  (length (bindings (envt self))))

(define-method rest? ((self <ic-loop>))
  #f)

(define-method initialize ((self <ic-loop>))
  (set-does-lambda?! self (or (does-lambda? (body self))
			      (does-lambda? (inits self))))
  (set-does-save?! self (or (does-save? (body self))
			    (does-save? (inits self))))
  (set-vars-referenced! self (append (vars-referenced (inits self))
				     (vars-referenced (body self))))
  (set-vars-captured! self (append (vars-captured (inits self))
				   (vars-captured (body self))))
  self)

(define-method initialize ((self <ic-jump>))
  (set-does-lambda?! self (does-lambda? (args self)))
  (set-does-save?! self (does-save? (args self)))
  (set-vars-referenced! self (vars-referenced (args self)))
  (set-vars-captured! self (vars-captured (args self)))
  self)
