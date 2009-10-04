#|------------------------------------------------------------*-Scheme-*--|
 | File:	    modules/compiler/srfi0.scm
 |
 |          Copyright (C)1999 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |	    See <http://www.rscheme.org/> for the latest info.
 |
 | File version:     1.11
 | File mod date:    2007-01-28 11:52:37
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  mlink
 |
 | Purpose:          implement SRFI-0
 |
 | See:              <http://srfi.schemers.org/srfi-0/srfi-0.html>
 |  
 `------------------------------------------------------------------------|#

;;;
;;;  In RScheme, there are two ways for a feature to be recognized as
;;;  implemented.  First, the feature may be globally implemented.
;;;  RScheme provides feature ids corresponding to underlying features
;;;  of the platform.  For example, a Linux system contains the symbol
;;;  `linux' in the list returned by `(os-type)', and correspondingly
;;;  implements the feature `os.linux'.  Likewise, a little-endian
;;;  system implements `os.little-endian'.  Another example is SRFI-0
;;;  itself.  If there were a SRFI for case sensitive identifiers,
;;;  that would also be globally implemented.
;;;
;;;  The other way in which a feature might be recognized as
;;;  implemented is to have imported a module which implements the
;;;  feature.  This supports locally selectable features as may be
;;;  represented by variable libraries such as SRFI-1.  To load
;;;  a portable program that prefers to have such a SRFI available,
;;;  simply specify the RScheme module which implements the SRFI
;;;  on the command line, like so:
;;;
;;;     rs +srfi.1 portable-listy-program.scm
;;;
;;;  the portable-listy-program will find (if-implements SRFI-1 t)
;;;  expands to t.
;;;
;;;  A module implements a feature F if it has a property `implements',
;;;  and F is an member of the value of that property.  When a module
;;;  is imported, the set of features implemented is merged into that
;;;  of the importing module. [hmm -- this fails to distinguish between
;;;  internally-implemented and exports-implementation]
;;;
;;;  A module created using `define-module' can use the `implements'
;;;  operator to declare that it implements some feature:
;;;
;;;    (define-module contrib.lists ()
;;;      (&module (implements SRFI-1))
;;;      ...)
;;;
;;;  This causes the `implements' property on the module to be set.
;;;
;;;  n.b., there may be a better way to do this, e.g., using per-module
;;;  variables instead of module properties...

(%early-once-only
 (define *globally-implemented* 
   (vector-append
    '#(rscheme RScheme
       ;; the base system implements these SRFIs...
       srfi-0  SRFI-0
       srfi-6  SRFI-6
       srfi-10 SRFI-10
       srfi-28 SRFI-28
       srfi-30 SRFI-30)
    ;; provide feature id's like `os.linux' and `os.big-endian'
    ;; taken from the compiled-in list of features returned by
    ;; (os-type)
    (list->vector (map (lambda (s)
			 (symbol-append "os." s))
		       (os-type))))))

(define (remove-globally-implemented! (fid <symbol>))
  (set! *globally-implemented* (vdelq fid *globally-implemented*))
  (values))

(define (add-globally-implemented! (fid <symbol>))
  (set! *globally-implemented* 
	(vector-append (vector fid)
		       *globally-implemented*))
  (values))

;;;------------------------------------------------------------
;;;  note -- this does not work as intended in the offline 
;;;  compiler `rsc'...
;;;------------------------------------------------------------

(define (in-implements-list? m fid)
  (let* ((p (properties m))
	 (e (assq 'implements (if (vector? p)
				  (remainder->list p)
				  p))))
    (and e (memq fid (cdr e)) #t)))

;;; poke through the list of loaded modules, looking for an
;;; implementation by a module that's already loaded

(define (choose-loaded-implementation fid)
  (let loop ((m (installed-modules)))
    (if (null? m)
	#f
	(if (in-implements-list? (cdar m) fid)
	    (list (cdar m))
	    (loop (cdr m))))))

;;;  [hook to] dynamically load a module that allegedly
;;;  implements one of the features

(%early-once-only
(define choose-loadable-implementation 
  (lambda (fid) #f))
)

;;;
;;;  this is used by the FEATURE-ID evaluator to look
;;;  in places other than the currently available
;;;  feature set.
;;;
;;;  used by `if-implements' to turn OFF checking

(define-thread-var *implements-loaders*
  (list choose-loaded-implementation
	choose-loadable-implementation))

;;;
;;;  evaluate a feature expression, which is of the form:
;;;
;;;    FEATURE-EXPR ::= FEATURE-ID
;;;                  |  (or FEATURE-EXPR ...)
;;;                  |  (and FEATURE-EXPR ...)
;;;                  |  (not FEATURE-EXPR)
;;;                  |  (version OP VID ...)    OP one of <, <=, =, >=, >
;;;                  |  (available MODULE-NAME)
;;;
;;;  returns #f it the feature expression cannot be satisfied,
;;;  or a list of modules (possibly empty -- isn't it great that
;;;  #f != ()) required to satify it.

(define (eval-implements feature-expr envt)
  (cond
   ((symbol? feature-expr)
    (if (eq? feature-expr 'else) ; special-case `else'
	'() ;; success with no imports required
	(eval-implements/feature feature-expr envt)))
   ((pair? feature-expr)
    (case (car feature-expr)
      ((or)
       (let loop ((disj (cdr feature-expr)))
	 (if (pair? disj)
	     (or (eval-implements (car disj) envt)
		 (loop (cdr disj)))
	     #f)))
      ((and)
       (let loop ((conj (cdr feature-expr))
		  (r '()))
	 (if (pair? conj)
	     (let ((r0 (eval-implements (car conj) envt)))
	       (if r0
		   (loop (cdr conj) (append r r0))
		   #f))
	     r)))
      ((not)
       (if (eval-implements (cadr feature-expr) envt)
	   #f
	   '()))
      ((version)
       (let ((cmp (lexical-compare-list
		   *rscheme-version-list* 
		   (cddr feature-expr))))
	 (if (case (cadr feature-expr)
	       ((<=) (<= cmp 0))
	       ((<) (< cmp 0))
	       ((=) (= cmp 0))
	       ((>=) (>= cmp 0))
	       ((>) (> cmp 0))
	       (else #f))
	     '()
	     #f)))
      ((available)
       (every? get-module* (cdr feature-expr)))
      (else 
       (error/syntax "~s: unrecognized feature-expr operator in ~s" 
		     (car feature-expr)
		     feature-expr))))
   (else 
    (error/syntax "~s: unrecognized feature-expr form" feature-expr))))

(define (lexical-compare-list v1 v2)
  (cond
   ((and (null? v1) (null? v2))
    0)
   ((null? v1)
    -1)
   ((null? v2)
    1)
   ((= (car v1) (car v2))
    (lexical-compare-list (cdr v1) (cdr v2)))
   ((< (car v1) (car v2))
    -1)
   (else
    1)))


(define (eval-implements/feature feature-id envt)
  (if (vmemq feature-id *globally-implemented*)
      '() ; global -- nothing to import
      (if envt
	  (if (in-implements-list? (owner (the-top-level envt)) feature-id)
	      '() ; already imported
	      (let loop ((alt *implements-loaders*))
		(if (null? alt)
		    #f
		    (or ((car alt) feature-id)
			(loop (cdr alt))))))
	  #f)))

(define (compile/if-implements sf form lex-envt dyn-envt mode)
  (if (memq (length form) '(3 4))
      (let ((fid (cadr form)))
	(compile
	 ;; turn off loading of 
	 (thread-let ((*implements-loaders* '()))
	   (if (eval-implements fid lex-envt)
	       (caddr form)
	       (if (null? (cdddr form))
		   '(begin)
		   (cadddr form))))
	 lex-envt
	 dyn-envt
	 mode)) ; preserve mode (e.g., top-levelness)
      (error/syntax "if-implements: expected 2 or 3 subforms (not ~d)"
		    (- (length form) 1))))

;;;----------------------------------------------------------------------
;;;  An implementation of the `code-implements' alternative
;;;

(define (make-export-implementation-bdg (into <top-level-contour>) from-ms)
  (make <special-form>
	name: 'export-implementation
	compiler-proc: (lambda (sf form lxe dyne mode)
			 (for-each
			  (lambda (from)
			    (use-module-in (name from) from into))
			  from-ms))
	compiler-description: 'export-implementation))

;;;

(define (push-withs ms lex-envt dyn-envt)
  (if (null? ms)
      (values lex-envt dyn-envt)
      (let ((e (make <with-envt>
		     table: (module-exports (car ms))
		     lexical-enclosing: lex-envt
		     dynamic-enclosing: dyn-envt)))
	(push-withs (cdr ms) e e))))

(define (compile/cond-expand sf form lex-envt dyn-envt mode)
  (let loop ((c (cdr form)))
    (if (null? c)
        (error/syntax "No feature expressions satisfied: ~#*@30s"
                      (map car (cdr form)))
	(let* ((test (caar c))
	       (body (cdar c))
	       (ms (if (eq? test 'else)
                       '()
                       (eval-implements test lex-envt))))
	  (if ms
              ;; note that if the relevant feature(s)
              ;; are already imported (the only scenario
              ;; supported by SRFI-0), we don't introduce new
              ;; (module-exported) bindings and hance are SRFI-0
              ;; compatible
	      (bind ((lxe dye (push-withs ms lex-envt dyn-envt))
		     (xi (make-export-implementation-bdg
			  (the-top-level lex-envt)
			  ms))
		     (e (make-lexical-envt (list xi) lxe dye)))
		(compile/body body e e mode)) ; preserve mode
	      ;; nothing in this clause implementable -- try the next
	      (loop (cdr c)))))))
