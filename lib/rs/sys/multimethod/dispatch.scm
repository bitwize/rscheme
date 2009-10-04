;;;
;;;   2. Multiargument-dispatch Generic Functions
;;;

(define-class <multimethod-generic> (<generic-function>))

(define *mm-dispatcher* (clone standalone-template))

(define-method install-next-method-syntax ((self <multimethod-generic>) form)
  ; next-method not supported yet...
  form)

(define (make-mm-generic-function gf-name)
  (make <multimethod-generic>
	template: *mm-dispatcher*
	generic-function-methods: '() 
	function-specializers: <object>
	generic-function-name: gf-name))

(define (mm-dispatch (gf <multimethod-generic>) args)
  (let loop ((method-list (generic-function-methods gf)))
    (if (pair? method-list)
	(if (method-accepts? (car method-list) args)
	    (apply (car method-list) args)
	    (loop (cdr method-list)))
	(error "~s: does not understand arguments ~s" 
	       (generic-function-name gf) 
	       args))))

(begin 
  (gvec-set! *mm-dispatcher* 3 mm-dispatch) 
  (values))

(define-macro (define-mm-generic-function name)
  `(define-constant ,name (make-mm-generic-function ',name)))
