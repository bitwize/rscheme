;;

(define-syntax (html . things)
  (with-output-to-port
      (make <html-output-port>
	    underlying-output-port: (current-output-port))
    (lambda ()
      (output-port-control (current-output-port) 'html #t)
      (begin . things)
      (output-port-control (current-output-port) 'html #f)))
  (newline)
  'text/html)

(define-syntax collect-kwds
  (syntax-form ((key :: predicate keyword?) value . items)
    (cons (list (keyword->symbol key) value)
	  (collect-kwds . items)))
  (syntax-form else '()))
    
(define-syntax collect-body
  (syntax-form ((key :: predicate keyword?) value . items)
    (collect-body . items))
  (syntax-form body (lambda () (begin . body))))

(define (start-tag tag attrs)
  (format-html "<~a" tag)
  (let loop ((a attrs))
    (if (pair? a)
	(begin
	  (if (pair? (cdar a))
	      (let ((key (caar a))
		    (val (cadar a)))
		(format-html " ~a=" key)
		   (if (and (pair? val)
			    (eq? (car val) 'quote))
		       (format-html "\"~a\"" (cadr val))
		       (format-html "~a" val)))
	      (let ((key (caar a)))
		(format-html " ~a" key)))
	  (loop (cdr a)))
	(format-html ">"))))

(define (end-tag tag)
  (format-html "</~a>" tag))

;; attrs is of the form:
;;
;;   ((key value) ...)
;;
;; e.g.,
;;   ((href "http://www.rscheme.org/"))

(define (with-tag tag attrs body)
  (start-tag tag attrs)
  (body)
  (end-tag tag))


;;

(define-syntax (unnumbered-list . items)
  (output-port-control (current-output-port) 'unnumbered-list #t)
  (begin . items)
  (output-port-control (current-output-port) 'unnumbered-list #f)
  (newline))

(define-syntax (numbered-list . items)
  (output-port-control (current-output-port) 'numbered-list #t)
  (begin . items)
  (output-port-control (current-output-port) 'numbered-list #f)
  (newline))

(define-syntax (header-1 . items)
  (output-port-control (current-output-port) 'header-1 #t)
  (begin . items)
  (output-port-control (current-output-port) 'header-1 #f)
  (newline))

(define-syntax (header-2 . items)
  (output-port-control (current-output-port) 'header-2 #t)
  (begin . items)
  (output-port-control (current-output-port) 'header-2 #f)
  (newline))

(define-syntax (header-3 . items)
  (output-port-control (current-output-port) 'header-3 #t)
  (begin . items)
  (output-port-control (current-output-port) 'header-3 #f)
  (newline))

(define-syntax (header-4 . items)
  (output-port-control (current-output-port) 'header-4 #t)
  (begin . items)
  (output-port-control (current-output-port) 'header-4 #f)
  (newline))

(define-syntax (title . items)
  (output-port-control (current-output-port) 'title #t)
  (begin . items)
  (output-port-control (current-output-port) 'title #f)
  (newline))

(define-syntax (bold . items)
  (output-port-control (current-output-port) 'bold #t)
  (begin . items)
  (output-port-control (current-output-port) 'bold #f))

(define-syntax (code . items)
  (output-port-control (current-output-port) 'code #t)
  (begin . items)
  (output-port-control (current-output-port) 'code #f))

(define-syntax (preformatted . items)
  (output-port-control (current-output-port) 'preformatted #t)
  (begin . items)
  (output-port-control (current-output-port) 'preformatted #f))

(define-syntax (list-item . items)
  (output-port-control (current-output-port) 'list-item #t)
  (begin . items)
  (newline))

(define-syntax (html-header . things)
  (output-port-control (current-output-port) 'header #t)
  (begin . things)
  (output-port-control (current-output-port) 'header #f))

(define (par)
  (output-port-control (current-output-port) 'paragraph #t)
  (newline))

(define-syntax (para . things) ;; para is DocBook's term
  (output-port-control (current-output-port) 'paragraph #t)
  (begin . things)
  (output-port-control (current-output-port) 'paragraph #f)
  (newline))

(define (nl)
  (output-port-control (current-output-port) 'line-break #t)
  (newline))

(define (horz-rule)
  (output-port-control (current-output-port) 'horz-rule #t)
  (newline))

(define-syntax (hyperlink (place) . things)
  (output-port-control (current-output-port)
		       'ref
		       place)
  (begin . things)
  (output-port-control (current-output-port)
		       'ref
		       #f))

(define-syntax (input-form (method action) . body)
  (output-port-control (current-output-port) 'input-form (list (mquote method)
							 action))
  (begin . body)
  (output-port-control (current-output-port) 'input-form #f))

(define-syntax input-submit
  (syntax-form (label)
    (with-tag "INPUT" `((TYPE submit) (VALUE ',label)) (lambda ()))))

(define-syntax input-reset
  (syntax-form (label)
    (with-tag "INPUT" `((TYPE reset) (VALUE ',label)) (lambda ()))))

(define-syntax input-hidden
  (syntax-form (label . opts)
    (with-tag "INPUT" `((TYPE hidden) 
			(NAME ',label)
			,@(collect-kwds . opts))
	      (lambda ()))))

(define-syntax input-area
  (syntax-form (name . opts)
    (with-tag "TEXTAREA" 
	      (cons `(name ',name)
		    (collect-kwds . opts))
	      (collect-body . opts))))

(define-syntax input-text ;; replaces functionality of `input-field'
  (syntax-form (name . opts)
    (with-tag "INPUT"
	      `((TYPE text) (NAME ',name) ,@(collect-kwds . opts))
	      (collect-body . opts))))

(define-syntax input-field
  (syntax-form (name length)
    (output-port-control (current-output-port)
			 'input-field
			 (list (mquote name) 'text length: length)))
  (syntax-form (name length dflt)
    (output-port-control (current-output-port)
			 'input-field
			 (list (mquote name) 'text length: length default: dflt))))

(define-syntax input-select
  (syntax-form ((name) . content)
    (output-port-control (current-output-port) 'select (list (mquote name)))
    (begin . content)
    (output-port-control (current-output-port) 'select #f))
  (syntax-form ((name 'length: n) . content)
    (output-port-control (current-output-port) 'select (list (mquote name) n))
    (begin . content)
    (output-port-control (current-output-port) 'select #f)))

(define-syntax select-option
  (syntax-form ()
    (output-port-control (current-output-port) 'option '(#f #f)))
  (syntax-form (':selected)
    (output-port-control (current-output-port) 'option '(#f #t)))
  (syntax-form (':selected val)
    (output-port-control (current-output-port) 'option (list val #t)))
  (syntax-form (val)
    (output-port-control (current-output-port) 'option (list val #f))))
  
(define-syntax (html-body . items)
  (with-tag "body" (collect-kwds . items) (collect-body . items)))

(define-syntax (html-table . items)
  (with-tag "table" (collect-kwds . items) (collect-body . items)))

(define-syntax (html-table-row . items)
  (with-tag "tr" (collect-kwds . items) (collect-body . items)))

(define-syntax (html-table-data . items)
  (with-tag "td" (collect-kwds . items) (collect-body . items)))

(define-syntax (html-font . items)
  (with-tag "font" (collect-kwds . items) (collect-body . items)))

;;;

(define-syntax (emph . items)
  (with-tag
   "em" '()
   (lambda ()
     (begin . items))))

