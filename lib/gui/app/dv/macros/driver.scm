;;;
;;;  A driver program for running macros
;;;

,(use paths)
,(use repl)
,(use rs.util.msgs)
,(use rs.sys.tables)
,(use gui.app.dv)
,(use graphics.geometry)
,(use rs.util.properties)

(define *dv-envt*
  (with-module mlink
    (top-level-envt (get-module 'gui.app.dv))))


(define *extension-map*
  '(("emap" event-map)
    ("ofig" object-fig)))

(define *macro-map*
  '((event-map "eventmap.scm" read-event-map)
    (object-fig "loadofig.scm" read-object-fig)
    (object-interaction "object-interaction.scm" read-dv-macro)
    (object-interaction-2 "object-interaction-2.scm" read-dv-macro)))

(define *have-loaded* '())

,(use regex)

(define magic-pattern (reg-expr->proc '(seq "-<"
					    (save (+ (or alpha
							 digit
							 #\_
							 #\-)))
					    ">")))

(define (get-file-type src)
  (let* ((f (open-input-file src))
	 (l (read-line f)))
    (close-input-port f)
    (bind ((s e type (magic-pattern l)))
      (if s
	  (string->symbol type)
	  (let ((t (assoc (extension (string->file src)) *extension-map*)))
	    (if t
		(cadr t)
		(error "cannot determine file type of: ~a" src)))))))

(define *macros-dir* (current-absolute-directory))

(define (setup-for-type src)
  (let ((t (get-file-type src)))
    (let ((ld (assq t *macro-map*)))
      (if (not ld)
	  (error "cannot figure out how to set up `~s' for: ~a" t src))
      (if (not (member (cadr ld) *have-loaded*))
	  (begin
	    (within-directory
	     *macros-dir*
	     (lambda ()
               ;; wouldn't we want each macro to have its own copy
               ;; of a *dv-envt*, instead of all sharing?
	       (load-into *dv-envt* (cadr ld))))
	    (set! *have-loaded* (cons (cadr ld) *have-loaded*))))
      (caddr ld))))

(define (build-doc src)
  (let ((fn (eval (setup-for-type src)  ; returns reader symbol
		  *dv-envt*)))
    (call-with-input-file 
	src
      (lambda (port)
	(fn port)))))

(define (process-doc src)
  (bind ((doc (build-doc src))
	 (view (car (document-views doc)))
	 (page (view-page view))
	 (bbox (bounding-box (page-contents page))))
    ;
    (set-property! doc 'eps #t)
    (set-property! page 'page-bbox bbox)
    (set-page-size! page (size+ (size bbox) (make-size 36 36)))
    ;
    (print-page page
		(pathname->string
		 (extension-related-path (string->file src) "eps")))
    (set-view-frame! view (make-rect2
			   (make-point 50 50)
			   (page-size (view-page view))))
    (reconfig-to-fit-window view)
    doc))

(define (main args)
  (let ((open-them #f))
    (let loop ((a args)
	       (d '()))
      (if (null? a)
	  (if open-them
	      (with-client
	       (make-client (getenv "DISPLAY"))
	       (lambda ()
		 (for-each open-document d)
		 (with-module repl
		   (cmd-loop *self* "dvo[~d]=>"))))
	      (values))
	  (if (string=? (car a) "-i")
	      (begin
		(set! open-them #t)
		(loop (cdr a)))
	      (loop (cdr a) (cons (process-doc (car a)) d)))))))
