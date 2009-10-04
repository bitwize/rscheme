#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/repl/load.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.13
 | File mod date:    2004-07-02 08:04:35
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  repl
 |
 | Purpose:          Evaluate the contents of a file in an environment
 |------------------------------------------------------------------------|
 | Notes:
 |      In order to support script-mode, the scanner interprets `#!'
 |      as a to-end-of-line-comment like ';'
 `------------------------------------------------------------------------|#

(define-thread-var *load-path* (list $dot-dir))

(define (load-into envt . args)
  (for-each (lambda (f)
	      (load-1 f envt))
	    args))

(define (canonicalize-path path)
  (cond 
   ((string? path)
    path)
   ((symbol? path)
    (symbol->string path))
   ((list? path)
    (string-join #\/ (map to-string path)))
   (else
    (error "Illegal path form: ~s" path))))

(define (null-load-hook filename envt)
  filename)

(define load-hook null-load-hook)  ; keep it a non-constant

(define (set-load-hook! proc)
  (set! load-hook (or proc null-load-hook)))

(define (locate-for-load path-form)
  (if (instance? path-form <file-name>)
      path-form
      (let* ((canon (string->file (canonicalize-path path-form)))
	     (path (search-for-file canon *load-path* '(#f "scm"))))
	(if path
	    path
	    (error "load: ~a not found" canon)))))

(define (load-1 path-form envt)
  (let* ((path (locate-for-load path-form))
         (hooked-path (load-hook path envt))
         (rpath (relative-file hooked-path)))
    (call-with-input-file
	(pathname->os-path hooked-path)
      (lambda (port)
	(within-directory
	 (file-directory path)
	 (lambda ()
	   (fluid-let ((*source-point* (vector 'file path #f #f #f)))
	     (if *compile-verbose*
		 (format #t ">> loading: ~a\n" rpath))
	     (let loop ((last '()))
	       (bind ((loctab (make-object-table))
                      (item line (input-port-read port location-table: loctab)))
		 (if (eof-object? item)
		     (begin
		       (if *compile-verbose*
			   (format #t "<< done loading: ~a\n" path))
		       (list->values last))
		     (begin
		       (if *compile-verbose*
			   (format #t "~a:~d: ~#*@30s\n" 
				   (name port)
				   line 
				   item))
		       (set! *source-point* (vector 'file path line item loctab))
		       (loop (values->list (eval-in-envt item envt))))))))))))))

