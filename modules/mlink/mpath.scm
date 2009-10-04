#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/mlink/mpath.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.13
 | File mod date:    2003-06-22 18:15:04
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  mlink
 |
 `------------------------------------------------------------------------|#

(define-class <module-not-found> (<condition>)
  (name type: <symbol>)
  (search-path type: <list>))

(define-thread-var *module-search-path* #f)

(set! *module-search-path* #f)  ;; reset to #f on startup

(%early-once-only
(define *default-module-search-path*
  (map string->dir '("~/lib/rs/modules" "[resource]/modules"))))

(define (init-module-search-path)
  (let ((p (getenv "RS_MODULE_PATH")))
    (if p
        (apply append
	       (map (lambda (dirname)
		      (if (or (string=? dirname "@STDPATH@")
			      ;; @STDPATH@ is too hard to remember...
			      (string=? dirname "@")) 
			  *default-module-search-path*
			  (list (string->dir dirname))))
		    (string-split p #\:)))
	*default-module-search-path*)))

(define (module-search-path)
  (or *module-search-path*
      (let ((p (init-module-search-path)))
       (set! *module-search-path* p)
       p)))

(define-method push-module-search-path! ((p <directory-name>))
  (let ((d (append-dirs
	    (append-dirs (process-directory)
			 (current-directory))
	    p)))
    (set! *module-search-path* (cons d (module-search-path)))))

(define-method push-module-search-path! ((p <string>))
  (push-module-search-path! (string->dir p)))

;;;;

(define (get-loaded-module name #optional scan-only?)
  (let ((m (assq name (installed-modules))))
    (if m
        (cdr m)
        #f)))

;;;;

;; a restricted case of `remove-specials' from rsc
;; because module names with #\.'s in them get #\_ in
;; the filesystem

(define (remove-specials name)
  (list->string
   (map (lambda (ch)
	  (if (eq? ch #\.)
	      #\_
	      ch))
	(string->list name))))

(define (load-compiled-module name scan-only?)
  (let* ((rel (make <file-name>
		    file-directory: #f
		    filename: (remove-specials (symbol->string name))
		    extension: "mif"))
	 (f (search-for-file rel (module-search-path) '())))
    (if f
        (if scan-only?
            f
            (link-load-module name f))
        #f)))

;;;;

(define *module-finders* '())

;;;  The `proc' is a procedure of two arguments,
;;;
;;;    - the <symbol> name of the module to be loaded
;;;    - a boolean flag, scan-only?, which if true
;;;      means that instead of actually loading the module,
;;;      only an indication of where the file would be loaded
;;;      from should be returned
;; 

(define (add-module-finder! proc)
  (set! *module-finders* (append *module-finders* (list proc))))

(%early-once-only
  (add-module-finder! get-loaded-module)
  (add-module-finder! load-compiled-module))

;;;

(define (get-module (n <symbol>))
  (or (get-module* n)
      (signal (make <module-not-found>
                    name: n
                    search-path: (module-search-path)))))

(define-method write-object ((self <module-not-found>) port)
  (format port "#[<module-not-found> ~s]" (name self)))

(define-method display-object ((self <module-not-found>) port)
  (format port "*** No module with name `~s' could be found\n"
          (name self))
  ;;
  ;;  Print out the value of the environment variable, if it is set,
  ;;  or tell them that it isn't.
  ;;
  (let ((env (getenv "RS_MODULE_PATH")))
    (format port "    The RS_MODULE_PATH environment variable is~a set\n"
            (if env "" " not"))
    (if env (format port "    to: ~s\n" env)))
  ;;
  ;;  Describe the current (captured at the time of error, actually) 
  ;;  search path
  ;;
  (format port "    There are ~d entries in the module search path:\n"
          (length (search-path self)))
  ;;
  (let ((t (make-string-table))
        (nmissing 0))
    (for-each (lambda (i p)
                (let ((fqn (pathname->os-path p)))
                  (format port "       (~d) ~a\n" (+ i 1) fqn)
                  (table-insert! t fqn #t)))
              (range (length (search-path self)))
              (search-path self))
    ;;
    ;;  Tell them which of the standard search directories are not
    ;;  in the current search path
    ;;
    (for-each (lambda (p)
                (if (not (table-lookup t (pathname->os-path p)))
                    (begin
                      (if (= nmissing 0)
                          (format port "\n    Note that the following standard (default) entries are missing:\n"))
                      (set! nmissing (+ nmissing 1))
                      (format port "      (~d) ~a\n" 
                              nmissing
                              (pathname->os-path p))
                      (thread-let ((*module-search-path* (list p)))
                        (let ((seek (get-module* (name self) #t)))                          (if seek
                              (format port "          (which contains ~a)\n" 
                                      (if (instance? seek <file-name>)
                                          (file-relative-to-dir seek p)
                                          seek))))))))
              *default-module-search-path*)
    (format port "***\n")))
  

(define (get-module* (name <symbol>) #optional scan-only?)
  (let loop ((mf *module-finders*))
    (if (null? mf)
        #f
        (let ((m ((car mf) name scan-only?)))
	  (if m
	      m
	      (loop (cdr mf)))))))
