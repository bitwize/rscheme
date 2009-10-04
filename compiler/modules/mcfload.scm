#|------------------------------------------------------------*-Scheme-*--|
 | File:    compiler/modules/mcfload.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.31
 | File mod date:    2005-04-08 17:43:36
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  (rsc)
 |
 `------------------------------------------------------------------------|#

(define-syntax (dbg . b) (values))  ;; lots of stuff about IBs
;(define-syntax (dbg . b) (begin . b))

(define-fluid *in-early-once-only?* #f)
(define-fluid *source-file*)

;; Load an .mcf file
;; (this amounts to doing a build on a module)

(define (mcf-load file)
  (let* ((actual-file (append-path (current-directory)
				   (string->file file)))
	 (form (with-input-from-file (pathname->string actual-file) read)))
    (if (and (pair? form)
	     (eq? (car form) 'define-module))
	(interp-define-module-form form actual-file)
	(interp-mcf-form form actual-file))))

;;
;; load a `define-module' file
;;  this is like loading a .mcf file, but the file contains
;;  a `define-module' form instead of MCF syntax module definition
;;

(define (interp-define-module-form form actual-file)
  (let* ((esc (caddr form))
	 (cont (cdddr form))
	 (name (cadr form)))
    (zz-interp-module name
		      (if (pair? esc) (car esc) '&module)
		      actual-file
		      (or *target-dest-dir* (file-directory actual-file))
		      (or *target-image-dir* (file-directory actual-file))
		      cont
		      interp-scm)))

(define (interp-mcf-form form (actual-file <file-name>))
  (zz-interp-module (caar form)
		    '&module
		    actual-file
		    (or *target-dest-dir*
                        (string->dist-dir
                         (file-directory actual-file)
                         (cadar form)))
		    (or *target-image-dir*
                        (string->dist-dir
                         (file-directory actual-file)
                         (caddar form)))
		    (list (cons '&module (cdr form)))
		    interp-mcf))

(define (zz-interp-module m-name
			  esc-name
			  actual-file
			  (m-dest-dir <directory-name>)
			  (m-image-dir <directory-name>)
			  (content-forms <list>)
			  esc-interp-proc)
  (let* ((envt (make-top-level-contour))
	 (m (make <module> 
		  name: m-name
		  properties: (list (cons 'directory 
					  (pathname->os-path 
					   (dir-from-to
					    *dist-path*
					    m-dest-dir))))
		  module-exports: (make-table eq? symbol->hash)
		  link-names: (list (remove-specials 
				     (symbol->string m-name)))
		  top-level-envt: envt))
	 (bcx (make <build-context>
		    name: m-name
		    dest-dir: m-dest-dir
		    image-dest-dir: m-image-dir
		    building: m)))
    (set-owner! envt m)
    (set! *current-module* m)
    (set! *build-context* bcx)
    (set-backing! (top-level-envt m) m)
    (for-each interp-pragma *initial-pragmas*)
    (fluid-let ((*source-dir* (file-directory actual-file)))
      (compile-with-ad-hoc-sf 
       content-forms
       esc-name
       (lambda (subforms lxe dye mode)
	 (for-each esc-interp-proc subforms))
       #t ;; required at top
       envt
       envt
       'top))
    (make-init-procs bcx)
    (warn-about-unbound (top-level-envt (building bcx)))
    bcx))


;;
;; these will be part of the *scheme* module
;;

(define (make-top-level-forms)
  (let-syntax ((sf (syntax-form (n)
		     (make <special-form>
			   name: (mquote n)
			   compiler-proc: #f
			   compiler-description: (mquote n))))
	       (def (syntax-form (n)
		      (make <definer>
			    name: (mquote n)
			    compiler-proc: #f
			    compiler-description: (mquote n)))))
    (list
     (def define)
     (def define-constant)
     (def define-syntax)
     (def define-full-bdg)
     (def define-rewriter)
     (def define-primop)
     (def define-glue)
     (def %early-once-only)
     (sf %strategy)
     ; these definers are present but disabled [for now]
     ; -- they are present so that we can build `rs.lang'
     ; in both the offline and online compiler.  When linked
     ; online, these definitions will be enabled
     (def define-module)
     (def define-module-extend))))

(define (compiler-save-module (path <string>) (root <module>))
  (set-image-mode! <symbol> 2)
  (save-module path root))

(define (write-module (bcx <build-context>))
  (let ((module-name (name bcx)))
    (if (needs-c-context bcx)
	(begin
          (if *tl-report*
              (format #t "building C support files for ~j...\n" (c-files bcx)))
	  (build-support-files bcx))
	(begin
          (if *tl-report*
              (display "no C code -- support files not built\n"))
	  (set-link-names! (building bcx) '())))
    (let (((m <module>) (building bcx)))
      ;; write out this module
      ;;too verbose(display "detaching module from linked-to modules...\n")
      (strip-module! m)
      (let ((f (make <file-name>
		     filename: (base-filename bcx)
		     extension: "mif"
		     file-directory: (image-dest-dir bcx))))
	(let ((fn (pathname->os-path f)))
	  (if (> (string-length fn) 63)
	      (format #t "saving mif: ~a...~a\n" 
		      (substring fn 0 30)
		      (substring fn (- (string-length fn) 30)))
	      (format #t "saving mif: ~a\n" fn)))
	(compiler-save-module (pathname->string f) m))
      ;; write out something to associate the real name
      ;; of this module with it's linkage name, along with
      ;; some other handy info
      (write-mx-file (make <file-name>
			   filename: (base-filename bcx)
			   extension: "mx"
			   file-directory: (image-dest-dir bcx))
		     module-name
		     (base-filename bcx)
		     (link-names m)
		     (module-bytecode-extensions m)
		     (map name (module-imports m))
		     (other-libs bcx))
      ;;
      (write-documentation bcx)
      ;;
      bcx)))

;; include any additional libraries that will be needed

(define (include-other-libs new-libs)
  (let ((old-libs (other-libs *build-context*)))
    (for-each
     (lambda (l)
       (if (not (member l old-libs))
	   (set! old-libs (append old-libs (list l)))))
     new-libs)
    (set-other-libs! *build-context* old-libs)))


(define (write-mx-file file 
		       module-name
		       base-filename
		       unit-names
		       bytecode-extensions
		       prereq-modules
		       other-libs)
  (call-with-output-file 
      (pathname->string file)
    (lambda (port)
      (format port ";mx2 -- RScheme Module indeX (MX) file  -*-Scheme-*-\n")
      (write (list module-name
		   base-filename
		   unit-names
		   bytecode-extensions
		   prereq-modules
		   other-libs)
	     port)
      (newline port))))

;; 
;; look for unbound variables
;;
(define (warn-about-unbound envt)
  (warn-about-unbound-vars-created)
  (table-for-each
   (table envt)
   (lambda (h k v)
     (if (and (instance? v <top-level-var>)
	      (eq? (value v) '#unbound))
	 (format #t "warning: `~s' is unbound\n" k)))))

;; once I figure out how to do it,
;; also removes bindings for things that are apparently unused

#|  The problem is: how can you tell when something is unused...
    currently, we eagerly bind into the TLE everything possible
    from an imported module.  Hence, pointers to these bindings
    occur in both the <imported-module>'s <link-bdgs> link command
    and in the TLE itself.  So, a <patch> which is used nowhere
    else will be referred to twice, once from each of those
    tables.  It's not clear how to distinguish this kind of
    practical non-use from similar cases of actual use

    one solution is to use weak pointers.  If both the TLE and
    the link-bdgs hold only weak pointers to the patches, then
    it will get dropped automatically unless a program object
    refers to it.  

    However, note that at any time that the TLE is reified
    (ie, during compilation), everything that could possibly
    be in the TLE needs to be retained, so this problem is
    probably really a non-problem
         dmk 94.12.06
|#

(define (strip-module! (m <module>))
  (let ((all-link-bdgss (get-and-detach-imported-bindings m)))
    ;;
    ;; delete all imported bindings from our own top-level environment;
    ;; we can do this because if we try to evaluate something there later,
    ;; it will get resolved (again) at that point
    ;; (actually, we do this by copying the top-level-envt table,
    ;; thereby compacting it)
    ;;
    (let ((new-tlt (make-symbol-table)))
      (table-for-each
       (table (top-level-envt m))
       (lambda (h k v)
	 (if (not (instance? v <imported-binding>))
	     (begin
	       ;(format #t "  keeping: ~s\n" v)
	       (table-insert! new-tlt k v)))))
      (if *tl-report*
          (format #t "stripped local environment table: ~d / ~d bindings\n"
                  (table-size new-tlt)
                  (table-size (table (top-level-envt m)))))
      (set-table! (top-level-envt m) new-tlt)
      ;;
      (install-patch-back-pointers m)
      ;;
    ;;
    (for-each decouple-unused-patchs all-link-bdgss)
      m)))

(define (decouple-unused-patchs info)
  (let ((unused '())
	(mname (car info))
	(tbl (imported-bindings (cdr info))))
    (table-for-each
     tbl
     (lambda (h k (ib <imported-binding>))
       (if (eq? (vector-length (places ib)) 0)
	   (set! unused (cons k unused)))))
    ;; turns out to be zero usually for now...
    ;;(format #t "   ~s: deleting ~d unused out of ~d bindings\n"
    ;;	    mname
    ;;	    (length unused)
    ;;	    (table-size tbl))
    (for-each
     (lambda (k)
       (table-remove! tbl k))
     unused)))

(define (get-and-detach-imported-bindings (m <module>))
  (let ((lst '()))
    (for-each
     (lambda ((im <imported-module>))
       ;; the only imported bindings should be in the
       ;; first link command, if anywhere
       (set-actual-module! im #f)
       (if (and (not (null? (link-commands im)))
		(instance? (car (link-commands im)) <link-bdgs>))
	   (let* ((lc (car (link-commands im)))
		  (tbl (imported-bindings lc)))
             (if *tl-report*
                 (format #t "  ~s: detaching ~d imported bindings\n"
                         (name im)
                         (table-size tbl)))
	     (set! lst (cons (cons (name im) lc) lst))
	     ;; go through and detach all pointers to imported bindings
	     (table-for-each
	      tbl
	      (lambda (h k (ib <imported-binding>))
		(set-remote-binding! ib #f))))))
     (module-imports m))
    lst))
    
;; cause the current module to use another module
;; use-form is one of:
;;	SYMBOL
;;	(SYMBOL [prefix: str] [all] import-form...)
;;	  where import-form is either SYMBOL or (SYMBOL => SYMBOL)
;;	  (the latter indicating a renaming)
;; the latter form not being implemented at the moment

(define (open-module open-form)
  (use-module (list 'use open-form)))

(define (use-module use-form)
  (if (or (not (pair? (cdr use-form)))
	  (not (list? (cdr use-form))))
      (error/syntax "module `use' form invalid\n==> ~s" use-form))
  ;;
  (if (not (symbol? (cadr use-form)))
      (error/syntax "module use target `~s' not a symbol" (cadr use-form)))
  ;;
  ;; process the "usual-inlines" virtual module definition
  ;; (it is an actual module created by REPL initialization)
  (if (eq? (cadr use-form) 'usual-inlines)
      (interp-mcf '(open *scheme* 
                         primops
                         corelib
                         iolib
                         low-scheme
                         high-scheme
                         mathlib
                         objsys))
      (let* ((im (add-dependent-module (cadr use-form) 'front)))
        ;;need to reimplement this...
        ;;(name-map (build-use-name-map m (cddr use-form)))
        (values))))

;;;
;;;  add a dependent module
;;;  (in theory, this function should not change the bindings of
;;;  of the current module, though the current implementation does)
;;;
;;;  returns the new <imported-module>

(define (add-dependent-module module-name where) ; where in (front back)
  (if *tl-report*
      (format #t "adding dependent module ~s (to ~s)\n" module-name where))
  (let ((im (make <imported-module> 
		  name: module-name
		  actual-module: (get-ct-module module-name)
		  owner: *current-module*)))
    ;; add the standard <link-command>, the first one for the module,
    ;; which is the one that links imported-bindings to their corresponding
    ;; bindings
    (set-link-commands! im 
			(list
			 (make <link-bdgs>
			       owner: im
			       imported-bindings: (make-symbol-table))))
    ;;
    (set-module-imports! 
     *current-module*
     (case where
       ((front)
	(cons im (module-imports *current-module*)))
       ((back)
	(append (module-imports *current-module*) (list im)))))
    im))

;;;
;;;  the <build-context> is the "backing" for the top-level-contour
;;;  we're constructing.  It is asked to resolve any unrecognized
;;;  variables, which it does by looking for the name in the imported
;;;  modules
;;;

(define-method lookup ((self <module>) var)
  (let loop ((imports (module-imports self)))
    (if (null? imports)
	#f ;; couldn't find it
	(let ((b (lookup-from-imported-module (car imports) var self)))
	  (if b
	      (begin
		(table-insert! (table (top-level-envt self)) var b)
		b)
	      (loop (cdr imports)))))))

(define *need-zap* #t)
(define (zap)
  (if *need-zap*
      (let ((was (vector-map image-mode (mif-save-defs))))
	(set! *need-zap* #f)
	(vector-for-each
	 (lambda (c)
	   (set-image-mode! c 99))
	 (all-instances <<standard-class>>))
	(vector-for-each
	 (lambda (c w)
	   (set-image-mode! c w))
	 (mif-save-defs)
	 was))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Try to find a variable with the given name (`var') in 
;;;  the exported variables of imported module `self'.
;;;
;;;  The module `self' is imported into module `in-m'.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	    
(define (lookup-from-imported-module (self <imported-module>)
				     (var <symbol>)
				     (in-m <module>))
  (zap)
  (let (((m <module>) (actual-module self)))
    (let ((b (table-lookup (module-exports m) var)))
      (if b
	  (if (eq? in-m *current-module*)
	      ; we are still in the current module...
	      (let* (((equiv-im <imported-module>) self)
		     ((lbc <link-bdgs>) (car (link-commands equiv-im)))
		     (xib (table-lookup (imported-bindings lbc) var)))
		(if xib
		    ; there's already an IB for it...
		    xib
		    ; create a new IB
		    (let ((ib (make <imported-binding>
				    remote-binding: b
				    link-command: lbc
				    description: (list var (name m))
				    name: var
				    ib-writable?: (and 
						   (memq 
						    var
						    (module-permits-writing m))
						   #t)
				    ib-shared?: (and 
						 (memq var (module-shares m))
						 #t))))
		  (dbg
		   (format #t "importing ~s from ~s (in current module)\n"
			   var (name self))
		   (format #t "          ib `~s' => [0x~04x~04x]\n"
			   var
			   (obj-high-bits ib) (obj-low-bits ib)))
		  (table-insert! (imported-bindings lbc) var ib)
		  ib)))
	      ; create an imported binding for the current-module's
	      ; equivalent imported-module
	      (let* (((equiv-im <imported-module>) (module->imported-module m))
		     ((lbc <link-bdgs>) (car (link-commands equiv-im)))
		     (xib (table-lookup (imported-bindings lbc) var)))
		(if xib
		    ; there's already an IB for it...
		    xib
		    ; create a new IB
		    (let ((ib (make <imported-binding>
				    remote-binding: b
				    link-command: lbc
				    description: (list var (name m))
				    name: var
				    ib-writable?: (and 
						   (memq 
						    var
						    (module-permits-writing m))
						   #t)
				    ib-shared?: (and 
						 (memq var (module-shares m))
						 #t))))
		  (dbg
		   (format #t "importing ~s from ~s (in foreign module ~s)\n"
			   var (name self) (name in-m))
		   (format #t "          ib `~s' => [0x~04x~04x]\n"
			   var
			   (obj-high-bits ib) (obj-low-bits ib)))
		  (table-insert! (imported-bindings lbc) var ib)
		  ib))))
	  #f))))

(define (module->imported-module (self <module>))
  (let loop ((ims (module-imports *current-module*)))
    (if (null? ims)
	(begin
          (if *tl-report*
              (format #t "implicitly importing module: ~a\n" (name self)))
	  (add-dependent-module (name self) 'back))
	(if (eq? (actual-module (car ims)) self)
	    (car ims)
	    (loop (cdr ims))))))

(define (find-equivalent-imported-module (self <imported-module>))
  (module->imported-module (actual-module self)))

(define (insert-into-equivalent-imported-module 
	 (self <imported-module>)
	 (var-name <symbol>)
	 (imported-binding <imported-binding>))
  (let* ((im (find-equivalent-imported-module self))
	 (lbc (car (link-commands im))))
    (set-link-command! imported-binding lbc)
    (table-insert! (imported-bindings lbc)
		   var-name
		   imported-binding)
    imported-binding))

(define (select-exported for lst)
  (let ((x (module-exports *current-module*)))
    (select (lambda (n)
	      (if (table-lookup x n)
		  #t
		  (begin
		    (warning "~a `~s' not exported -- ignored" for n)
		    #f)))
	    lst)))

(define (interp-pragma p)
  (case p
    ((function-definitions-are-const)
     (set-fn-def-are-const?! *build-context* #t))
    ((slots-are-sealed)
     (set! *slots-sealed-by-default?* #t))
    (else
     (error "unknown pragma: ~s" p))))

(define (interp-scm scm)
  (cond
   ;; transform ",(use foo)" and ",(import foo)" into ",(open foo)"
   ((and (pair? scm)
	 (or (eq? (car scm) 'use)
	     (eq? (car scm) 'import)))
    (interp-mcf (cons 'open (cdr scm))))
   ;; otherwise, everything else is the same
   (else
    (interp-mcf scm))))

(define (process-inc-path form)
  (let ((b *build-context*))
    (let loop ((f form))
      (cond
       ((null? f)
        (values))
       ((or (not (pair? f))
            (not (pair? (cdr f)))
            (not (string? (cadr f))))
        (error "malformed require-c-include-dir at: ~s" f))
       (else
        (let ((suffix (list (cadr f))))
          (case (car f)
            ((system:)
             (set-system-inc-path! 
              b
              (append (system-inc-path b) suffix)))
            ((user:)
             (set-user-inc-path!
              b
              (append (user-inc-path b) suffix))))
          (loop (cddr f))))))))

(define (interp-mcf mcf)
  (if (pair? mcf)
      ;; it's a valid module control form
      (case (car mcf)
	((files load)
	 ;; files that are part of this module
	 ;--(mark-non-module-objects)
	 ;;
	 (for-each
	  participating-file
	  (cdr mcf)))
	((c-files)
	 ;; `.c' files that are part of this module
	 (for-each participating-c-file
		   (cdr mcf)))
	((other-files)
	 (for-each other-file (cdr mcf)))
	((pragma)
	 (for-each interp-pragma (cdr mcf)))
        ((require-c-include-dir)
         (process-inc-path (cdr mcf)))
	((h-files)
	 ;; `.h' files that are part of this module
	 (for-each participating-h-file
		   (cdr mcf)))
	((pub-h-files)
	 ;; `.h' files that are part of this module
	 ;; and are made public for other (C) code to see
	 (for-each participating-pub-h-file
		   (cdr mcf)))
	((require-c-header)
	 ;; a `.h' file that is required by the generated
	 ;; C code to be #include'd
	 (set-extern-h-files! 
	  *build-context*
	  (append (extern-h-files *build-context*)
		  (cdr mcf))))
	((dir)
	 (fluid-let ((*source-dir* (append-dirs (fluid-ref *source-dir*)
						(string->dir
						 (if (symbol? (cadr mcf))
						     (symbol->string
						      (cadr mcf))
						     (cadr mcf))))))
	   (for-each interp-mcf (cddr mcf))))
	((open)
	 ;; a shorthand use of another module
	 (for-each open-module (cdr mcf)))
	((use)
	 ;; a use of another module
	 (use-module mcf))
	((share)
	 ;; names given in a "share" decl are EXPORTED names
	 (set-module-shares! *current-module*
			     (append (module-shares *current-module*)
				     (select-exported 'share
						      (cdr mcf)))))
	((writable)
	 ;; names given in a "writable" decl are EXPORTED names
	 (set-module-permits-writing!
	  *current-module*
	  (append (module-permits-writing *current-module*)
		  (select-exported 'writable (cdr mcf)))))
	;; shorthand for share & writable
	((fluid)
	 (let ((x (select-exported 'fluid (cdr mcf))))
	   (set-module-permits-writing!
	    *current-module*
	    (append (module-permits-writing *current-module*) x))
	   (set-module-shares!
	    *current-module*
	    (append (module-shares *current-module*) x))))
	;;
	((c-module-roots)
	 (set-root-variable-info! *build-context* (cdr mcf)))
	;;
	((bytecode-extension)
	 (let ((extn-num (cadr mcf))
	       (extn-proc (caddr mcf)))
	   (if (not (integer? extn-num))
	       (error/syntax "bytecode extension number invalid: ~s" 
			     extn-num))
	   (if (not (string? extn-proc))
	       (error/syntax 
		"bytecode extension procedure ~s, expected string"
		extn-proc))
	   (set-module-bytecode-extensions!
	    *current-module*
	    (cons (list extn-num extn-proc)
		  (module-bytecode-extensions *current-module*)))))
	;;
	((export)
	 (let ((from (top-level-envt *current-module*)))
	   ;; export some stuff
	   (for-each
	    (lambda (export-spec)
	      ;; currently, only symbols are supported
	      ;; (should check to make sure it's bound!)
	      (let ((bdg (lookup from export-spec)))
		(if bdg
		    (table-insert! (module-exports *current-module*)
				   export-spec
				   bdg)
		    (error/syntax "export of unbound ~s illegal" 
				  export-spec))))
	    (if (eq? (cadr mcf) ':all)
		(table-keys->list (table from))
		(if (eq? (cadr mcf) ':local)
		    (let ((lst '()))
		      (table-for-each
		       (table from)
		       (lambda (h k v)
			 (if (not (instance? v <imported-binding>))
			     (set! lst (cons k lst)))))
		      lst)
		    (cdr mcf))))))
	(else
	 (format #t "Invalid module control form: ~s\n" mcf)))
      (format #t "Invalid module control form: ~s\n" mcf)))

;; try to find a source file

(define (locate-source file extn)
  (let* ((canon (if (symbol? file)
		    (symbol->string file)
		    (if (string? file)
			file
			(abort 'locate-source
			       "unknown form for filename `~s'"
			       file))))
	 (basic (string->file canon))
	 (path #f))
    (if (not (extension basic))
	(set! basic (make <file-name>
                          filename: (filename basic)
                          extension: extn
                          file-directory: (file-directory basic))))
    (set! path (append-path (fluid-ref *source-dir*) basic))
    ;;
    (if (file-exists? path)
        path
        (let ((alt (and *target-dest-dir*
                        (append-path *target-dest-dir* basic))))
          (if (and alt (file-exists? alt))
              (begin
                (if *tl-report*
                    (format #t "located alternate location for ~s: ~a\n" 
                            file alt))
                alt)
              (error "could not locate '.~a' source file: ~a\n" extn file))))))


(define (add-project-file filename)
  ;;warning is only valid if the suffix conflicts
  ;;(if (member filename (file-names *build-context*))
  ;;    (warning "File name `~a' already in project" filename))
  (set-file-names! *build-context*
		   (cons filename (file-names *build-context*))))

(define (other-file file)
  (if *tl-report*
      (format #t "Including other file: ~a\n" file))
  (let ((path (append-path (fluid-ref *source-dir*)
			   (string->file file))))
    (add-project-file (filename path))
    (copy-source-file path "")))

(define (include-other-c-files files)
  (for-each
   (lambda (f)
     (let ((fn (filename (string->file f))))
       (if (not (member fn (c-files *build-context*)))
	   (participating-c-file fn))))
   files))

(define (participating-c-file file)
  (if *tl-report*
      (format #t "Participating `.c' file: ~a\n" file))
  (let ((path (locate-source file "c")))
    (set-c-files! *build-context*
		  (append (c-files *build-context*)
			  (list (filename path))))
    (add-project-file (filename path))
    ;; copy the file
    (copy-source-file path)))

(define (participating-h-file file)
  (if *tl-report*
      (format #t "Participating `.h' file: ~a\n" file))
  (let ((path (locate-source file "h")))
    (set-h-files! *build-context*
		  (append (h-files *build-context*)
			  (list (filename path))))
    (add-project-file (filename path))
    (copy-source-file path)))

(define (participating-pub-h-file file)
  (if *tl-report*
      (format #t "Participating public `.h' file: ~a\n" file))
  (let ((path (locate-source file "h")))
    (set-public-h-files! *build-context*
			 (append (public-h-files *build-context*)
				 (list (filename path))))
    (add-project-file (filename path))
    (copy-source-file path)))

(define (participating-file file)
  (let ((tl-envt (top-level-envt *current-module*))
	(path (locate-source file "scm")))
    (if *tl-report*
        (begin
          (format #t "Participating scheme file: ~a\n" file)
          (format #t "  at ~a\n" path)))
    (participating-code
     path
     (lambda ()
       (fluid-let ((*source-file* path))
	 (with-objects-from-file
	  (pathname->os-path path)
	  (lambda (tl-expr)
	    (compile-tl-form tl-expr tl-envt tl-envt))))))))

;;
;;  `del-function-descr-tag!' deletes an entry from the
;;  function-info association list of a <template>
;;

(define (del-function-descr-tag! (t <template>) (tag <symbol>))
  (let ((a (assq tag (function-descr t))))
    (if a
	(set-function-descr! t (delq! a (function-descr t))))
    (function-descr t)))

;;  path is used to help name the resulting C file(s), if any.
;;  Usually, this is the name of the source file from
;;  which the code is derived, for ease of human legibility

(define (participating-code path do-gen-thunk)
  (let ((cds (make-seq)))
    (fluid-let ((*code-descriptors* cds))
      (do-gen-thunk))
    ;; generate code for each of the code descriptors
    (let ((ccds (make-seq)))
      (for-each (lambda ((cd <code-descriptor>))
		  (case (strategy cd)
		    ((ccode literal-c)
		     (seq-add! ccds cd))
		    ((bytecode)
		     (generate-bytecodes cd))
		    (else
		     (error/internal
		      "unrecognized strategy: ~s" (strategy cd)))))
		(seq->list cds))
      ;; actually spit out the c code
      ;; (we do this all at once, because the function
      ;;  may find it useful to have all the <c-d>'s up
      ;;  front)
      (write-code-descriptors path
			      (seq->list ccds))
      ;;
      ;; post-process the templates to delete any inlining info
      ;;
      (for-each (lambda ((cd <code-descriptor>))
		  (del-function-descr-tag! (template cd) 'inline))
		(seq->list cds)))))

(define-syntax (value-2nd expr) 
  (bind ((a b #rest r expr)) b))

;; fix up a <template> so that it will refer to the target system's
;; bytecode interpreter

(define *bci-part-descr* #f)
(on-startup (set! *bci-part-descr* #f))

(define (patch-bytecode-template! t)
  (set-code-pointer! t 0)  ;; function #
  (set-linkage-info! t (or *bci-part-descr*
			   (let ((p (make <part-descr>
					  module-name: "bci"
					  part-tag: 8902
					  linkage: #f)))
			     (set! *bci-part-descr* p)
			     p))))

(define *last-code* #f)

;;
;;  compile the AML stored in the code-descriptor into 
;;  byte codes and fill in the code pointer and linkage info
;;  slot of the code-descriptor's template
;;

(define (generate-bytecodes (cd <code-descriptor>))
  (set! *last-code* cd)
  (if (eq? (strategy cd) 'bytecode)
      (let (((t <template>) (template cd)))
	(patch-bytecode-template! t)
	(set-function-descr! t (code-properties cd))
	(gvec-set! t 3 (aml->byte-coded (code cd)))
	t)))

(define-method compile-head ((self <definer>) orig tl-expr tl-envt d-envt mode)
  (if (not (eq? mode 'top))
      (error/syntax "definer form `~s' not at top level" (name self)))
  (if (and (syntax-checker self)
	   (not ((syntax-checker self) tl-expr tl-envt)))
      (error/syntax "~a form doesn't pass syntax-checker" (name self))
      ((special-form-compiler self) tl-expr tl-envt d-envt)))

(define (compile-tl-define-module tl-def tl-envt dyn-envt)
  (error/semantic
   "`define-module' not supported by offline compiler\n   in this context"))

(define (compile-tl-define-module-extend tl-def tl-envt dyn-envt)
  (error/semantic
   "`define-module-extend' not supported by offline compiler"))

(define (top-level-compiler->proc description)
  (case description
    ;;
    ;; definers
    ;;
    ((define) compile-tl-define)
    ((define-constant) compile-tl-define-const)
    ((define-syntax) compile-tl-define-syntax)
    ((define-glue) compile-tl-define-glue)
    ((define-primop) compile-tl-define-primop)
    ((define-full-bdg) compile-tl-define-full-bdg)
    ((define-rewriter) compile-tl-define-rewriter)
    ;;
    ;;  other things that will override what's provided by the
    ;;  underlying compiler
    ;;
    ((%early-once-only) compile-early-once-only)
    ((well-known-function) compile-well-known-function)
    ;;
    ;;  even if that means its disabled here
    ((define-module) compile-tl-define-module)
    ((define-module-extend) compile-tl-define-module-extend)
    ;;
    ;;  and yet more things that only we understand...
    ;;
    ((%strategy) compile-strategy)
    ;;
    (else #f)))

(define (compile-tl-form tl-expr (tl-envt <top-level-contour>) dyn-envt)
  (if *tl-report*
      (format #t "  tl-form: ~#*@60s\n" tl-expr))
  (let ((tl-expr (preprocess-toplevel-form tl-expr tl-envt dyn-envt)))
    ;;
    ;; compile the form, with mode 'top
    ;; (this allows certain special forms to know when they're
    ;;  really at the top level; macros preserve top mode)
    ;; all special top-level things return #f instead of 
    ;; return icode.  So, if we get icode back, then we know
    ;; the form is a top-level expression that is to be
    ;; run at runtime
    ;;
    (let ((ic (compile tl-expr tl-envt dyn-envt 'top)))
      (if (instance? ic <icode>)
          (begin
            (if *tl-report*
                (format #t "  init expr ==> ~s" ic))
            (wrap-tl-expr ic))
	  (if ic
	      (format #t "top-level compile protocol error\n~#*@50s => ~s\n" 
		      tl-expr
		      ic))))))

(define (compile-early-once-only tl-expr tl-envt dyn-envt)
  (fluid-let ((*in-early-once-only?* #t))
    (for-each (lambda (form)
		(compile-tl-form form tl-envt dyn-envt))
	      (cdr tl-expr))
    #f))

(define (compile-strategy sf form lxe dye mode)
  (let ((s (cadr form)))
    ;;(format #t "switching strategy from ~s to ~s\n" *strategy* s)
    (if (memq s '(ccode bytecode))
	(fluid-let ((*strategy* (cadr form)))
	  (compile/begin sf
			 (cdr form)
			 lxe
			 dye
			 mode))
	(error/syntax "%strategy ~s: invalid strategy, use ccode or bytecode" 
		      s))))

;;


;; this actually generates the <code-descriptor>,
;; for which the actual code will get generated
;; later.  However, it returns the actual <template>
;; that will eventually represent the code in
;; question.  (Note that at this point, we
;; should probably know what the code generation
;; strategy is)

(define (aml->template asm code-ctx)
  (gen-template asm code-ctx))

;; (this is an analogue to aml->template, but defers
;;  actual code generation until later)

(define (gen-template asm code-ctx)
  (let ((tmpl #f)
	(property-list (code-ctx-properties code-ctx))
	(cd #f)
	(strategy (fluid-ref *strategy* 'bytecode)))
    (case strategy
     ((bytecode)
      (set! tmpl (make-gvec* <template> 0 0 '() #f
			     (code-ctx-literals code-ctx))))
     ((ccode)
      (set! tmpl (make-gvec* <template> 0 0 '()
			     (code-ctx-literals code-ctx))))
     (else
      (error/internal "unrecognized strategy: ~s" (fluid-ref *strategy*))))
    (seq-add! (fluid-ref *code-descriptors*)
	      (make <code-descriptor>
		    template: tmpl
		    code-properties: property-list
		    code: asm
		    c-name: '#uninit
		    strategy: strategy))
    tmpl))

(define (wrap-tl-expr (icode <icode>))
  (if *in-early-once-only?*
      (set-patch-time-only-icode! 
       *build-context*
       (cons icode (patch-time-only-icode *build-context*)))
      (set-top-level-icode! 
       *build-context*
       (cons icode (top-level-icode *build-context*)))))


(define (make-init-procs bcx)
  (participating-code
   ;; usually, no c-code will be generated here, but just in case
   ;; (as may happen, e.g., when the default strategy is `ccode')
   (string->file "inits.scm")
   (lambda ()
     (if (not (null? (top-level-icode bcx)))
	 (set-init-thunks! (building bcx)
			   (append (init-thunks (building bcx))
				   (list
				    (icode-list->thunk 
				     (reverse (top-level-icode bcx))
				     (list 'regular-inits (name bcx)))))))
     (set-first-init-thunks! 
      (building bcx)
      (if (not (null? (patch-time-only-icode bcx)))
	  (cons
	   (icode-list->thunk 
	    (reverse (patch-time-only-icode bcx))
	    (list 'once-only-inits (name bcx)))
	   (init-thunks (building bcx)))
	  (init-thunks (building bcx)))))))

(define (icode-list->thunk icode-list name)
  (icode->thunk (make <ic-seq>
		      return-types: '()
		      stmt-list: icode-list
		      mode: 'tail)
		name))

(define (icode->thunk icode name)
  (let ((cc (make-code-ctx (list (cons 'function-scope name)))))
    (let ((asm (expr->aml icode cc)))
      (make <target-closure>
	    environment: '()
	    template: (gen-template asm cc)))))

;; this function makes sure that name is bound
;; to a top-level variable in the given
;; (top-level) environment
;; it returns the <binding> that name is bound to

(define (ensure-tlv name envt)
  (let ((b (lookup (the-top-level envt) name)))
    (if b
	(if (instance? (actual-bdg b) <top-level-var>)
	    b
	    (error/semantic "~s not bound to a TLV" name))
	(let ((b (make <top-level-var>
		       name: name
		       value: '#unbound)))
	  (bind! (the-top-level envt) b)
	  b))))

;; make sure the argument binding is writable
;; (used by `define' and `set!' to check the
;;  validity of their arguments)
    
(define (ensure-writable bdg)
    (if (instance? bdg <imported-binding>)
	(error/semantic "~s is imported, not writable" bdg)
	(if (instance? bdg <top-level-var>)
	    (if (write-prot bdg)
		(error/semantic "~s is marked write-protected" bdg)
		bdg)
	    (if (instance? bdg <lexical-var>)
		bdg
		(error/semantic "~s is not a writable kind of thing" bdg)))))

(define (ensure-writable-tlv name envt)
    (ensure-writable (ensure-tlv name envt)))

;; make sure that we are creating a new binding
;; used by define-class & the like to prevent
;; multiple definitions
;; of course, it's a little tricky because we create
;; tlv bindings sans def.  In this case, the ACTUAL
;; binding is returned.  THEREFORE, WHEN ensure-new-tlb'ing
;; a <top-level-var> DON'T ASSUME THAT THE GIVEN bdg
;; WILL BE THE ACTUAL BDG -- use the return value

(define (ensure-new-tlb b-name envt bdg)
  (let ((b (lookup (the-top-level envt) b-name)))
    (if b
	(if (and (instance? b <top-level-var>)
		 (eq? (value b) '#unbound))
	    (if (instance? bdg <top-level-var>)
		(begin
		    (if (write-prot bdg)
			(warning "~s may have been side effected already" bdg))
		    ;; copy the new values 
		    (set-write-prot! b (write-prot bdg))
		    (set-value! b (value bdg))
		    (set-name! b (name bdg))
		    b)
		(error/semantic "~s used before defined (and not a TLV)" bdg))
	    (error/semantic "~s is already bound" b-name))
	(begin
	  (bind! (the-top-level envt) bdg)
	  bdg))))

(load "def.scm")
(load "defsyntx.scm")
(load "parseglue.scm")
(load "defglue.scm")
(load "defprim.scm")
(load "deffull.scm")

(load "expnvirt.scm")

;;;
;;; note that by design (found #f) => #f
;;;

(define (redirect-to-imported (self <top-level-contour>)
			      (var-name <symbol>)
			      b)
  (if (and (instance? b <top-level-var>)
	   (not (eq? self (top-level-envt *current-module*))))
      ; if we have found a top-level-var, and it is not from
      ; the current top-level envt, then redirect the binding
      ; to an appropriate <imported-binding>
      (begin
        (dbg (format #t "redirect with owner = ~s\n" (owner self)))
      (let* (((foreign-module <module>) (owner self))
	     ; find the <imported-module> that represents the
	     ; foreign module within the current module
	     ((im <imported-module>) (module->imported-module foreign-module))
	     ; get the link commands for the bindings from there
	     ((lbc <link-bdgs>) (car (link-commands im))))
	(dbg 
	 (format #t "redirecting ~s from ~s" var-name (name foreign-module)))
	; if there is already an imported binding from there, use that
	(let ((ib (table-lookup (imported-bindings lbc) var-name)))
	  (if ib
	      (begin
		(dbg (format #t " [0x~04x~04x]\n"
			     (obj-high-bits ib) (obj-low-bits ib)))
		ib)
	      ; otherwise, create and insert one
	      (let ((ib (make <imported-binding>
			      remote-binding: b
			      link-command: lbc
			      description: (list var-name (name im))
			      name: var-name
			      ib-writable?: (and
					     (memq var-name 
						   (module-permits-writing 
						    foreign-module))
					     #t)
			      ib-shared?: (and (memq var-name 
						     (module-shares 
						      foreign-module))
					       #t))))
		(dbg (format #t "   new [0x~04x~04x]\n"
			     (obj-high-bits ib) (obj-low-bits ib)))
		(table-insert! (imported-bindings lbc) var-name ib)
		ib)))))
      ; if it isn't a TLV, then there is no danger of it getting
      ; stored in a persistent data structure (usually...?)
      b))

(define-method lookup ((self <top-level-contour>) name)
  (redirect-to-imported
   self
   name
   (or (table-lookup (table self) name)
       (and (backing self)
	    (lookup (backing self) name)))))

;;;

(define-class <offline-with-envt> (<scope-record>)
  with-imported-module
  lexical-enclosing
  dynamic-enclosing)

(define-method lookup ((self <offline-with-envt>) (name <symbol>))
  (or (lookup-from-imported-module (with-imported-module self)
				   name
				   (owner (the-top-level self)))
      (lookup (lexical-enclosing self) name)))

(define (compile/with-module sf form lxe dye mode)
  (let ((e (make <offline-with-envt>
                 with-imported-module: (get-dependent-module (cadr form))
                 lexical-enclosing: lxe
                 dynamic-enclosing: dye)))
    (compile/body (cddr form) e e mode)))

(define (get-dependent-module m-name)
  (let loop ((ims (module-imports *current-module*)))
    (if (null? ims)
	(add-dependent-module m-name 'back)
	(if (eq? (name (car ims)) m-name)
	    (car ims)
	    (loop (cdr ims))))))
