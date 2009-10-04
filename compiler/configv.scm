#|------------------------------------------------------------*-Scheme-*--|
 | File:    compiler/configv.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.6
 | File mod date:    1998-12-06 11:19:21
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  (rsc)
 |
 `------------------------------------------------------------------------|#

;;
;; define the properties of this RScheme
;; system represented by this SOURCE tree
;;

#|
   if this is true, then a sandbox-type technique is used,
   whereby the dest. hierarchy is built with leaves as
   soft links into the SOURCE directory
   (doesn't apply to template-copy; only module files)
|#

(define *link-files?* #f)

;;
;; "module" refers to the RScheme compilation units, 
;; which may or may not have any C link-units associated
;; with them (module_descr's) (it may have several, in fact)
;;
;; "subproject" refers a portion of the main compile that results
;; in librs.a.  The subprojects are the C subprojects of the
;; standard modules plus the base runtime subprojects
;;

(define *base-runtime-components*
  '(#("runtime" "runtime" "runtime" rscheme)
    #("hasht" "hasht" "hasht" #f)
    #("heapi" "heapi" "heapi" #f)
    #("loadboot" "loadboot" "ldboot" #f)
    #("unstub" "unstub" "unstb" #f)
    #("gc" "gc" "xgc" #f)
    #("gcadapt" "gcadapt" "gcadapt" #f)
    #("rdln" "rdln" "rdln" #f)
    #("bci" "bci" "bci" bci)
    #("dfltmain" "dfltmain" "dfltmain" #f)))

;; must be listed most-requiring to least-requiring
;; (so the last module can depend on at most *scheme* and primops)

(define *modules* '(primops
		    precore
		    corelib
		    low-scheme
		    objsys
		    paths
		    tables
		    mathlib
		    iolib
		    high-scheme
		    regex
		    sort
		    imageio
		    editinp
		    start
		    compiler
		    codegen
		    mlink
		    repl
		    hacks))

;; a list is given when the file name and the C linkage name is different

;; make our path as absolute as we can...

(define *source-path* (append-dirs (process-directory) (current-directory)))

;; derived values...


;; for writing directly into the distribution
;; (create using `install-resource->path'
;;           and `distribution-resource->path',
;;  respectively for a resource that is to make
;;  it to the installation and one that is part
;;  of the distribution, but not part of the installation)

(define *dist-install-resource-dir* 
  (append-dirs *dist-path* (string->dir "install/resource")))

(define *dist-resource-dir* 
  (append-dirs *dist-path* (string->dir "resource")))

;; note that each stage of the end-to-end build gets
;; it's own resource directory
;; (search using `locate-src-resource')

(define *source-resource-path*
  (list (append-dirs *source-path* (string->dir "resource"))
	(append-dirs *source-path* (string->dir "dist/resource"))
	(append-dirs *source-path* (string->dir "dist/install/resource"))))

(define *dist-resource-path*
  (list (append-dirs *dist-path* (string->dir "resource"))
	(append-dirs *dist-path* (string->dir "install/resource"))))

;; we can't form an install resource path, because at this
;; stage the system hasn't been configured

;; (note that, once we get inside ourselves again, then there
;;  will be an install path which represents what we're running
;;  in now, but pre-0.6.0 doesn't have resource directories)
