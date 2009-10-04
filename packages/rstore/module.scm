#|------------------------------------------------------------*-Scheme-*--|
 | File:	    packages/rstore/module.scm
 |
 |          Copyright (C)1998 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |	    See <http://www.rscheme.org/> for the latest info.
 |
 | File version:     1.17
 | File mod date:    2005-05-13 08:59:36
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  rs.db.rstore
 |
 | Purpose:          Module control file
 `------------------------------------------------------------------------|#

(define-module rs.db.rstore ()
  (&module
   (import rs.lang)
   (import rs.lang.internal)
   (import rs.sys.paths)
   (import rs.sys.tables)
   (import rs.db.lss)
   (import rs.glue)
   (import rs.sys.linker)
   (import sort))
  ;
  (&module
   ;;
   (load "../lss/glue.scm")
   (load "debugmsg.scm")
   (load "glue.scm")
   (load "pstore.scm")
   (load "copyin.scm")
   (load "indir.scm")
   (load "commit.scm")
   ;;
   (export register-pivot!
           extend-named-pivot-index!
           get-named-pivots)
   ;;
   (export open-persistent-store
	   create-persistent-store
	   read-persistent-store
	   close-persistent-store
           open-pstore-on-lss
           lss-tune
	   commit
	   set-compression-method!
	   root-object
           copy-into-pstore
	   set-root-object!
	   register-indirect-page
	   default-allocation-area
	   
	   num-dirty-pages
	   rollback-dirty-pages
           pstore-set-commit-id
           in-persistent-store?
           pstore-last-commit-id
	   object->allocation-area
	   <persistent-store>
	   make-allocation-area
           allocation-area->store
           object-deallocate
           location-deallocate)
   (export pstore-write-protect?
           pstore-set-write-protect!
           pstore-next-commit-id)

   (export transient->persistent
           persistent->parts
           parts->persistent
           persistent->transient
           persistent-hash)

   (export pstore-meta-scan-pagetable
           pstore-meta-scan-starts
           pstore-meta-scan-objects
           pstore-meta-scan-npages
           pstore-meta-root-info
           pstore-meta-std-indirects
           pstore-meta-insert-indirects
           pstore-meta-scan-first-pp
           pstore-meta-scan-interior-pp)

   (export gc:set-tracking
           gc:color
           gc:work
           gc:set-gray!)
   ;;
   (load "gc.scm")
   (export ;;
           start-online-compacter       ; does actual work when pstore commits
           fuel-online-compaction!      ; do extra work... right now!
           stop-online-compacter        ; immediately stop any future work
           print-compaction-status)))
    
