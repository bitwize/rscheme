(define-method input-port-read-line ((self <std-input-port>))
  (let ((line (fgetln (file-stream self))))
    (increment-line self)
    (or line $eof-object)))

(define-module-extend rs.lang ()
  (&module (export quantity? logical-shift-right)))

(define-module gui.app.dv (unquote)
  ,(use rs.lang
        rs.lang.eval)
  ,(use rs.lang.internal) ;; we want clone
  ,(use rs.sys.numeric)
  ,(use rs.sys.tables)
  ,(use rs.sys.multimethod)

  ,(load "dvm.scm")
  ,(export start-dv dv-main)

  ,(export <group>
	   <graphic-object>
	   set-page-size!
	   set-view-frame!
	   reconfig-to-fit-window
	   open-document
	   call-interactive
	   dv-eval
	   make-new-doc
	   make-new-eps-doc
	   document-pages
	   document-views
	   view-page
	   page-contents
	   bounding-box
	   paste-from-extern
	   ;
	   define-interactive
	   ;; blech...  do it hygienically!
	   set-interactive-wrapper!
	   get-interactive-wrapper
	   set-synchronous!)
  ;
  ,(export print-page
	   page-size
	   with-client
	   make-client))
