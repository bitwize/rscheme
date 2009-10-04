(load "dev/ps/driver.scm")

(define *num-pages-printed* 0)

;;;  print the current page 

(define-interactive (preview-page view)
  (interactive (owner))
  (set! *num-pages-printed* (+ *num-pages-printed* 1))
  (let* ((f (print-page (view-page (underlying-object view))))
         (p (run "gv" f)))
    (dm "Preview process: ~s" p)
    (thread-resume
     (make-thread
      (lambda ()
        (let ((s (exit-status p)))
          (dm "Preview process exited: ~s" s)
          (unlink f)))))
    (values)))

(define-interactive (print-view view)
  (interactive (owner))
  (set! *num-pages-printed* (+ *num-pages-printed* 1))
  (print-page (view-page (underlying-object view)))
  (values))

(define (print-page page #optional file)
  (let* (((doc <document>) (in-document page))
	 (eps? (get-property doc 'eps #f))
	 (file (or file
		   (format #f "/tmp/dv-~d-~d.~a"
			   (with-module unixm (getpid))
			   (let ((n *num-pages-printed*))
			     (set! *num-pages-printed* (+ 1 n))
				n)
			   (if eps? "eps" "ps"))))
	 (dev (open-view-ps-device page file)))
    (dm "document ~s eps? ~s" doc eps?)
    (set-owner! dev 'whatdya-care)
    (paint-object (page-contents page) dev)
    (close-ps-device dev)
    file))

(define (open-view-ps-device page file)
  (let* ((doc (in-document page))
	 (eps? (get-property doc 'eps #f)))
    (format #t "printing page `~a' to file: ~a\n" (name page) file)
    (if eps?
	(open-eps-device file (page-bbox page))
	(open-ps-device file))))

(define (page-bbox (page <page>))
  (get-property
   page
   'page-bbox
   (make-rect 0 0 (width (page-size page)) (height (page-size page)))))

(global-set-key '(#\C-x #\C-p) preview-page)

(define-interactive (set-is-eps doc)
  (interactive (document))
  (set-property! doc 'eps #t)
  (dm "document ~s eps? ~s" doc (get-property doc 'eps)))

(define-interactive (set-is-not-eps doc)
  (interactive (document))
  (set-property! doc 'eps? #f))

