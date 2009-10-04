
(define (make-new-doc)
  (let* ((doc (make <document>))
	 (rpage (make-rect 0 0 612 782))
	 (g1 (make <root-group>
		   in-document: doc
		   graphic-bounding-box: rpage))
	 (pg1 (make <page> 
		    in-document: doc
		    name: "1"
		    page-size: (make-size 612 782)
		    page-contents: g1))
	 (view1 (make <view>
		      in-document: doc
		      name: "1"
		      view-page: pg1
		      view-origin: $zero-point
		      view-ctm: (translate
				  (scale (make-affine-transform) 
					 (make-point 0.5 -0.5))
				  (make-point 0 (/ -782 2)))
		      view-frame: (make-rect 
                                   150 50
                                   (/ (+ 612 (* -2 $view-extent-margin)) 2)
                                   (/ (+ 782 (* -2 $view-extent-margin)) 2)))))
    ;(reconfig-to-fit-window view1)
    (set-document-views! doc (list view1))
    (set-document-pages! doc (vector pg1))
    doc))

;;;
;;;  Create a new document object intended for
;;;  encapsulating an EPS figure.
;;;
;;;  Returns the document *and* the root group
;;;  of the first page.
;;;

(define (make-new-eps-doc)
  (let ((d (make-new-doc)))
    (set-property! d 'eps #t)
    (values d (page-contents (view-page (car (document-views d)))))))
