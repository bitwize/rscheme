
(define (make-test-doc)
  (let* ((doc (make <document>))
	 (rpage (make-rect 0 0 612 782))
	 (g1 (make <root-group>
		   in-document: doc
		   graphic-bounding-box: rpage))
	 (g2 (make <root-group>
		   in-document: doc
		   graphic-bounding-box: rpage))
	 (pg1 (make <page> 
		    in-document: doc
		    name: "1"
		    page-size: (make-size 612 782)
		    page-contents: g1))
	 (pg2 (make <page> 
		    in-document: doc
		    page-size: (make-size 612 782)
		    name: "2"
		    page-contents: g2))
	 ;
	 (box1 (make <box-graphic>
		     in-document: doc
		     parent-object: g1
		     graphic-bounding-box: rpage
		     origin: (make-point 20 20)
		     width: 30
		     height: 20))
	 (box2 (make <box-graphic>
		     in-document: doc
		     parent-object: g1
		     graphic-bounding-box: rpage
		     origin: (make-point 100 100)
		     width: 50
		     height: 50))
	 (line1 (make <line-graphic>
		      in-document: doc
		      parent-object: g1
		      graphic-bounding-box: rpage
		      line-start: (make-point 50 30)
		      line-end: (make-point 75 30)))
	 (line2 (make <line-graphic>
		      in-document: doc
		      parent-object: g1
		      graphic-bounding-box: rpage
		      line-start: (make-point 75 30)
		      line-end: (make-point 75 110)))
	 (line3 (make <line-graphic>
		      in-document: doc
		      parent-object: g1
		      graphic-bounding-box: rpage
		      line-start: (make-point 75 110)
		      line-end: (make-point 100 110)))
	 (view1 (make <view>
		      in-document: doc
		      name: "1"
		      view-page: pg1
		      view-frame: (make-rect 50 50 632 812)))
	 (view2 (make <view>
		      in-document: doc
		      name: "2"
		      view-page: pg1
		      view-frame: (make-rect 200 70 250 250))))
    ;(reconfig-to-fit-window view1)
    ;(update-view-extents! view1)
    ;(update-view-extents! view2)
    (concat-view-ctm! view2 (rotate $identity-transform 20))
    (set-center-point! view2 (make-point 125 125))
    (set-document-views! doc (list view1 view2))
    (set-document-pages! doc (vector pg1 pg2))
    doc))
