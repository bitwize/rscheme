,(use util.xml
      util.xpath)

(define (import-svg->document file)
  (bind ((doc (make-new-doc))
	 (view (car (document-views doc)))
	 (page (view-page view))
	 (bbox (bounding-box (page-contents page))))
    ;
    (set-property! doc 'eps #t)
    (set-property! page 'page-bbox bbox)
    (set-page-size! page (make-size 50 50))
    ;
    (reconfig-to-fit-window view)
    doc))
