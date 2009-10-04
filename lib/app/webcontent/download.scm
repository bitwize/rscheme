
(define (downloader path rsp)
  (let ((node (search-top-level type: 'release
                                sxml: `(file ,(car path)))))
    (if node
        (values
         (file->content (xpath-str (content node) "src"))
         "application/x-gzip")
        (error "nada ~s" (car path)))))

(uri-link-add! *rscheme-top*
               "download"
               (make-uri-simple-rsp-script (& downloader)))
