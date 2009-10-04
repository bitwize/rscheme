;;;
;;;  (request (@ (id "GUID") (state "FOO")) ...)
;;;  states:
;;;     submitted -> approved | returned
;;;     approved -> completed | canceled
;;;     completed
;;;     returned -> submitted | canceled
;;;     canceled

,(use rs.util.properties)

(define (form->sxml rsp)
  (let ((fields (parse-fields->table (read-content (response->request rsp)))))
    (print fields)
    '()))

(define (request-submit sxml)
  (sxml->content (web-node->page "request.p")))

(define (foo path rsp)
  (if (eq? (get-property (response->request rsp) 'request-type) 'post)
      (request-submit (form->sxml rsp))
      (error "Not a POST")))

(uri-link-add! *rscheme-top*
               "request.p"
               (make-uri-simple-rsp-script (& foo)))


