
(define (faq-page name)
  (let* ((node (lookup type: 'faq
                       name: name))
         (q (sxml:children (car (xpath () (content node) "content/question"))))
         (a (sxml:children (car (xpath () (content node) "content/answer"))))
         (t (lookup type: 'template name: "*faq")))
    (web-node->page* node ((compile-template (content t)) q a))))
  
(define (faq path rsp)
  (values (sxml->content (faq-page (car path))) "text/html"))

(define *faq-dir* (make-uri-directory))

(uri-link-add! *rscheme-top* "faq" *faq-dir*)

(uri-link-add! *faq-dir*
               "="
               (make-uri-simple-rsp-script (& faq)))
