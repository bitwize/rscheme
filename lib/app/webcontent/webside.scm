,(use rs.net.httpd)

(define *rscheme-webspace* (make-web-space))
(define *rscheme-top* (make-uri-directory))
(uri-link-add! *rscheme-webspace* "" *rscheme-top*)

(define *xynthesis-webspace* (make-web-space))
(define *xynthesis-top* (make-uri-directory))
(uri-link-add! *xynthesis-webspace* "" *xynthesis-top*)

;;;

(define (opengo f)
  (access f)
  (land-on "home")
  (go))

(define (go)
  (start-http-server
   (list
    (list "rscheme.org" 8880 *rscheme-webspace* 'stdout)
    (list "xynthesis.com" 8881 *xynthesis-webspace* 'stdout))))

;;;
               

(define (static-generator name)
  (let* ((a (lambda () (let ((c (content (lookup type: 'static name: name))))
                     (list (xpath-str c "content")
                           (xpath-str c "@type"))))))
    (lambda (path rsp)
      (list->values (a)))))


#|
(uri-link-add! *rscheme-top*
               "style.css"
               (make-uri-simple-rsp-script
                (static-generator "style.css")))
|#


(add-static! *rscheme-top* "style.css" "style.css")
(add-static! *rscheme-top* "rscheme.png" "rscheme.png")

(uri-link-add! *rscheme-top*
               "test1"
               (make-uri-simple-rsp-script
                (lambda (path rsp)
                  (values (sxml->content (ww))
                          "text/html"))))

(define (add-webnode! node name src)
  (uri-link-add! node
                 name
                 (make-uri-simple-rsp-script
                  (lambda (path rsp)
                    (values (sxml->content (web-node->page src))
                            "text/html")))))
                          
(add-webnode! *rscheme-top* "account-request" "account-request")
(add-webnode! *rscheme-top* "site-request" "site-request")

(define (current-releases)
  (map
   (lambda (name)
     (let* ((rel (lookup type: 'release name: name))
            (note (lookup type: 'relnote name: name))
            (r (content rel)))
       `(tr
         (td ,(xpath-str r "major"))
         (td ,(let ((minor (xpath-str r "minor"))
                    (build (xpath-str r "build")))
                (if (string=? build "final")
                    minor
                    (~ "~a-~a" minor build))))
         (td ,(xpath-str r "date"))
         (td (a (@ (href ,(~ "/download/~a" (xpath-str r "file"))))
                ,(~ "~.1fMB" (/ (string->number (xpath-str r "size"))
                                1048576.0))))
         (td ,@(if note
                   (sxml:children (car (xpath () (content note) "content")))
                   '((& "nbsp")))))))
   (scan-directory "current-releases")))

(define (recent-faqs)
  (map
   (lambda (id)
     (let* ((entry (lookup type: 'faq name: id))
            (r (content entry)))
       `(tr
         (td ,(time->string (timestamp (modification-audit entry))
                            "%Y-%m-%d"))
         (td ,(name (principal (modification-audit entry))))
         (td (a (@ (href ,(~ "/faq/=/~a" id)))
                ,@(sxml:children (car (xpath () r "content/question"))))))))
   (scan-directory "recent-faqs")))
