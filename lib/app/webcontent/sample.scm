
(define (web-node->page name)
  (let ((x* (lookup type: 'webnode name: name)))
    (web-node->page* x* (content x*))))

(define (web-node->page* x* x)
  (let* ((_title (xpath-str x "content/title"))
         (_content (cdar (xpath () x "content/content")))
         (trailer (lookup type: 'template name: "*trailer"))
         (common (lookup type: 'template name: "*common"))
         (f_trailer (compile-template (content trailer)))
         (f_common (compile-template (content common)))
         (t_common (xpath-str (content common) "@lang")))
    (values (f_common _title _content (f_trailer x*))
            t_common)))

(define (ww)
  (let* ((t (lookup type: 'template name: "welcome")))
    (web-node->page* 
     t
     ((compile-template (content t))))))

;;;


(define (add-static! node name src)
  (uri-link-add! node
                 name
                 (make-uri-simple-rsp-script (static-generator src))))
  

(define (reload)
  (xbegin (root-user))
  (create-world "www.rscheme.org")
  (land-on "www.rscheme.org")
  (load "rscheme_org.scm")
  (xend))


