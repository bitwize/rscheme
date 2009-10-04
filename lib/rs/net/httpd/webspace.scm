,(use tables)

;;;

(define-class <web-space> (<object>)
  (properties type: <vector> init-value: '#())
  root-uri-node)

;;;

(define-class <uri-node> (<object>) :abstract)

(define-class <uri-union-node> (<uri-node>)
  (members type: <list>))

(define-class <uri-redirect> (<uri-node>)
  (target-uri type: <string>))

(define-class <uri-redirect-proc> (<uri-node>)
  (target-uri type: <function>))

(define-class <uri-literal-node> (<uri-node>)
  (contents type: <string>)
  (mime-type init-value: #f))

(define-class <uri-disk-node> (<uri-node>)
  (mime-type init-value: #f)
  disk-file-path)

(define-class <uri-disk-dir> (<uri-node>)
  (permit-listing? init-value: #t)
  disk-file-path)

(define-class <uri-directory> (<uri-node>)
  (permit-listing? init-value: #t)
  (as-leaf-node init-value: #f)
  (contents init-function: make-string-table))

;;;
;;;  The members of a pattern directory are
;;;  pairs of a procedure that determines whether
;;;  a given path matches and a procedure to
;;;  process the match
;;;

(define-class <uri-pattern-directory> (<uri-node>)
  (contents type: <list> init-value: '()))

(define-class <uri-dynamic-directory> (<uri-node>)
  (as-leaf-node init-value: #f)
  (property-name type: <symbol>)
  (binder type: <function>)
  (contents type: <uri-node>))

(define (make-uri-dynamic-directory #key
                                    (property type: <symbol>)
                                    (lookup-proc type: <function>)
                                    (content type: <uri-node>)
                                    (as-leaf default: #f))
  (if as-leaf
      (assert (instance? as-leaf <uri-node>)))
  (make <uri-dynamic-directory>
        as-leaf-node: as-leaf
        binder: lookup-proc
        property-name: property
        contents: content))

;;; if a node returns #t for `directory?', 
;;; then it has children nodes...

(define-method directory? ((self <uri-node>))
  #f)

(define-method directory? ((self <uri-directory>))
  #t)

(define-method directory? ((self <uri-pattern-directory>))
  #t)

(define-method directory? ((self <uri-dynamic-directory>))
  #t)

(define-method directory? ((self <uri-disk-dir>))
  #t)

;;;

(define-class <script-node> (<uri-node>) :abstract)

(define-class <complete-script-node> (<script-node>)
  (procedure type: <function>))

(define-class <simple-script-node> (<script-node>)
  (procedure type: <function>))

(define-class <simple-rsp-script-node> (<simple-script-node>))

(define-class <uri-post-form> (<uri-node>)
  (parameters type: <list>)
  (procedure type: <function>))


;;;

(define (make-web-space)
  (make <web-space>
        root-uri-node: (make <uri-directory>)))

(define (make-uri-directory #key (as-leaf default: #f))
  (make <uri-directory>
        as-leaf-node: as-leaf))

(define (make-uri-pattern-directory)
  (make <uri-pattern-directory>))

(define-method make-uri-simple-rsp-script ((tlv <top-level-var>))
  (make <simple-rsp-script-node>
        procedure: (lambda (path rsp)
                     ((value tlv) path rsp))))

(define-method make-uri-simple-rsp-script ((proc <function>))
  (make <simple-rsp-script-node>
        procedure: proc))

(define-method make-uri-complete-script ((tlv <top-level-var>))
  (make <complete-script-node>
        procedure: (lambda (path rsp)
                     ((value tlv) path rsp))))

(define-method make-uri-complete-script ((proc <function>))
  (make <complete-script-node>
        procedure: proc))
  
(define-method make-uri-simple-script ((tlv <top-level-var>))
  (make <simple-script-node>
        procedure: (lambda (path req)
                     ((value tlv) path req))))
  
;;;  The `parms' is a list of things that the server code will
;;;  parse out of the post request.  
;;;
;;;  Each thing is a list whose car is a name, whose cadr is either
;;;  `field' or `property', and whose caddr is is the field or property
;;;  to obtain.  For example:
;;;
;;;      parms => ((login: field "login") (cookie: property %cookie))
;;;
;;;  will cause the proc to be called as follows:
;;;
;;;      (proc [the response object]
;;;            login: [the value of the "login" POST field]
;;;            cookie: [the value of the '%cookie property of the request obj]
;;;      )
;;;
;;;  Furthermore, all of the post fields
;;;  will be available in the '%form-fields property of the request,
;;; 

(define (make-uri-post-form parms proc)
  (make <uri-post-form>
        parameters: parms
        procedure: (if (instance? proc <top-level-var>)
                       (lambda (rsp . rest)
                         (apply (value proc) rsp rest))
                       proc)))

;;; The given procedure must take 2 arguments (path and req)
;;; and should return one or two values,
;;; the first the body content to be returned, which can be
;;;   - a string
;;;   - some other content denotation, e.g., built using file->content
;;;     or sxml->content
;;;   - a <redirection> built using (redirection URI)
;;;
;;; The second is optional and is the content-type which defaults
;;; in most cases to "text/html"

(define-method make-uri-simple-script ((proc <function>))
  (make <simple-script-node>
        procedure: proc))
  
(define-method uri-link-add! ((self <web-space>) name entry)
  (uri-link-add! (root-uri-node self) name entry))

(define (simple-path-match test)
  (lambda (path)
    (and (pair? path)
         (string=? (car path) test))))

(define (simple-regex-match test)
  (lambda (path)
    (if (pair? path)
        (bind ((s e #rest r (test (car path))))
          (if s
              r
              #f))
        #f)))

(define (simple-always-match path)
  (if (pair? path)
      (car path)
      ""))

(define-method uri-link-add! ((self <uri-pattern-directory>) test proc)
  (let ((t (cond
            ((string? test)
             (simple-path-match test))
            ((pair? test)
             (simple-regex-match (reg-expr->proc test)))
            ((procedure? test)
             test)
            ((eq? test #t)
             simple-always-match)
            (else
             (error "don't know how to handle pattern test ~s" test))))
        (p (cond
            ((instance? proc <uri-node>)
             proc)
            ((procedure? proc)
             proc)
            (else
             (error "don't know how to handle pattern exec ~s" proc))))
        (old (assoc test (contents self))))
    (if old
        (set-cdr! old (list t p))
        (set-contents! self (append (contents self) 
                                    (list (list test t p)))))
    (values)))
                              
(define-method uri-link-add! ((self <uri-directory>) 
                              (name <string>)
                              (entry <uri-node>))
  (table-insert! (contents self) name entry))

(define-method uri-link-remove! ((self <uri-directory>) 
                                 (name <string>))
  (table-remove! (contents self) name))

(define (check-mime-type spec)
  (if spec
      (if (string? spec)
          spec
          (error "expected <string> or #f ; saw ~s for mime-type" spec))))

(define (make-uri-literal-node (content <string>) #key (mime-type default: #f))
  (check-mime-type mime-type)
  (make <uri-literal-node>
        contents: content
        mime-type: mime-type))

(define-method make-uri-disk-node ((file <string>) 
                                   #key (mime-type default: #f))
  (check-mime-type mime-type)
  (make <uri-disk-node>
        disk-file-path: file
        mime-type: mime-type))

(define-method make-uri-disk-node ((thunk <function>)
                                   #key (mime-type default: #f))
  (check-mime-type mime-type)
  (make <simple-script-node>
        procedure: (if mime-type
                       (lambda (req path)
                         (values (file->content (thunk)) mime-type))
                       (lambda (req path)
                         (file->content (thunk))))))
  
(define (make-uri-disk-dir (dir <string>))
  (make <uri-disk-dir>
        disk-file-path: dir))

(define-method make-uri-redirect ((uri <string>))
  (make <uri-redirect>
        target-uri: uri))

(define-method make-uri-redirect ((uri <function>))
  (make <uri-redirect-proc>
        target-uri: uri))

(define (make-uri-union-space . members)
  (assert (every? (lambda (x)
                    (and (instance? x <uri-node>)
                         (directory? x)))
                  members))
  (make <uri-union-node>
        members: members))

;;;

(define *mime-type-guesses* '())

(define (define-mime-type-guess-by-suffix suffix type)
  (set! *mime-type-guesses*
        (cons (let ((n (string-length suffix)))
                (lambda ((p <string>))
                  (if (and (>= (string-length p) n)
                           (string=? (substring p (- (string-length p) n))
                                     suffix))
                      type
                      (values))))
              *mime-type-guesses*)))

(define-mime-type-guess-by-suffix "" "text/plain")
(define-mime-type-guess-by-suffix ".html" "text/html")
(define-mime-type-guess-by-suffix ".htm" "text/html")
(define-mime-type-guess-by-suffix ".xml" "text/xml")

(define-mime-type-guess-by-suffix ".png" "image/png")
(define-mime-type-guess-by-suffix ".jpg" "image/jpeg")
(define-mime-type-guess-by-suffix ".jpeg" "image/jpeg")
(define-mime-type-guess-by-suffix ".swf" "application/x-shockwave-flash")

(define-mime-type-guess-by-suffix ".tar" "application/octet-stream")
(define-mime-type-guess-by-suffix ".gz" "application/x-gzip")
(define-mime-type-guess-by-suffix ".rpm" "application/x-rpm")
(define-mime-type-guess-by-suffix ".exe" "application/octet-stream")
(define-mime-type-guess-by-suffix ".pdf" "application/pdf")
(define-mime-type-guess-by-suffix ".eps" "application/postscript")

(define (guess-mime-type filename)
  (let loop ((g *mime-type-guesses*))
    (if (null? g)
        (values)
        (or ((car g) filename)
            (loop (cdr g))))))
