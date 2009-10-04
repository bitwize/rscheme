
(define (import-webspace file)
  (import-webspace*
   (car (xpath:eval (call-with-input-file file port->sxml)
                    '((child webspace)
                      (child directory))))))

(define (import-webspace* node)
  (case (car node)
    ((directory)
     (let ((n (make <uri-directory>
                    permit-listing?: (get-boolean-attr node 'listable))))
       (for-each
        (lambda (entry)
          (let ((content (car (select sxml:element?
                                       (sxml:children entry)))))
            (table-insert! (contents n)
                           (get-string-attr entry 'name)
                           (import-webspace* content))))
        (xpath:eval node '((child entry))))
       n))
    ((literal)
     (let* ((content (xpath:node-set->string (sxml:children node)))
            (encoding (get-string-attr node 'encoding))
            (decoded (cond
                      ((string=? encoding "pem")
                       (pem-decode content))
                      (else
                       content))))
       (make <uri-literal-node>
             mime-type: (get-string-attr node 'mimetype)
             contents: decoded)))
    (else
     (error "bad node: ~s" (car node)))))


(define (get-string-attr node key)
  (let ((s (xpath:eval node (list (list 'attribute key)))))
    (if (string? s)
        s
        "")))

(define (get-boolean-attr node key)
  (let ((s (xpath:eval node (list (list 'attribute key)))))
    (and (string? s) (string=? s "1"))))

;;;

(define-class <uri-cached-raw> (<uri-node>)
  raw-file
  cache-file
  (cached init-value: #f))

(define (open-raw-cache cachefile)
  (let ((p (open-persistent-store cachefile)))
    (register-indirect-page p 10 (vector <uri-directory>
                                         <uri-literal-node>))
    p))

(define (make-raw-cache rawfile cachefile)
  (let* ((top (import-webspace rawfile))
         (p (create-persistent-store cachefile)))
    (register-indirect-page p 10 (vector <uri-directory>
                                         <uri-literal-node>))
    (commit p top)
    p))

(define (mmap-loaded-raw (self <uri-cached-raw>))
  (let ((a (stat (raw-file self)))
        (b (stat (cache-file self))))
    (if (and b (time<? (stat-mtime a) (stat-mtime b)))
        (or (cached self)
            (begin
              (set-cached! self (open-raw-cache (cache-file self)))
              (cached self)))
        (begin
          (if (cached self)
              (close-persistent-store (cached self)))
          (set-cached! self (make-raw-cache (raw-file self)
                                            (cache-file self)))
          (cached self)))))
        

(define-method dispatch-uri ((self <uri-cached-raw>) path rsp)
  (dispatch-uri (root-object (mmap-loaded-raw self)) path rsp))

(define (make-uri-cached-raw rawfile cachefile)
  (make <uri-cached-raw>
        raw-file: rawfile
        cache-file: cachefile))


;;;  a uri-cached-raw is always presumed to be a directory,
;;;  so we can avoid the caching hit when just generating
;;;  a listing of our parent directory

(define-method directory? ((self <uri-cached-raw>))
  ;(directory? (root-object (mmap-loaded-raw self)))
  #t)
