;;;
;;;  Take an article file (a <chapter> written in DocBook 5.0)
;;;  and build an article directory for the web server
;;;

#|
(define (load-article src)
  (with-xml-refs
   (lambda (key)
     (table-lookup *application-entities* key))
   (lambda ()
     (call-with-input-file
         src
       port->sxml))))
|#


(define-class <article> (<object>)
  (year type: <fixnum>)
  id-index
  numbering-index
  article-id
  title
  document
  rss-sxml
  (artifacts init-value: '()))

(define-class <artifact> (<object>)
  (container type: <article>))

(define-class <linked-file> (<artifact>)
  (name type: <string>)
  (file type: <file-name>))

(define-class <image-file> (<artifact>)
  (name type: <string>)
  (png-file type: <file-name>)
  (eps-file type: <file-name>))

(define-class <article-node> (<artifact>)
  (name type: <string>)
  (subtitle type: <string>)
  (content type: <list>))


(define (init-article src)
  (let ((x (xpath-str-unique src "info/biblioid[@otherclass='rscheme.org']")))
    (make <article>
          document: src
          title: (xpath-str-unique src "title")
          article-id: (cadr (string-split x #\/))
          year: (string->number (car (string-split x #\/)))
          id-index: (make-string-table)
          numbering-index: (make-string-table)
          rss-sxml: `(rss
                      (guid 
                       ,(xpath-str-unique src "info/biblioid[@otherclass='uuid']"))
                      (description ,(xpath-str-unique src "info/abstract/para"))
                      (author ,(xpath-str src "info/author/text()")
                              " <"
                              ,(xpath-str src "info/author/link/email")
                              ">")))))

(define (add-artifact (self <article>) (a <artifact>))
  (set-artifacts! self (cons a (artifacts self))))

(define-thread-var *current-article* #f)
(define-thread-var *current-article-node* #f)

(define (make-artifact class . args)
  (let* ((super *current-article*)
         (a (apply (with-module objsys make-instance)
                   class 
                   container: super
                   args)))
    (add-artifact super a)
    a))

(define $articles-fs-root "/u/donovan/p/rscheme-web/state/rs-articles/articles")

(define (article-dir (self <article>))
  (~ "~a/~d/~a" $articles-fs-root (year self) (article-id self)))

(load "uptodate.scm")

(define-method output-article-artifact ((self <linked-file>))
  (let* ((rel (string->file (name self)))
         (dst (append-path (string->dir (article-dir (container self))) rel)))
    ;;
    (format #t "  link: ~a\n" (name self))
    ;;
    (symlink
     (pathname->os-path (file self))
     (pathname->os-path dst))))

(define-method output-article-artifact ((self <image-file>))
  (format #t "  image: ~a" (name self))
  (let* ((src (pathname->os-path (eps-file self)))
         (dstf (extension-related-path (eps-file self) "png"))
         (dst (pathname->os-path dstf)))
    ;;
    (if (up-to-date? src dst)
        (format #t " (up to date)\n")
        (begin
          (format #t " updating\n")
          (check-exit-status (run "eps2png" src dst))))
    ;;
    (symlink 
     dst
     (pathname->os-path 
      (append-path
       (string->dir (article-dir (container self)))
       (png-file self))))))



(define-method output-article-artifact ((self <article-node>))
  (let ((file (string-append (name self) ".xml")))
    (format #t "output: ~s (~a)\n" file (name self))
    (call-with-output-file
        (string-append (article-dir (container self)) "/" file)
      (lambda (port)
        (write-sxml
         (list '*TOP*
               '(*XML* "version='1.0'") "\n"
               (pretty-printify-xml
                `(article-node
                  (title ,(title (container self)))
                  ,@(if (string=? (name self) "root")
                        (list (rss-sxml (container self)))
                        '())
                  ;;
                  (body
                   (h1 ,(subtitle self))
                   (div (@ (class "body"))
                        ,@(content self)))))
               "\n")
         port)))))

(define (output-article (self <article>))
  (let* ((dir (article-dir self)))
    ;;
    (check-exit-status (run "rm" "-rf" dir))
    ;;
    (os-mkdir dir)      ; don't create the YEAR directory
    ;;
    (for-each
     (lambda (aux)
       (let ((d (string-append dir "/" aux)))
         (if (not (stat d))
             (os-mkdir d))
         (values)))
     '("references"))
    ;;
    (for-each output-article-artifact (artifacts self))))
         
       
