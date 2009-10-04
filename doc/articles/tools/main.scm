;(load "~/p/rslib/graphics/device/scripting.scm")

(load "common.scm")
;(load "expand-entities.scm")
(load "assign-ids.scm")
(load "make-article.scm")
(load "graphic-script.scm")
(load "style-driver.scm")
(load "breakup.scm")
(load "numbering.scm")

(define (load-xml-document (src <file-name>))
  (call-with-input-file src port->sxml))

(define *art* #f)

(define (r)
  (assign-ids
   (sxml:root-element 
    (load-xml-document (string->file "./persistence.xml")))))

(define (t)
  (let* ((content (r))
         (a (init-article content)))
    ;;
    (thread-let ((*current-article* a))
      (set! *art* a)
      (number-article-objects a)
      (let ((root (build-article a content)))
        (format #t "style: ")
        (style-article-node root)
        (format #t " ...done\n"))
      ;;
      (for-each
       (lambda (uri)
         (format #t "   INCLUDING ~s\n" uri))
       (xpath () content "//bibliosource/link/uri"))
      ;;
      (output-article a))
    a))

#|
(define rr (load-xml-document (string->file "./test-break.xml")))
(define aa (init-article (sxml:root-element rr)))
(build-article aa rr)

(for-each print (artifacts aa))

(last (artifacts aa))

|#
