;;;
;;;  A simple framework to invoke a macro subprogram and
;;;  bring up the resulting diagram in a window
;;;

(define (document-w-macro thunk)
  (bind ((doc (make-new-doc))
         (page (vector-ref (document-pages doc) 0)))
    ;;
    (paste-from-extern (thunk) (page-contents page) $zero-size)
    ;;
    (let ((bbox (bounding-box (page-contents page))))
      ;;
      (set-property! doc 'eps #t)
      (set-property! page 'page-bbox bbox)
      (set-page-size! page (size+ (size bbox) (make-size 36 36)))
      ;;
      doc)))
  
(define (open-macro thunk)
  (open-document (document-w-macro thunk)))

(define (macro->eps thunk epsfile)
  (let* ((doc (document-w-macro thunk)))
    (print-page (vector-ref (document-pages doc) 0) epsfile)
    doc))

