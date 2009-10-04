
(define impl:with-xml-application-entity-ref
  (if-implements
   (available util.sxml)
   (with-module util.sxml with-xml-application-entity-ref)
   (lambda (fn thunk)
     (error "with-xml-application-entity-ref: not supported by SSAX back end"))))
   
(define impl:port->sxml
  (if-implements 
   (available util.sxml)
   (with-module util.sxml port->sxml)
   (with-module util.ssax ssax:port->sxml)))

(define impl:string->sxml
  (if-implements 
   (available util.sxml)
   (with-module util.sxml string->sxml)
   (with-module util.ssax (lambda (s ns)
                            (port->sxml (open-input-string s) ns)))))


;;;
;;;  If additional namespaces need to be supplied,
;;;  the appear in the format:
;;;      namespaces ::= (namespace-decl ...)
;;;      namespace-decl ::= ([PREFIX] LOCAL-NAME URI)
;;;
;;;     where LOCAL-NAME is the prefix (symbol) that will be used 
;;;                      in the returned SXML,
;;;           URI is the (string) which identifies the namespace,
;;;           PREFIX, if present, is the prefix (symbol) that
;;;                   occurs in the XML text.
;;;
;;;
;;;  e.g.,
;;;    (string->sxml "<a:foo>bar</a:foo>" '((a AA "http://www.foo.org/")))

(define (string->sxml (str <string>) #optional (namespaces default: '()))
  (impl:string->sxml str namespaces))

(define (port->sxml (port <input-port>) #optional (namespaces default: '()))
  (impl:port->sxml port namespaces))

;;;
;;;  Reads out multiple "documents" from a given string
;;;
;;;   (string->sxml-sequence "<a/><a>foo</a>")
;;;             -> ((*TOP* (a)) (*TOP* (a "foo")))

(define (string->sxml-sequence (str <string>) 
                               #optional (namespaces default: '()))
  (let ((p (open-input-pushback-port (open-input-string str)))
        (q (make-dequeue)))
    (let loop ()
      (let ((item (impl:port->sxml p namespaces)))
        (if (eof-object? item)
            (vector->list (dequeue-state q))
            (begin
              (dequeue-push-back! q item)
              (loop)))))))
