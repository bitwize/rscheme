
(define-class <document> (<object>) :abstract
  (properties type: <vector> init-value: '#())
  (id type: <fixnum> init-value: -1)) ;; >=0 for bound documents

;;;
;;;  parses a document and returns a set (list) of
;;;    (SECTION . KEYWORDS)
;;;  where SECTION refers to an index collection
;;;  such as `main', `subject', `from', etc.
;;;

(define-generic-function get-keywords)

