
(define-class <<style>> (<<standard-class>>)
  (style-attributes type: <vector>))

(define-class <style> (<object>) :abstract
  (basis init-value: #f)
  (overrides type: <vector> init-value: '#())
  (name init-value: #f))         ; or a symbol

(define (style? x)
  (instance? x <style>))

(define-method write-object ((self <style>) port)
  (if (name self)
      (format port "#[~a ~a]" (name (object-class self)) (name self))
      (next-method)))

;;;

(define *style-type-dict* (make-symbol-table))

(define (declare-style-type (style-type <<style>>))
  (table-insert! *style-type-dict* (class-name style-type) style-type))

;;;
;;;   For example,
;;;
;;;     (define-style-type <font-style> family size weight slant)

(define-macro (define-style-type class-name . attributes)
  `(begin
     (define-class ,class-name (<style>)
       metaclass: <<style>>
       style-attributes: ',(list->vector attributes))
     (declare-style-type ,class-name)))

;;;

(define-generic-function get-style)
(define-generic-function style-compile)

;;;

(define-macro (define-style-definer definer-name type)
  `(define-syntax ,definer-name
     (syntax-form (name () . settings)
       (define-style name ,type () . settings))
     (syntax-form (name (basis) . settings)
       (define-style name ,type (basis) . settings))))

