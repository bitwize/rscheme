
(define-class <stylesheet> (<object>)
  (table type: <symbol-table>))

(define (make-stylesheet)
  (make <stylesheet>
        table: (make-symbol-table)))

;;;

(define-thread-var *current-stylesheet* #f)

(define (current-stylesheet)
  (or *current-stylesheet*
      (let ((x (make-stylesheet)))
        (set! *current-stylesheet* x)
        x)))

(define (call-with-stylesheet (s <stylesheet>) thunk)
  (thread-let ((*current-stylesheet* s))
    (thunk)))

;;;

;;;


(define-method get-style ((self <style>))
  self)

(define-method get-style ((self <symbol>))
  (if (current-stylesheet)
      (get-style (current-stylesheet) self)
      (error "~s: no current stylesheet" self)))

(define-method get-style ((self <stylesheet>) (name <symbol>))
  (or (table-lookup (table self) name)
      (error "~s: style ~s not defined in stylesheet" self name)))

;;;


(define-method style-compile ((self <symbol>) #rest r)
  (apply style-compile (get-style self) r))

;;;

(define-method set-style! ((name <symbol>) (item <style>))
  (set-style! (current-stylesheet) name item))

(define-method set-style! ((self <stylesheet>) (name <symbol>) (item <style>))
  (table-insert! (table self) name item))

(define (make-style (type <<style>>) . attrs)
  (bind ((a (keyword-list->symbol-assoc-list attrs))
         (q (make-dequeue)))
    (for-each (lambda (t)
                (if (not (memq (car t) '(name basis)))
                    (begin
                      (dequeue-push-back! q (car t))
                      (dequeue-push-back! q (cdr t))
                      (values))))
              a)
    ;;
    (make type 
          name: (let ((t (assq 'name a)))
                  (and t (cdr t)))
          basis: (let ((t (assq 'basis a)))
                   (and t (cdr t)))
          overrides: (dequeue-state q))))
                       
(define-syntax define-style
  (syntax-form (name type () . settings)
    (set-style!
     (current-stylesheet)
     (mquote name)
     (make-style type
                 name: (mquote name)
                 . settings)))
  (syntax-form (name type (basis) . settings)
    (set-style!
     (current-stylesheet)
     (mquote name)
     (make-style type
                 name: (mquote name)
                 basis: (mquote basis)
                 . settings))))
