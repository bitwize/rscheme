
(define-class <memo-table> (<object>)
  name
  underlying-procedure
  memoized-procedure
  symbol-setter
  memo-table)

;;;

(define *enable-memoization* #t)
(define *all-memo-tables* '())

;;

(define (disable-memoization #optional (flag default: 0))
  (set! *enable-memoization* (not flag)))

(define (find-by-fn fn)
  (let loop ((l *all-memo-tables*))
    (if (null? l)
        #f
        (if (eq? (memoized-procedure (car l)) fn)
            (car l)
            (loop (cdr l))))))

(define (clear-memoization1 (self <memo-table>))
  (hash-table-clear! (memo-table self)))

(define (clear-all-memoizations #optional fn)
  (if fn
      (let ((m (find-by-fn fn)))
        (if m
            (clear-memoization1 m)
            (error "No memoization table found for ~s" fn)))
      (for-each clear-memoization1 *all-memo-tables*)))
  
(define (list-memoizations #optional fn)
  (if fn
      (let ((m (find-by-fn fn)))
        (if m
            (list-memoization1 m)
            (error "No memoization table found for ~s" fn)))
      (for-each list-memoization1 *all-memo-tables*)))

(define (unmemoize fn)
  (let ((m (find-by-fn fn)))
    (if m
        (begin
          ((symbol-setter m) (underlying-procedure m))
          (set! *all-memo-tables* (delq m *all-memo-tables*))
          (values))
        (error "No memoization table found for ~s" fn))))
       
(define (list-memoization1 (self <memo-table>))
  (format #t "================ MEMOIZATIONS FOR [~a] ===============\n" 
          (name self))
  (print (memo-table self)))
      

(define (add-memo-table name ctor old-fn setter)
  (bind ((new-fn tbl (ctor old-fn)))
    (set! *all-memo-tables*
          (cons (make <memo-table>
                      name: name
                      symbol-setter: setter
                      underlying-procedure: old-fn
                      memoized-procedure: new-fn
                      memo-table: tbl)
                *all-memo-tables*))
    ;;
    (setter new-fn)))

;;

(define (make-memoized fn)
  (let ((tbl (make-table)))
    (values
     (lambda args
       (list->values
        (or (table-lookup tbl args)
            (bind ((#rest r (apply fn args)))
              (table-insert! tbl args r)
              r))))
     tbl)))

(define (make-memoized-file-accessor fn)           ; file name is first arg
  (let ((tbl (make-table)))
    (values
     (lambda args
       (let ((e (table-lookup tbl args))
             (sb (stat (car args))))
         (if sb
             (if (and e (time=? (stat-mtime sb) (car e)))
                 (list->values (cdr e))
                 (bind ((#rest r (apply fn args)))
                   (table-insert! tbl args (cons (stat-mtime sb) r))
                   (list->values r)))
             (apply fn args))))
     tbl)))

(define-macro (memoize name)
  `(add-memo-table ',name make-memoized
                   ,name (lambda (x) (set! ,name x))))
                               
(define-macro (memoize-file-accessor name)
  `(add-memo-table ',name make-memoized-file-accessor
                   ,name (lambda (x) (set! ,name x))))

