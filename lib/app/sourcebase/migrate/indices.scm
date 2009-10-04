
(define cmvc-component #f)
(define cmvc-defect #f)
(define cmvc-file #f)
(define cmvc-level #f)
(define cmvc-path #f)
(define cmvc-release #f)
(define cmvc-track #f)
(define cmvc-user #f)
(define cmvc-version #f)

(define (cmvc-table name)
    (let ((a (assq name (vector-ref *migration-state* 3))))
	(if a
	    (cdr a)
	    (error "No `~a' table" name))))

(define (make-one-index values)
  (let ((tbl (make-table eq? integer->hash)))
    (for-each (lambda (v)
                (table-insert! tbl (id v) v))
              values)
    tbl))

(define-syntax (define-cmvc-index lookup-proc ixslot name src)
  (set! lookup-proc (lambda (key)
     (or (table-lookup (vector-ref *migration-state* ixslot) key)
         (error "No `~a' with id ~s" (mquote name) key))))
  (add-index-create-proc!
     (lambda ()
       (vector-set! *migration-state* 
       		    ixslot
		    (make-one-index (cmvc-table (mquote src)))))))

(define *index-create-procs* '())

(define (add-index-create-proc! proc)
  (set! *index-create-procs* (append *index-create-procs* (list proc))))

(define (migrate-cmvc-indices)
  (for-each (lambda (thunk) (thunk))
            *index-create-procs*))

(define-cmvc-index cmvc-component 4 Component Components)
(define-cmvc-index cmvc-defect 5 Defect Defects)
(define-cmvc-index cmvc-file 6 File Files)
(define-cmvc-index cmvc-level 7 Level Levels)
(define-cmvc-index cmvc-path 8 Path Path)
(define-cmvc-index cmvc-release 9 Release Releases)
(define-cmvc-index cmvc-track 10 Track Tracks)
(define-cmvc-index cmvc-user 11 User Users)
(define-cmvc-index cmvc-version 12 Version Versions)
