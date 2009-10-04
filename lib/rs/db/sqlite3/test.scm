

(define (scan)
  (let ((d (sqlite3-open "/tmp/vdata.db")))
    ;;
    (let ((s (sqlite3-prepare d "select addr, ar_time, ar_host from acache" 
                              0)))
      (sqlite3-for-each
       s
       (lambda (x y z)
         (let ((x (bvec->venti-ptr x)))
           (format #t "==> ~s ~s ~s\n" x y z))))
      (sqlite3-finalize s)
      (sqlite3-close d))))

(define (tree d)
  (let ((s (sqlite3-prepare d "select data from cam where addr = ?" 0)))
    ;;
    (define (get (p <venti-ptr>))
      (sqlite3-bind s p)
      (if (sqlite3-step s)
          (sqlite3-row s)
          #f))
    ;;
    get))

,(use rs.net.ubf)

#|
(define (g)
  (let* ((d (sqlite3-open "/tmp/vdata.db"))
         (g (tree d))
         (p (string->venti-ptr
             "a,4f72e3d3886f78f56c07a19850abe4aa2e8788e0bec562d83ae25327bba6"))
         (a (g p)))
    ;;
    ;(print a)
    ;;
    (read-ubf a)))
|#

(define (g)
  (let* ((d (sqlite3-open "/tmp/v2.dat"))
         (g (tree d)))
    (scan-archive
     (string->venti-ptr
      "a,c37636d0c5b7fc05a104fdf65514390a57986fa0dd1f1d6d7f41e70e0ab8")
     (tree d))))

(define ubfb (load-ubf-b-spec "/u/donovan/p/venti/aux/venti/protocol.ubf"))

(define check:archive (generate-ubf-checker/scheme ubfb 'archive))

(define-class <archive-info> (<object>)
  pointer
  host
  top
  timestamp)

(define (scan-archive (p <venti-ptr>) load)
  (let ((a (read-ubf (or (load p)
                         (error "~a: No data" p)))))
    (assert (check:archive a))
    ;;
    (bind ((i (archive-info p a)))
      ;;
      (format #t "~a:~a   ~a\n"
              (host i)
              (top i)
              (timestamp i)))))
 

(define (ubf-assq l k)
  (let loop ((l l))
    (if (null? l)
        #f
        (if (eq? (vector-ref (car l) 0) k)
            (vector-ref (car l) 1)
            (loop (cdr l))))))

(define (archive-info ptr (a <vector>))
  (let* ((p (vector-ref a 4)))
    ;;
    (make <archive-info>
          pointer: ptr
          host: (or (ubf-assq p 'origin-host)
                    (vector-ref a 1))
          top: (or (ubf-assq p 'origin-top)
                   (vector-ref a 3))
          timestamp: (intern-time (or (ubf-assq p 'duplicity-time)
                                      (ubf-assq p 'origin-time)
                                      (vector-ref a 2))))))



(define-method intern-time ((self <number>))
  (epoch-seconds->time self))

(define-method intern-time ((self <string>))
  (string->time self))

    
  
  

    
#|
(define d (sqlite3-open "/tmp/vdata.db"))
(define g (tree d))
|#
