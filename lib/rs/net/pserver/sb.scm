
;;,(use app.sourcebase)

(define (bind-to-root (self <pserver-connection>) root)
  (let ((parts (string-split root #\/)))
    (if (and (>= (length parts) 4)
             (string=? (car parts) "")
             (string=? (cadr parts) "sb"))
        (let ((fam (table-lookup *pserver-families* (caddr parts))))
          (if fam
              (make <root-binding>
                    family: fam
                    filesystem: (string->filesystem (list-ref parts 3))
                    path-to-top: (steps->fs-path (list-tail parts 4) #t))
              (em 808 "No family ~s available" (caddr parts))))
        (em 809 "Cannot bind to root ~s" root))))

;;;

(define (build-up-to-date-table root dirs)
  (let ((all-nodes (enumerate-nodes
                    (filesystem root)
                    (map (lambda (d)
                           (fs-append-path (path-to-top root)
                                           (string->fs-path d)))
                         dirs)))
        (base (steps->fs-path (list "sb"
                                    (name (family root))
                                    (name (filesystem root)))
                              #t))
        (upto (make-string-table))
        (seq (make-dequeue)))
    ;;
    (for-each
     (lambda (p)
       (let* ((type (car p))
              (path (cadr p))
              (node (caddr p))
              (k (fs-append-path base path))
              (item (case type
                      ((dir)
                       (make <inventory-item>
                             properties: (vector 'path path)
                             name: (fs-tail-path path)
                             version: 'dir
                             status: #f))
                      ((file)
                       (make <inventory-item>
                             properties: (vector 'path path
                                                 'node node)
                             name: (fs-tail-path path)
                             version: (to-string (version-tag node))
                             status: #f)))))
         (dequeue-push-back! seq (to-string k))
         (table-insert! upto (to-string k) item)))
     all-nodes)
    (values upto seq)))

(define (print-outer-table-join inven upto)
  (define (strip-pre f)
    (string-join #\/ (cddddr (string-split f #\/))))
  ;;
  (let ((r '()))
    (table-join inven upto
                (lambda (k1 v1)
                  (set! r (cons (list (strip-pre k1) (version v1) "-") r)))
                (lambda (k2 v2)
                  (set! r (cons (list (strip-pre k2) "-" (version v2)) r)))
                (lambda (k1 v1 k2 v2)
                  (set! r (cons (list (strip-pre k1) (version v1) (version v2)) r))))
    (display-table '("File" "Client" "Target")
                   (sort r (lambda (a b)
                             (string<? (car a) (car b)))))))
;;

(define (generate-update-plan inven root dirs)
  (bind ((upto upto-seq (build-up-to-date-table root dirs))
         (plan (make-string-table)))
    ;;
    (print-outer-table-join inven upto)
    ;(print inven)
    ;(print upto)
    ;;
    (table-join inven upto
                (lambda (k1 v1) ; entry in `inven' but not in `upto'
                  (values))
                (lambda (k2 v2) ; entry in `upto' but not in `inven'
                  (if (eq? (version v2) 'dir)
                      (set-status! v2 'clear-static)
                      (set-status! v2 'create))
                  (table-insert! plan k2 v2))
                (lambda (k1 v1 k2 v2) ; entry in both
                  (case (status v1)
                    ((modified)
                     (if (string=? (version v1) (version v2))
                         (set-status! v2 'modified)
                         (set-status! v2 'merge)))
                    ((unchanged)
                     (if (string=? (version v1) (version v2))
                         (set-status! v2 'noop)
                         (set-status! v2 'update))))
                  (table-insert! plan k2 v2)))
    ;;
    (let ((result (make-dequeue)))
      (vector-for-each
       (lambda (s)
         (let ((p (table-lookup plan s)))
           (if p (dequeue-push-back! result p))
           (values)))
       (dequeue-state upto-seq))
      (vector->list (dequeue-state result)))))

    ;;
#|
    (let ((q (make-dequeue))
          (all-nodes (enumerate-nodes
                      (filesystem root)
                      (map (lambda (d)
                             (fs-append-path (path-to-top root)
                                             (string->fs-path d)))
                           dirs)))
          (base (steps->fs-path (list "sb"
                                      (name (family root))
                                      (name (filesystem root)))
                                #t)))
      ;;
      (for-each
       (lambda (p)
         (let ((type (car p))
               (path (cadr p))
               (node (caddr p)))
           (let* ((k (fs-append-path base path)))
             (dm 840 "Plan ~s" k)
             (case type
               ((dir)
                (dequeue-push-back! q (list 'clear-static path)))
               ((file)
                (dequeue-push-back! q (list 'create path node))))))
         (values))
       all-nodes)
|#

(define (cvs-pathname (self <pserver-connection>) f)
  (let ((root (get-property self 'root))
        (dir (get-property self 'directory)))
    (format #f "~a/\n/sb/~a/~a~a"
            (fs-parent-path f)
            (name (family root))
            (name (filesystem root))
            (fs-append-path (path-to-top root) f))))
  
;;;


(define *pserver-families* (make-string-table))

(define (pserver-add-family name)
  (let ((f (make <pserver-family>
                 name: name)))
    (table-insert! *pserver-families* name f)
    f))

