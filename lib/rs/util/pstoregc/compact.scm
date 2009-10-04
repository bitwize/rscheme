
;;;
;;;   This is the part of the GC that runs within the
;;;   application; it manages compaction of the LSS
;;;   volumes and initiation of object-GC cycles.
;;;

;;;   Currently, we are hard-coded to support 5 volumes:
;;;
;;;     _____
;;;    (     )
;;;    |-----|                  ; nursery
;;;    |  4  |
;;;    (_____)
;;;
;;;
;;;     _____      _____
;;;    (     )    (     )
;;;    |-----|    |-----|       ; young
;;;    |  2  |    |  3  |
;;;    (_____)    (_____)
;;;
;;;
;;;     _____      _____
;;;    (     )    (     )
;;;    |-----|    |-----|       ; old
;;;    |  0  |    |  1  |
;;;    (_____)    (_____)
;;;
;;;

(define *lss* #f)

(define *size-bounds* '#(1000000 1000000 400000 200000 100000))

;;;

(define *volume-set* '())

(define (init-volume-set)
  (let loop ((vs '())
             (i 0))
    (let ((f (lss-file *lss* i)))
      (if f
          (loop (cons f vs) (+ i 1))
          (set! *volume-set* (reverse vs))))))

(define (init-compaction lss)
  (set! *lss* lss)
  (clear-pack)
  ;;
  (init-volume-set)
  (thread-resume (make-thread young-compaction "young-compaction"))
  (values))
      

