
;;;
;;;  This policy stuff is driving me batty!
;;;
;;;  This file implements a very simple, non-generational policy
;;;
;;;  1. There are two volumes, [0] and [1]
;;;
;;;  2. Initially, writes go to volume [0]
;;;
;;;  3. When volume [0] reaches a threshold size X,
;;;     a scan is started.
;;;
;;;  4. When the scan is complete, the white objects
;;;     are freed and [0] is packed into [1]
;;;
;;;  5. Cycle repeats, with V0 and V1 switching roles
;;;

(define *rstore* #f)
(define *lss* #f)
(define *active* 0)
(define *volume-set* '())

(define (init-volume-set)
  (let loop ((vs '())
             (i 0))
    (let ((f (lss-file *lss* i)))
      (if f
          (loop (cons f vs) (+ i 1))
          (set! *volume-set* (reverse vs))))))

(define *threshold* 100000)

(define *scanner* #f)

(define (init-compaction (lss <lss>)
                         (ps <persistent-store>)
                         (gc-state-file <string>)
                         . rest)
  (if (stat gc-state-file)
      (unlink gc-state-file))
  (if (pair? rest)
      (set! *gcscan* (string->file (string-append (car rest) "gcmain"))))
  (set! *scanner* (make <gc-process>
                        state-file: gc-state-file
                        use-bookmark: $young-bookmark
                        work-order: 1000))
  (set! *rstore* ps)
  (set! *lss* lss)
  (set! *active* (lss-get-tip *lss*))
  (init-volume-set)
  (values))

;;;

(define *status* 'init)
(define *last-size* 0)

(define (did-commit)
  (let ((l (lss-get-vol-size *lss* *active*)))
    (format #t "did commit: ~d (~a)\n" l *status*)
    (if (> l *threshold*)
        (case *status*
          ((init)
           (pack-it-all)
           (set! *status* 'run))
          ((run)
           (if (equal? (process-status *scanner*) '#(exited 0))
               (begin
                 (set! *last-size* 0)
                 (process-gc-results *scanner*)
                 (pack-it-all)
                 (set! *status* 'done))
               (begin
                 (do-gc-work *scanner* (- l *last-size*))
                 (set! *last-size* l))))
          ((done)
           (set! *status* 'run))))))

(define (pack-it-all)
  (let ((x (case *active*
             ((0) (lss-record-query *lss* '(0 . 0) '(0 . #x1FFFFFFF)))
             ((1) (lss-record-query *lss* '(1 . 0) '(1 . #x1FFFFFFF))))))
    ;;
    (set! *active* (- 1 *active*))
    (lss-set-tip *lss* *active*)
    (format #t "(packing ~d records onto vol.~d)\n" (vector-length x) *active*)
    ;;
    (vector-for-each
     (lambda (r)
       (lss-move-record *lss* *active* r))
     x)
    ;;
    (lss-commit *lss* $young-bookmark)          ; Bookmark #1
    (lss-commit *lss*)                          ; force the CR onto the new vol
    (let ((f (lss-file *lss* (- 1 *active*))))
      (lss-detach-vol *lss* (- 1 *active*))
      (if (stat f) 
          (unlink f))
      (lss-attach-vol *lss* (- 1 *active*) f))
    (lss-set-tip *lss* *active*)))

