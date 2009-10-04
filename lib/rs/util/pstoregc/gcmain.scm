
(define *verbose* #t)

(load "state.scm")

;;;
;;;  this is the procedure we run as a subprocess
;;;

(define (usage-error)
  (format (current-error-port)
          "Usage: gc [-b <statefile>] [-w <work>] [-i <volume> ...]\n")
  (process-exit 2))

(define (getarg args opt)
  (if (and (> (string-length (car args)) 1)
           (string=? (substring (car args) 0 2) opt))
      (if (> (string-length (car args)) 2)
          (values (substring (car args) 2) (cdr args))
          (values (cadr args) (cddr args)))
      #f))

(define (main args)
  (let ((work-order #f)
        (backing-store #f)
        (gen #f))
    (let loop ((a args))
      (if (null? a)
          (resume-gc backing-store work-order)
          (if (string=? (car a) "-i")
              (init-gc backing-store work-order gen (cdr a))
              (if (string=? (car a) "-P")
                  (issue-final-report backing-store)
                  (if (string=? (car a) "-S")
                      (issue-summary-report backing-store)
                      (bind ((arg rest (getarg a "-b")))
                        (if arg
                            (begin
                              (set! backing-store arg)
                              (loop rest))
                            (bind ((arg rest (getarg a "-w")))
                              (if arg
                                  (begin
                                    (set! work-order (string->number arg))
                                    (loop rest))
                                  (bind ((arg rest (getarg a "-g")))
                                    (if arg
                                        (begin
                                          (set! gen (string->number arg))
                                          (loop rest))
                                        (usage-error))))))))))))))

(define (resume-gc backing-store work)
  (format #t ">>> Resuming GC state\n")
  (do-gc-work (restart-gc-cycle backing-store) work))

(define (init-gc backing-store work gen vols)
  (format #t ">>> Initializing GC state\n")
  (do-gc-work (create-gc-cycle
               backing-store
               gen
               (list->vector vols)
               (lambda (defpivots)
                 (defpivots 1 (vector <vector>
                                      <vector>
                                      <vector>))))
              work))

(define (issue-summary-report backing-store)
  (let* ((s (restart-gc-cycle backing-store))
         (ps (persistent-state s)))
    (report-page-stats s ps)
    (report-unused-pages s ps)))
  
(define (issue-final-report backing-store)
  (let* ((s (restart-gc-cycle backing-store))
         (ps (persistent-state s)))
    (report-white-objects s ps)
    (report-unused-pages s ps)))

(define (report-unused-pages ac ps)
  (vector-for-each
   (lambda (r)
     (if (not (or (eq? r 0)
                  (table-lookup (page-index ps) r)))
         (format #t "u ~08x\n" r)))
   (sort (lss-record-query (target-lss ac) '(0 . 0) '(15 . 0)) <)))

(define (report-page-stats ac ps)
  (let ((total-num-black 0)
        (total-bytes-black 0))
    (for-each
     (lambda (k)
       (let* ((pg (table-lookup (page-index ps) k))
              ((m <object-map>) (object-map pg))
              ((n <fixnum>) (gvec-length m))
              ((num-black <fixnum>) 0)
              ((bytes-black <fixnum>) 0)
              ((num-white <fixnum>) 0)
              ((bytes-white <fixnum>) 0)
              (offsets (vector-append
                        (vector-map (lambda (ent)
                                      (logical-shift-right ent 2))
                                    (clone2 m <vector>))
                        '#(8192))))
         ;; XXX note: we seem to assign too much to the 
         ;; last object of the page (i.e., for the page which is
         ;; currently being allocated from)
         (let loop (((i <fixnum>) 0))
           (if (fixnum<? i n)
               (let (((e <fixnum>) (gvec-ref m i))
                     ((size <fixnum>) (- (vector-ref offsets (+ i 1))
                                         (vector-ref offsets i))))
                 (if (eq? (bitwise-and e #b11) #b00)
                     (begin
                       (set! num-white (add1 num-white))
                       (set! bytes-white (fixnum+ bytes-white size)))
                     (begin
                       (set! num-black (add1 num-black))
                       (set! bytes-black (fixnum+ bytes-black size))))
                 (loop (add1 i)))
               (begin
                 (format #t "s ~08x ~d ~d ~d ~d ~d%\n"
                         (page-num pg)
                         num-black
                         bytes-black
                         num-white
                         bytes-white
                         (round (/ bytes-black 81.92)))
                 (set! total-num-black
                       (+ total-num-black num-black))
                 (set! total-bytes-black
                       (+ total-bytes-black bytes-black)))))))
     (sort (key-sequence (page-index ps)) <))
    (format #t "t ~d ~d ~d%\n" 
            total-num-black
            total-bytes-black
            (round (* 100.0
                      (/ total-bytes-black
                         (total-bytes-on-disk (target-lss ac))))))))

,(use syscalls)

(define (total-bytes-on-disk lss)
  (reduce + 0.0 
          (map (lambda (k)
                 (let ((f (lss-file lss k)))
                   (if f
                       (stat-size (stat f))
                       0)))
               (range 10))))

(define (report-white-objects ac ps)
  (for-each
   (lambda (k)
     (let* ((pg (table-lookup (page-index ps) k))
            ((m <object-map>) (object-map pg))
            ((n <fixnum>) (gvec-length m)))
       (let loop (((i <fixnum>) 0))
         (if (fixnum<? i n)
             (let (((e <fixnum>) (gvec-ref m i)))
               (if (eq? (bitwise-and e #b11) #b00)
                   (format #t "w ~08x ~04x\n" 
                           k
                           (logical-shift-right e 2)))
               (loop (add1 i)))))))
   (sort (key-sequence (page-index ps)) <)))

(define (do-gc-work (state <active-cycle>) work)
  (gc-status (persistent-state state))
  (let loop ()
    (let ((remain (do-scanning-work state (or work 1000))))
      (commit (backing-store state))
      (gc-status (persistent-state state))
      (if (<= remain 0)
          (if work
              (begin
                ;; still more to do, but we ran out our work order
                (format #t "%[Still more to do]%\n")
                (process-exit 3))
              (begin
                ;; still more to do, and we have leave to do it all
                (format #t "%[Still going]%\n")
                (loop)))
          (begin
            (format #t "%[All done]%\n")
            (process-exit 0))))))
