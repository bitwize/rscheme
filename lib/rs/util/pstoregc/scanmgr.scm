
(define-class <gc-process> (<object>)
  state-file
  use-bookmark
  (process-object init-value: #f)
  (process-status init-value: #f)
  (work-order init-value: 0))

(define $young-bookmark 1)
#|  

(define *young* (make <gc-process>
                      state-file: "/ram/donovan/young.gc"
                      use-bookmark: $young-bookmark
                      work-order: 1000))

|#
(define *gcscan* (string->file "[install]/bin/gcmain"))


(define (resume-gc! (p <gc-process>))
  (kick-gc-process p (lambda () '())))

(define (start-gc! (p <gc-process>))
  (kick-gc-process p
                   (lambda ()
                     `("-g" 
                       ,(use-bookmark p)
                       "-i" 
                       ,@*volume-set*))))

(define (kick-gc-process (p <gc-process>) more-args-thunk)
  (let ((args (append (list "-b" (state-file p)
                            "-w" (number->string (work-order p)))
                      (more-args-thunk))))
    (format #t "starting GCSCAN: ~s\n" args)
    (bind ((proc (apply run (pathname->os-path *gcscan*) args)))
      (set-work-order! p 0)
      (set-process-object! p proc)
      (set-process-status! p #f)
      ;;
      (thread-resume
       (make-thread (lambda ()
                      (let ((x (exit-status proc)))
                        (set-process-status! p (exit-status proc))
                        (format #t "GCSCAN - ~s\n" args)
                        (format #t "finished with GCSCAN: exit ~s\n" x)
                        (if (not (member x '(#(exited 2)
                                             #(exited 0))))
                            (process-exit 3))
                        (set-process-object! p #f)))
                    "wait:gc-scanner")))))

(define (process-gc-results (p <gc-process>))
  (let ((args (list "-b" (state-file p) "-P")))
    (format #t "starting GCEXTRACT: ~s\n" args)
    (set-process-status! p #f)
    ;(gc-now)
    (bind ((port proc (apply run->port (pathname->os-path *gcscan*) args)))
      (let loop ((i 0))
        (let ((l (read-line port)))
          (if (string? l)
              (begin
                ;(format #t "WHITE <~a>\n" l)
                (if (process-white-line l)
                    (loop (+ i 1))
                    (loop i)))
              (begin
                (format #t "GCEXTRACT exit status ~s\n" (exit-status proc))
                (if (not (equal? (exit-status proc) '#(exited 0)))
                    (process-exit 1))
                (format #t "          ~d new white objects\n" i)
                (unlink (state-file p)))))))))

(define process-white-line
  (let ((pat (reg-expr->proc '(entire (seq "w"
                                           (+ space)
                                           (save (+ hex-digit))
                                           (+ space)
                                           (save (+ hex-digit)))))))
    (lambda (line)
      (bind ((s e page offset (pat line)))
        (if s
            (let ((page-num (string->number page 16))
                  (offset (string->number offset 16)))
              ;; XXX don't handle page-size or larger objects yet...
              (if (> offset 0)
                  (begin
                    (location-deallocate *rstore* 
                                         page-num
                                         ;; nth=1 indirect=NO(0) first=YES(1)
                                         #b101
                                         offset)
                    #t)
                  #f))
            #f)))))


(define (do-gc-work (gcp <gc-process>) amt)
  (set-work-order! gcp (+ (work-order gcp) amt))
  (if (not (process-object gcp))
      (if (stat (state-file gcp))
          (resume-gc! gcp)
          (start-gc! gcp))))
