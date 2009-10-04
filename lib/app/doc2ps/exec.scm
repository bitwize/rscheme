(load "main.scm")

(get-afm "Palatino-Roman")
(get-afm "Palatino-Italic")
(get-afm "Palatino-Bold")
(get-afm "Helvetica-Bold")
(get-afm "Helvetica")
(get-afm "Minion-Condensed")
(get-afm "Minion-CondensedItalic")
(get-afm "Minion-BoldCondensed")
(get-afm "Minion-BoldCondensedItalic")

;;;
;;;  And a main entry point...
;;;

(define (sigint)
  (format (current-error-port) "\ndocbook2ps: Interrupted\n")
  (process-exit 1))

,(use syscalls)

(define *build-time* (time))

(define (process-doc-arg files)
  (format #t "docbook2ps (0.5.1, ~a)\n" 
          (time->string *build-time* "%Y-%m-%d %H:%M"))
  (let ((logf (change-extension (last files) "log")))
    (with-output-to-file
        logf
      (lambda ()
        (progress "Detailed log output to: ~a\n" logf)
        (handler-case
         (apply files->ps files)
         ((<condition> condition: err)
          (format (current-error-port)
                  "\n\n*** Error processing ~s\n*** ~a" 
                  files
                  err)
          (with-module repl (apply-backtrace err))))))))

(define *chapter-list* #f)

(define *default-stylesheet* (pathname->os-path
                              (append-path
                               (current-absolute-directory)
                               (string->file "bookstyles.scm"))))

(define (main args)
  (if (equal? args '("-repl"))
      (with-module repl (main '()))
      (let ((stylesheet #f))
        (with-module start (register-c-signal-handler! 'sigint sigint))
        (let loop ((a args))
          (cond
           ((null? a)
            (usage))
           ;;
           ((string=? (car a) "-h")
            (format #t "usage: doc2ps [-S stylesheet] [-ch CHAP] source.sgml ...")
            (process-exit 0))
           ;;
           ((string=? (car a) "-sgmls")
            (set! *sgmls-command-line* (cadr a))
            (loop (cddr a)))
           ;;
           ((string=? (car a) "-S")
            (set! stylesheet (cadr a))
            (loop (cddr a)))
           ((string=? (car a) "-e")
            (load (cadr a))
            (loop (cddr a)))
           ;;
           ((string=? (car a) "--show-outlines")
            (set! *show-outlines* #t)
            (loop (cdr a)))
           ;;
           ((string=? (car a) "-c")
            (set! *sgmls-command-line* (format #f "~a -c ~a"
                                               *sgmls-command-line*
                                               (cadr a)))
            (loop (cddr a)))
           ;;
           ((string=? (car a) "-ch")
            (set! *chapter-list* (append (or *chapter-list* '())
                                         (list (cadr a))))
            (loop (cddr a)))
           ;;
           (else
            (let ((s (or stylesheet *default-stylesheet*)))
              (format #t "Loading stylesheet: ~s\n" s)
              (load s))
            (process-doc-arg a)))))))

(define (usage)
  (format (current-error-port) "usage: docbook2ps file.sgml ...\n")
  (process-exit 1))
  
