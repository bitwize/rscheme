

(define (loopy (module <module>) #optional n)
  (bind ((in0 out0 (make-internal-pipe 'test.stdin))
         (in1 out1 (make-internal-pipe 'test.stdout))
         (in2 out2 (make-internal-pipe 'test.stderr))
         (th (make-thread
              (lambda ()
                (with-edit-port in0 out1 out2
                                (if n
                                    (lambda ()
                                      (cmd-loop (top-level-envt module)
                                                "test[~d]=>"
                                                limit: n))
                                    (lambda ()
                                      (cmd-loop (top-level-envt module)
                                                "test[~d]=>"))))))))
    ;;
    (thread-resume th)
    ;;
    (values out0 in1 in2 th)))

(define *blockq* (make-dequeue))

(define (run-to-completion t check)
  (let loop ()
    (dequeue-push-back! *blockq* (thread-blocked-on t))
    (if (not (check))
        (begin
          (thread-sleep 0.01)
          (loop)))))

;;; `cmds' is a list of strings, each denoting a form to be
;;; passed through the REPL

;;;  This doesn't always work, e.g., when we're trying to
;;;  test SRFI-10, but it allows us to elide the final prompt
;;;  when it does work.  It probably doesn't work if there
;;;  is a (read) evaluation either...

(define (count-forms lines)
  (handler-case
   (let ((p (open-input-string (string-join #\newline 
                                            (select string? lines)))))
     (let loop ((i 0))
       (if (eof-object? (read p))
           i
           (loop (+ i 1)))))
   ((<condition>) #f)))

(define (repl-test-case cmds)
  (bind ((tmodule (get-module 'user))
         (out0 in1 in2 thread (loopy tmodule (count-forms cmds)))
         (q (make-dequeue))
         (last-prompt #f))
    ;;
    ;;  Find the last prompt in the given data, and save it aside
    ;;  (complete with markup), so that the "@reprompt" escape command
    ;;  can work
    ;;
    (define (remember-prompt data)
      (let loop ((i 0)
                 (j0 #f)
                 (j1 #f))
        (if (< i (vector-length data))
            (let ((q (vector-ref data i)))
              (if (equal? q '(markup start edit-prompt primary))
                  (loop (+ i 1) i #f)
                  (if (equal? q '(markup end edit-prompt primary))
                      (loop (+ i 1) j0 (+ i 1))
                      (loop (+ i 1) j0 j1))))
            (if (and j0 j1)
                (set! last-prompt 
                      `(stdout
                        ,@(vector->list (subvector data j0 j1))))))))
    ;;
    (define (flush-pipe name pipe)
      (let ((data (flush-internal-pipe pipe)))
        (if (> (vector-length data) 0)
            (begin
              (if (eq? name 'stdout) (remember-prompt data))
              (dequeue-push-back! q (cons name (vector->list data)))
              (values)))))
    ;;
    (define (flush-pipes)
      (flush-pipe 'stdout in1)
      (flush-pipe 'stderr in2))
    ;;
    (define (sync)
      (run-to-completion
       thread
       (lambda ()
         (or (thread-complete? thread)
             (is-waiting-for-internal-pipe? thread out0))))
      (flush-pipes))
    ;;
    (let loop ((l cmds)
               (first? #t))
      (if (null? l)
          (begin
            (if first? (sync))
            (vector->list (dequeue-state q)))
          (if (string? (car l))
              (begin
                (if first? (sync))
                (dequeue-push-back! q (list 'stdin (car l) "\n"))
                (format out0 "~a\n" (car l))
                (sync)
                (loop (cdr l) #f))
              (begin
                (if (eq? (caar l) 'esc)
                    (case (cadar l)
                      ((import)
                       (use-in (string->symbol (caddar l)) 
                               (top-level-envt tmodule)))
                      ((eval)
                       (eval-in-envt (read (open-input-string (caddar l)))
                                     (top-level-envt tmodule)))
                      ((note)
                       (dequeue-push-back! q (list 'note (caddar l)))
                       (values))
                      ((expect)
                       (let* ((exp (read (open-input-string (caddar l))))
                              (chk (eval-in-envt exp 
                                                 (top-level-envt tmodule))))
                         (if (not chk)
                             (begin
                               (format #t "  *** expectation failed: ~s\n" exp)
                               (dequeue-push-back! 
                                q
                                (list
                                 'note
                                 (sxml->string
                                  `(p (font (@ (color "red"))
                                            (b "Failed!"))
                                      " Expected "
                                      (code ,(caddar l))
                                      " to evaluate to true"
                                      ", but it did not."))))))))
                      ((shell)
                       (format #t "   >> system: ~s\n" (caddar l))
                       (with-module unixm (system (caddar l)))
                       (values))
                      ((reprompt)
                       (if last-prompt
                           (dequeue-push-back! q last-prompt))
                       (values))
                      (else
                       (error "Unknown escape command: ~s" (cadar l))))
                    (error "Unknown meta function: ~s" (caar l)))
                (loop (cdr l) first?)))))))

(define (repl-test-case->sxml cmds)
  (cons*
   'results
   `(@ (time ,(time->string (time) "%Y-%m-%d %H:%M:%S %Z"))
       (version ,(string-join #\. (map number->string
                                       *rscheme-version-list*)))
       (versionstr ,(with-module start *version*)))
   (rollup-top-level (repl-test-case cmds))))

;;;  the "top level" markup consists of alternations
;;;  between stdin, stdout, note, and other special commands, e.g.,
;;;
;;;    test-case-result =>
;;;       ((stdout (markup start ...) "test[0]=>" (markup end ...))
;;;        (stdin (markup start ...) "(cons 1 2)" (markup end ...))
;;;        (stdout (markup start ...) ...)
;;;        (note "<p>That really\n")
;;;        (note "was neat.</p>\n")
;;;       )

(define (rollup-top-level tlm)
  (cond
   ((null? tlm) '())
   ((eq? (caar tlm) 'note)
    (bind ((sxml rest (note-sequence->sxml tlm)))
      (cons (cons 'note sxml) 
            (rollup-top-level rest))))
   ((memq (caar tlm) '(stdin stdout))
    (bind ((sxml rest (io-sequence->sxml tlm)))
      (cons (cons 'io sxml)
            (rollup-top-level rest))))))


(define (io-sequence->sxml tlm)
  (let ((q (make-dequeue)))
    (let loop ((tlm tlm))
      (if (and (pair? tlm) (memq (caar tlm) '(stdin stdout)))
          (begin
            (case (caar tlm)
              ((stdin)
               (dequeue-push-back! q '(markup start stdin))
               (for-each (lambda (r)
                           (dequeue-push-back! q r)
                           (values))
                         (cdar tlm))
               (dequeue-push-back! q '(markup end stdin)))
              ((stdout)
               (for-each (lambda (r)
                           (dequeue-push-back! q r)
                           (values))
                         (cdar tlm))))
            (loop (cdr tlm)))
          (begin
            (values (treeify-markup (vector->list (dequeue-state q)))
                    tlm))))))

(define (note-sequence->sxml tlm)
  (let ((p (open-output-string)))
    (let loop ((tlm tlm))
      (if (and (pair? tlm) (eq? (caar tlm) 'note))
          (begin
            (write-string p (cadar tlm))
            (newline p)
            (loop (cdr tlm)))
          (values (string->sxml-sequence* (get-output-string p))
                  tlm)))))

(define (string->sxml-sequence* str)
  (map sxml:root-element (string->sxml-sequence str)))

(define (treeify-markup linear)
  ;;
  (define (app! stack item)
    (let ((top (car stack)))
      (set-cdr! top (append! (cdr top) (list item)))
      (values)))
  ;;
  (let loop ((stack (list (cons #f '())))
             (l linear))
    (if (null? l)
        (cdr (last stack))
        (let ((item (car l)))
          (if (pair? item)
              (case (car item)
                ((markup)
                 (case (cadr item)
                   ((start)
                    (let ((node (if (null? (cdddr item))
                                    (list (caddr item))
                                    (list (caddr item)
                                          (cons '@
                                           (map (lambda (attr)
                                                  (list attr "1"))
                                                (cdddr item)))))))
                      (app! stack node)
                      (loop (cons node stack) (cdr l))))
                   ((end)
                    (loop (cdr stack) (cdr l)))
                   (else
                    (error "bad markup ~s" (cadr item)))))
                (else
                 (error "bad item type ~s" (car item))))
              (if (string? item)
                  (begin
                    (app! stack item)
                    (loop stack (cdr l)))
                  (error "bad item ~s" item)))))))
