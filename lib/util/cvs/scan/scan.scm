#|
   This is an RScheme module that knows how to scan the output of "cvs log".
   It provides an iteration interface over the CVS log, which iterates
   at two levels:
      1. file level
      2. revision level
|#

(define *working-file* 
  (reg-expr->proc 
   '(prefix (seq "Working file: "
                 (save (+ any))))))

(define *head-rev* 
  (reg-expr->proc 
   '(prefix (seq "head:" 
                 (* space)
                 (save (+ (not space)))))))

(define *dash-break* (reg-expr->proc '(entire (+ #\-))))
(define *double-break* (reg-expr->proc '(entire (+ #\=))))
(define *revision* (reg-expr->proc '(prefix (seq "revision " (save (+ (or #\. digit)))))))
(define *dateline* (reg-expr->proc 
                    '(prefix (seq "date: " (save (+ (not #\;)))
                                  ";" (+ space)
                                  "author: " (save (+ (not #\;)))
                                  ";" (+ space)
                                  "state: " (save (+ (not #\;)))))))
(define *branches* (reg-expr->proc
                    '(prefix (seq "branches: " (+ (not #\;)) #\;))))

(define *symbolic-names* (reg-expr->proc '(prefix "symbolic names:")))
(define *symbolic-name* (reg-expr->proc '(prefix
                                          (seq (+ space)
                                               (save (+ (not #\:)))
                                               #\:
                                               (* space)
                                               (save (+ (or #\. digit)))))))

(define *pub-name* (reg-expr->proc '(entire (seq "pub" 
                                                 (* (seq #\_ (+ digit)))))))

(define *status-line* (reg-expr->proc '(prefix 
                                        (seq "File:"
                                             (+ space)
                                             (save (+ (not space)))
                                             (+ space)
                                             "Status:"
                                             (+ space)
                                             (save (+ any))))))

;;;
;;;  `scan-cvs-logs'
;;;
;;;    - wf-start-proc: A procedure to be called when starting a WF
;;;         called with 2 arguments
;;;         - working-file: the working file name
;;;         - tag-list: the list of tag.  Each tag entry is a pair of
;;;                     the tag name and the revision number.  The
;;;                     head revision appears as a HEAD tag
;;;    - wf-end-proc: A procedure to be called when done with a WF
;;;         called with 2 arguments
;;;         - working-file: the value returned from wf-start-proc
;;;         - revs: a list of the return values from the invocations
;;;                 of the rev-proc for each consitutent revision
;;;    - rev-proc: A procedure to be called for each revision entry
;;;         - working-file: the value returned from wf-start-proc
;;;         - comment-lines: a list of comment lines
;;;         - revision: the revision string (e.g., "1.35.2.1")
;;;         - date: the date of the checkin (e.g., "2001/12/08 05:27:09")
;;;         - author: the author of the revision (e.g., "donovan")
;;;         - state: the state string (e.g., "Exp" or "dead")

(define (scan-cvs-logs port wf-start-proc wf-end-proc rev-proc)
  (letrec ((wf #f)
           (sn '())
           (headrev #f)
           (start (lambda ()
                    (let ((l (read-line port)))
                      (if (eof-object? l)
                          l
                          (bind ((s e x (*working-file* l)))
                            (if s
                                (begin
                                  (set! wf x)
                                  (start))
                                (bind ((s e h (*head-rev* l)))
                                  (if s
                                      (begin
                                        (set! headrev h)
                                        (start))
                                      (bind ((s e (*symbolic-names* l)))
                                        (if s
                                            (in-sym-names '())
                                            (if (*dash-break* l)
                                                (begin
                                                  (set! wf (wf-start-proc wf (cons (cons "HEAD" headrev) sn)))
                                                  (in-revs '()))
                                                (start))))))))))))
           (in-sym-names (lambda (lst)
                           (let ((l (read-line port)))
                             (bind ((s e k v (*symbolic-name* l)))
                               (if s
                                   (in-sym-names (cons (cons k v) lst))
                                   (begin
                                     (set! sn (reverse lst))
                                     (start)))))))
           (in-revs (lambda (revs)
                      (let ((l (read-line port)))
                        (bind ((s e x (*revision* l)))
                          (if s
                              (at-date revs x)
                              (if (*double-break* l)
                                  (start)
                                  (begin
                                    (format #t "?in-revs ~s\n" l)
                                    (start))))))))
           (at-date (lambda (revs rev)
                      (let ((l (read-line port)))
                        (bind ((s e date auth state (*dateline* l)))
                          (if s
                              (before-comments revs (list rev date auth state))
                              (begin
                                (format #t "?at-date ~s\n" l)
                                (start)))))))
           (before-comments (lambda (revs info)
                              (let ((l (read-line port)))
                                (cond
                                 ((*double-break* l)
                                  (fini-wf (cons (apply rev-proc wf '() info)
                                                 revs)))
                                 ((*dash-break* l)
                                  (fini-wf (cons (apply rev-proc wf '() info)
                                                 revs)))
                                 ((*branches* l)
                                  (before-comments revs info))
                                 (else
                                  (in-comments revs info (list l)))))))
           (in-comments (lambda (revs info cmts)
                          (let ((l (read-line port)))
                            (if (*double-break* l)
                                (fini-wf (cons (apply rev-proc
                                                      wf
                                                      (reverse cmts)
                                                      info)
                                               revs))
                                (if (*dash-break* l)
                                    (in-revs (cons (apply rev-proc
                                                          wf
                                                          (reverse cmts)
                                                          info)
                                                   revs))
                                    (in-comments revs info (cons l cmts)))))))
           (fini-wf (lambda (revs)
                      (wf-end-proc wf (reverse revs))
                      (start))))
    (start)))
