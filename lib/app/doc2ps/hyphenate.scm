(define *hyphenation-cache* (make-string-ci-table))
(define *hyphenation-exceptions* (make-string-ci-table))

(table-insert! *hyphenation-exceptions* "unresolved" "un-resolved")
(table-insert! *hyphenation-exceptions* "procedure" "proc-edure")
(table-insert! *hyphenation-exceptions* "user" "us-er")
(table-insert! *hyphenation-exceptions* "vector" "vec-tor")
(table-insert! *hyphenation-exceptions* "application" "app-lica-tion")
(table-insert! *hyphenation-exceptions* "confusion" "con-fu-sion")
(table-insert! *hyphenation-exceptions* "generate" "gen-er-ate")

(define (hyphenated para word)
  (let ((c (table-lookup *hyphenation-cache* word)))
    (cond
     ((eq? c #t)
      word)
     (c
      c)
     (else
      (let* ((a (table-lookup *hyphenation-exceptions* word)))
        (table-insert! *hyphenation-cache* word (or a #t))
        (hyphenated para word))))))
