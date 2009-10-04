;;;
;;;  Initialize a scanner character table
;;;

(define (specialized (fn <function>) (port <<class>>))
  fn)

(define (insert-all! (t <char-table>) fn port-class chars)
  (let ((s (specialized fn port-class)))
    (vector-for-each
     (lambda (ch)
       (table-insert! t ch s))
     chars)))


(define (make-scanner-table (port-class <<class>>))
  (let ((t (make-char-table)))
    ;;
    ;; install whitespace characters
    ;;
    (insert-all! t scan-whitespace port-class
		 '#(#\space #\tab #\ff #\newline))
    ;;
    ;; install delimiter characters
    ;;
    (table-insert! t #\( scan-open-paren)
    (table-insert! t #\) scan-close-paren)
    (table-insert! t #\[ scan-open-sqbracket)
    (table-insert! t #\] scan-close-sqbracket)
    (table-insert! t #\{ scan-curly)
    (table-insert! t #\` scan-backquote)
    (table-insert! t #\' scan-quote)
    (table-insert! t #\, scan-unquote)
    ;;
    ;; other tokens
    ;;
    (table-insert! t #\" scan-string-token)
    (table-insert! t #\# scan-sharp)
    (table-insert! t #\; scan-line-comment)
    ;;
    ;; install identifier characters
    ;;
    (insert-all! t scan-identifier port-class
		 (key-sequence *id-initial*))
    ;;
    ;; install number characters
    ;;
    (insert-all! t scan-number port-class
		 (key-sequence *num-initial*))
    ;;
    ;; assuming we're not in pedantic mode, an initial ':' becomes
    ;; a flag indicator
    ;;
    (insert-all! t scan-flag port-class '#(#\:))
    ;;
    t))

