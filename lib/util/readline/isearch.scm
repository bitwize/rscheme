(define *isearch-keymap* (make-table char=? char->hash))

(table-insert! *isearch-keymap* #\C-g 'quit)
(table-insert! *isearch-keymap* #\newline 'commit)
(table-insert! *isearch-keymap* #\return 'commit)
(table-insert! *isearch-keymap* #\C-h 'undo)
(table-insert! *isearch-keymap* #\del 'undo)
(table-insert! *isearch-keymap* #\C-s 'isearch-forward)
(table-insert! *isearch-keymap* #\C-r 'isearch-backward)

(for-each
 (lambda (inherit)
   (for-each
    (lambda (cb)
      (if (eq? (cdr cb) inherit)
          (table-insert! *isearch-keymap* (car cb) (cdr cb))))
    (char-branch *default-keymap*)))
 '(right left
   beginning-of-line end-of-line
   history-prev history-next
   suspend-process
   interrupt-process
   destroy-process))

(for-each (lambda (code)
            (table-insert! *isearch-keymap* 
                           (integer->char code)
                           'insert-right))
          (list-tail (range 127) 32))

(define (show-isearch-line buf dir out 
                           (stack <list>)
                           (line <fixnum>)
                           (col <fixnum>)
                           (lines <vector>))
  (write-string out "\033[L\015")
  (case dir
    ((backward)
     (write-string out "(reverse-i-search)`"))
    ((forward)
     (write-string out "(forward-i-search)`")))
  (write-string out (list->string (reverse! (map car stack))))
  (write-string out "': ")
  ;(format out "[~d:~d] " line col)
  ;;
  (let ((t (car (vector-ref lines line))))
    (write-string out (substring t 0 col))
    (write-string out "\033[43;4m") ;; underline + background yellow
    (write-string out (substring t col (min (string-length t)
                                            (+ col (length stack)))))
    (write-string out "\033[m") ;; end
    (write-string out (substring t (min (string-length t)
                                        (+ col (length stack)))))
    (let ((goback (- (string-length t) col)))
      (if (not (zero? goback))
          (write-string out (format #f "\033[~dD" goback)))))
  ;;
  (flush-output-port out))


(define (isearch buf dir inp out)
  (let ((lines (vector-append (list->vector (reverse (buffer-prev buf)))
                              (vector (buffer-current-state buf))
                              (list->vector (buffer-succ buf)))))
    
    ;;
    (define (scan line stack)
      (let ((d (if (eq? dir 'forward) 1 -1))
            (t (list->string (reverse! (map car stack)))))
        (let loop ((j line))
          (if (and (>= j 0) (< j (vector-length lines)))
              (let ((x (string-search (car (vector-ref lines j)) t)))
                (if x
                    (values j x)
                    (loop (+ j d))))
              (values)))))
    ;;
    (let loop ((stack '())
               (line (length (buffer-prev buf)))
               (col (cdr (buffer-current-state buf))))
      ;;
      (define (normal-mode)
        ;; switch to normal (non-isearch mode)
        (write-string out "\033[L\015")
        (write-string out "\033\.7") ;; save cursor
        ;; commit the changes
        (set-buffer-prev! buf (reverse! (vector->list (subvector lines 0 line))))
        (set-buffer-succ! buf (vector->list (subvector lines (+ line 1))))
        ;; note that this also renders the state on the terminal
        (buffer-load-state buf (cons (car (vector-ref lines line))
                                     col)))
      ;;
      (show-isearch-line buf dir out stack line col lines)
      ;;
      (let* ((ch (read-char inp))
             (entry (table-lookup *isearch-keymap* ch)))
        (case entry
          ((quit) 
           (write-string out "\033[L\015")
           (write-string out "\033\.7") ;; save cursor
           (buffer-load-state buf (buffer-current-state buf))
           (values))
          ((commit)
           (normal-mode))
          ((undo)
           (if (pair? stack)
               (loop (cdr stack)
                     (cadar stack)
                     (caddar stack))
               (loop stack line col)))
          ((insert-right)
           (bind ((s (cons (list ch line col) stack))
                  (l c (scan line s)))
             (if l
                 (loop s l c)
                 (loop s line col))))
          ((isearch-forward)
           (if (eq? dir 'forward)
               ;; find next
               (bind ((l c (scan (min (+ line 1)
                                      (- (vector-length lines) 1))
                                 stack)))
                     (if l
                         (loop stack l c)
                         (loop stack line col)))
               ;; switch directions
               (begin
                 (set! dir 'forward)
                 (loop stack line col))))
          ((isearch-backward)
           (if (eq? dir 'backward)
               ;; find previous
               (bind ((l c (scan (max (- line 1) 0) stack)))
                     (if l
                         (loop stack l c)
                         (loop stack line col)))
               (begin
                 ;; switch directions
                 (set! dir 'backward)
                 (loop stack line col))))
          (else
           (let ((fn (and (symbol? entry) 
                          (table-lookup *default-fnmap* entry))))
             (if fn
                 (begin
                   ;; commit the changes, then execute the command
                   (normal-mode)
                   ;; execute the command
                   (fn buf (list ch)))
                 (loop stack line col)))))))))
      

(defun (isearch-backward)
  (isearch buf 'backward (input-port buf) (output-port buf)))
  ;(save-cursor)
  ;(load-state (current-state))

(defun (isearch-forward)
  (isearch buf 'forward (input-port buf) (output-port buf)))
;  (save-cursor)
;  (load-state (current-state)))

(insert-binding! *default-keymap* '(#\C-s) 'isearch-forward)
(insert-binding! *default-keymap* '(#\C-r) 'isearch-backward)
