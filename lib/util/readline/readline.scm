,(use syscalls sort)

(define (xterm-readline prompt inp out (state <readline-state>))
  (let ((reset-thunk (init-for-readline 0)))
    (if (not (ever-read-from? state))
	(begin
	  (on-exit reset-thunk)
	  (set-ever-read-from?! state #t)))
    (let ((s (xterm-readline* prompt inp out state)))
      (reset-thunk)
      s)))

;;;

(define (substring->dequeue (str <string>) (from <fixnum>) (to <fixnum>))
  (if (eq? from to)
      (make-dequeue)
      (let (((v <vector>) (make-vector (max 4 (+ (- to from) 1)))))
	(let loop (((i <fixnum>) 0)
		   ((x <fixnum>) from))
	  (if (eq? x to)
	      (make <dequeue>
		    state: v
		    front: 0
		    back: i)
	      (begin
		(vector-set! v i (string-ref str x))
		(loop (add1 i) (add1 x))))))))

;;;

(define-constant *default-fnmap* (make-symbol-table))
(define-constant *default-keymap* (make <key-state-map>))

(insert-binding! *default-keymap* '(#\esc) *esc-key-map*)

(insert-binding! *default-keymap* '(#\C-f) 'right)
(insert-binding! *default-keymap* '(#\C-b) 'left)
(insert-binding! *default-keymap* '(#\C-o) 'sponsor)
(insert-binding! *default-keymap* '(#\return) 'enter)
(insert-binding! *default-keymap* '(#\newline) 'enter)
(insert-binding! *default-keymap* '(#\C-a) 'beginning-of-line)
(insert-binding! *default-keymap* '(#\C-e) 'end-of-line)
(insert-binding! *default-keymap* '(#\tab) 'tab-completion)
(insert-binding! *default-keymap* '(#\C-k) 'kill-to-eol)
(insert-binding! *default-keymap* '(#\C-u) 'delete-line)
(insert-binding! *default-keymap* '(#\del) 'delete-left)
(insert-binding! *default-keymap* '(#\C-h) 'delete-left)
(insert-binding! *default-keymap* '(#\C-d) 'delete-right-or-eof)
(insert-binding! *default-keymap* '(#\C-_) 'undo)
(insert-binding! *default-keymap* '(#\C-p) 'history-prev)
(insert-binding! *default-keymap* '(#\C-n) 'history-next)

(insert-binding! *default-keymap* '(#\C-z) 'suspend-process)
(insert-binding! *default-keymap* '(#\C-c) 'interrupt-process)
(insert-binding! *default-keymap* '(#\C-\) 'destroy-process)

(for-each (lambda (code)
            (insert-binding! *default-keymap* 
                             (list (integer->char code))
                             'insert-right))
          (list-tail (range 127) 32))

(define (make-empty-queue) (make-dequeue))

(define-class <readline-activation> (<object>)
  (input-port :sealed)
  (output-port :sealed)
  (buffer-prompt :sealed)
  (buffer-state :sealed)
  (buffer-blink-posn :sealed init-value: #f)
  (buffer-return-proc :sealed)
  (buffer-left :sealed init-function: make-empty-queue)
  (buffer-right :sealed init-function: make-empty-queue)
  (buffer-tab-time :sealed init-value: #f)
  (buffer-fnmap :sealed init-value: *default-fnmap*)
  (buffer-keymap :sealed init-value: *default-keymap*)
  (buffer-succ :sealed init-value: '())
  (buffer-prev :sealed init-value: '())
  (buffer-undo :sealed init-value: '()))

(define (buffer-return buf value)
  ((buffer-return-proc buf) value))

(define (buffer-exec fn self ks . extra)
  (apply (table-lookup (buffer-fnmap self) fn) self ks extra))

(define (buffer-current-state buf)
  (cons (list->string
         (vector->list
          (vector-append (dequeue-state (buffer-left buf))
                         (dequeue-state (buffer-right buf)))))
        (dequeue-count (buffer-left buf))))

(define-macro (with-buf-macros . body)
  `(let-syntax ((right (else (buffer-right buf)))
                (left (else (buffer-left buf)))
                (out (else (output-port buf)))
                (in (else (input-port buf)))
                (undo (else (buffer-undo buf)))
                (prompt (else (buffer-prompt buf)))
                (control (syntax-form (str)
                           (write-string (output-port buf) 
                                         str))))
     (letrec-syntax 
         ((clear-to-eol
           (syntax-form () (control "\033[K")))
          (beep
           (syntax-form () (control "\7")))
          (insert-right 
           (syntax-form (ks)
             ((table-lookup (buffer-fnmap buf) 'insert-right)
              buf
              ks)))
          (current-state
           (syntax-form () (buffer-current-state buf)))
          (exec
           (syntax-form (n) (buffer-exec (mquote n) buf keyseq)))
          (move-left
           (syntax-form () (exec move-left)))
          (move-right
           (syntax-form () (exec move-right)))
          (delete-right
           (syntax-form () (exec delete-right)))
          (save-cursor
           (syntax-form () (control "\033\.7")))
          (load-state
           (syntax-form (s) (buffer-load-state buf s)))
          (can-undo
           (syntax-form ()
             (set-buffer-undo! buf 
                               (cons (current-state)
                                     (buffer-undo buf)))))
          )
       (begin ,@body))))

(define-macro (defun (name . args) . body)
  `(table-insert! *default-fnmap*
                  ',name
                  (lambda ',name ((buf <readline-activation>) keyseq ,@args)
                          (with-buf-macros ,@body))))

(defun (move-left)
  (set-buffer-tab-time! buf #f)
  (dequeue-push-front! right (dequeue-pop-back! left))
  (control "\10"))
;
(defun (move-right)
  (set-buffer-tab-time! buf #f)
  (dequeue-push-back! left (dequeue-pop-front! right))
  (control "\033[C"))

;

(defun (insert-right)
  (set-buffer-tab-time! buf #f)
  (for-each
   (lambda (ch)
     (if (not (dequeue-empty? right))
         (control "\033[@"))
     (write-char ch (output-port buf))
     (dequeue-push-back! left ch)
     (values))
   keyseq))

;
(defun (delete-right)
  (set-buffer-tab-time! buf #f)
  (dequeue-pop-front! right)
  (control "\033[P"))

;

(define (slide-cursor out delta)
  (if (< delta 0)
      (format out "\033[~dD" (- delta))
      (format out "\033[~dC" delta)))

(define (buffer-char-at (buf <readline-activation>) posn)
  (let* ((left (buffer-left buf))
         (n (dequeue-count left)))
    (if (< posn n)
        (dequeue-ref left posn)
        (dequeue-ref (buffer-right buf) (- posn n)))))

(define (buffer-unblink-paren (buf <readline-activation>))
  ;; go un-hilight the old matching paren, if there was one
  (if (buffer-blink-posn buf)
      (let ((out (output-port buf))
            (delta (- (dequeue-count (buffer-left buf))
                      (buffer-blink-posn buf))))
        (debug "   unblink delta ~d\n" delta)
        (slide-cursor out (- delta))
        ;(write-string out "\033[P")   ; delete-right
        (write-char (buffer-char-at buf (buffer-blink-posn buf)))
        (slide-cursor out (- delta 1))
        (set-buffer-blink-posn! buf #f))))

(define (find-paren-matching buf n)
  (let ((left (buffer-left buf)))
  (debug "find-paren-matching in: ~s from ~s\n" left n)
  (let loop ((k n)
             (depth 0))
    (debug "   find-paren-matching... k = ~d, depth = ~d\n" k depth)
    (if (> k 0)
        (let ((ch (dequeue-ref left (- k 1))))
          (if (char=? ch #\()
              (if (= depth 0)
                  (- k 1)
                  (loop (- k 1) (- depth 1)))
              (if (char=? ch #\))
                  (loop (- k 1) (+ depth 1))
                  (loop (- k 1) depth))))
        #f))))

(define (buffer-blink-paren (buf <readline-activation>))
  (buffer-unblink-paren buf)
  (let* ((q (buffer-left buf))
         (n (dequeue-count q)))
    (if (and (> n 0)
             (char=? (dequeue-ref q (- n 1)) #\)))
        (let ((k (find-paren-matching buf (- n 1))))
          (debug "find-paren-matching => ~s\n" k)
          (if k
              (let ((delta (- n k))
                    (out (output-port buf)))
                (debug "   blink delta ~d\n" delta)
                (slide-cursor out (- delta))
                ;(write-string out "\033[P")   ; delete-right
                (write-string out "\033[1m") ;; bold
                (write-string out "(")
                (write-string out "\033[m") ;; endbold
                (slide-cursor out (- delta 1)) ; -1 because we wrote a char
                (set-buffer-blink-posn! buf k)))))))
;

(define (buffer-load-state (buf <readline-activation>) s)
  (with-buf-macros
   (let ((out (output-port buf)))
     ;; first, erase what we have
     (write-string out "\033\.8") ;; restore cursor
     (clear-to-eol)
     ;(write-string out "\033[1m") ;; bold
     (write-string out prompt)
     ;(write-string out "\033[m") ;; end bold
     (write-string out (car s))
     (set-buffer-left! buf (substring->dequeue (car s) 0 (cdr s)))
     (set-buffer-right! buf (substring->dequeue (car s) (cdr s) (string-length (car s))))
     (let ((goback (- (string-length (car s)) (cdr s))))
       (if (not (zero? goback))
           (write-string out (format #f "\033[~dD" goback))))))) ;; move left

(defun (enter)
  (set-buffer-tab-time! buf #f)
  (control "\n")
  (buffer-return buf (car (current-state))))

(defun (sponsor)
  (write-string out "\033[L\015")
  (write-string out "+--------------------------------+\n")
  (write-string out "| A message from the sponsor...  |\n")
  (format out "| current => ~s\n" (current-state))
  (format out "| undo stack => ~d items\n" (length undo))
  (let* ((pre (collect-prefix left (buffer-state buf))))
    (format out "| prefix: ~s\n" pre)
    (let ((c (collect-completions (buffer-state buf) pre)))
      (format out "| completions: ~s\n" c)))
  (write-string out "+--------------------------------+\n")
  (save-cursor)
  (load-state (current-state)))

(defun (beginning-of-line)
  (while (not (dequeue-empty? left))
         (move-left)))

(defun (end-of-line)
  (while (not (dequeue-empty? right))
         (move-right)))

(defun (left)
  (if (not (dequeue-empty? left))
      (move-left)))

(defun (right)
  (if (not (dequeue-empty? right))
      (move-right)))

(defun (tab-completion)
  (let* ((pre (collect-prefix left (buffer-state buf)))
         (c (collect-completions (buffer-state buf) pre)))
    ;(set! matches c)
    (cond
     ((null? c)
      ;; no completions (c will be null if pre is empty)
      (write-string out "\7"))
     ((null? (cdr c))
      ;; unique completion
      (can-undo)
      (insert-right (list-tail (string->list (car c))
                               (string-length pre)))
      (insert-right '(#\space)))
     ((and (buffer-tab-time buf)
           (< (interval->seconds (time-time (time) (buffer-tab-time buf))) 1))
      (set-buffer-tab-time! buf (time))
      (write-string out "\015\n")
      (format out "Completions (~d of them)\n" (length c))
      (column-out out (sort c string<?) 79)
      (write-string out "\033\.7") ; re-save cursor on this line
      (load-state (current-state)))
     (else
      (set-buffer-tab-time! buf (time))
      (let ((com (common-prefix c)))
        (if (<= (string-length com) (string-length pre))
            (write-string out "\7") ;; no more to complete
            (begin
              (can-undo)
              (insert-right 
               (string->list (substring com (string-length pre)))))))))))


(defun (kill-to-eol)
  (can-undo)
  (set-buffer-tab-time! buf #f)
  (set-buffer-right! buf (make-dequeue))
  (clear-to-eol))

(defun (delete-line)
  (can-undo)
  (set-buffer-tab-time! buf #f)
  (load-state '("" . 0)))

(defun (delete-left)
  (if (not (dequeue-empty? left))
      (begin
        (can-undo)
        (set-buffer-tab-time! buf #f)
        (dequeue-pop-back! left)
        (control "\10\033[P"))))

(defun (delete-right)
  (if (not (dequeue-empty? right))
      (begin
        (can-undo)
        (set-buffer-tab-time! buf #f)
        (dequeue-pop-front! right)
        (control "\033[P"))))

(defun (delete-right-or-eof)
  (if (and (dequeue-empty? right)
           (dequeue-empty? left))
      (buffer-return buf (with-input-from-string "" read-char))
      (exec delete-right)))

(defun (undo)
  (if (pair? undo)
      (begin
        (set-buffer-tab-time! buf #f)
        (load-state (car undo))
        (set-buffer-undo! buf (cdr undo)))))

(defun (history-prev)
  (if (pair? (buffer-prev buf))
      (begin
        (set-buffer-tab-time! buf #f)
        (set-buffer-succ! buf (cons (current-state) (buffer-succ buf)))
        (load-state (car (buffer-prev buf)))
        (set-buffer-prev! buf (cdr (buffer-prev buf))))))

(defun (history-next)
  (if (pair? (buffer-succ buf))
      (begin
        (set-buffer-tab-time! buf #f)
        (set-buffer-prev! buf (cons (current-state) (buffer-prev buf)))
        (load-state (car (buffer-succ buf)))
        (set-buffer-succ! buf (cdr (buffer-succ buf))))))


(defun (suspend-process)
  (set-buffer-tab-time! buf #f)
  (flush-output-port out)
  (with-module unixm
    (kill (getpid) (vmemq 'SIGTSTP (os-signal-name-vector)))))

(defun (interrupt-process)
  (set-buffer-tab-time! buf #f)
  (flush-output-port out)
  (with-module unixm
    (kill (getpid) (vmemq 'SIGINT (os-signal-name-vector)))))

(defun (destroy-process)
  (set-buffer-tab-time! buf #f)
  (flush-output-port out)
  (with-module unixm
    (kill (getpid) (vmemq 'SIGQUIT (os-signal-name-vector)))))


;;;

(define (xterm-readline* prompt inp out (state <readline-state>))
  (call-with-current-continuation
   (lambda (return)
     (let ((buf (make <readline-activation>
                      buffer-state: state
                      buffer-prompt: prompt
                      input-port: inp
                      buffer-prev: (map (lambda (h)
                                          (cons h (string-length h)))
                                        (history state))
                      output-port: out
                      buffer-return-proc: return)))
       ;;
       (with-buf-macros
        (save-cursor)
        (load-state '("" . 0)))
       ;;
       (let loop ()
         (flush-output-port out)
         (bind ((cmd keyseq (process-key-map (buffer-keymap buf)
                                             (input-port buf)
                                             (output-port buf)
                                             '())))
           (debug "(~s => ~s)\n" keyseq cmd)
           (buffer-unblink-paren buf)
           (if cmd
               (let ((p (table-lookup (buffer-fnmap buf) cmd)))
                 (if p
                     (p buf keyseq))))
           (buffer-blink-paren buf)
           (loop)))))))
