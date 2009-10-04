#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/repl/cmdloop.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.19
 | File mod date:    2003-11-04 15:58:12
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  repl
 |
 | Purpose:          Define the main command loop
 `------------------------------------------------------------------------|#

(define-thread-var *cmd-loop* #f)

(define-hook start-cmd-loop)
(define-hook cmd-loop-prompt)

(%early-once-only
 (add-cmd-loop-prompt-hook! warn-about-unbound-vars-created))

(define-class <cmd-loop> (<object>)
  (cmd-loop-exit init-keyword: #f init-value: #f)
  (cmd-loop-restart init-keyword: #f init-value: #f)
  (prompt init-value: "top[~d]=>")
  (cmd-loop-depth init-value: 0)
  (outer-loop init-value: #f)
  input-port
  output-port
  environment
  (properties init-value: '#()))

(define (sub-loop (cl <cmd-loop>) prompt . opts)
  (cmd-loop-run (make <cmd-loop>
                      properties: (keyword-value-list->vector opts)
		      prompt: prompt
		      input-port: (input-port cl)
		      output-port: (output-port cl)
		      environment: (environment cl)
		      outer-loop: cl
		      cmd-loop-depth: (+ (cmd-loop-depth cl) 1))))

(define (cmd-loop envt prompt . opts)
  (cmd-loop-run (make <cmd-loop>
                      properties: (keyword-value-list->vector opts)
		      prompt: prompt
		      environment: envt
		      input-port: *console-input-port*
		      output-port: *console-output-port*)))

(define (cmd-loop-limit-reached? (self <cmd-loop>) i)
  (let ((k (vassq 'limit: (properties self))))
    (and k (>= i (vector-ref (properties self) k)))))
        
(define (cmd-loop-run (self <cmd-loop>))
  (call-with-current-continuation
   (lambda (exit)
     (thread-let ((*input-port* (input-port self))
		  (*output-port* (output-port self))
		  (*cmd-loop* self))
       (run-hooks *start-cmd-loop-hook*)
       (handler-bind (<condition> repl-condition-handler)
         (set-cmd-loop-exit! self exit)
	 (let loop ((i 0))
           (if (cmd-loop-limit-reached? self i)
               (values)
               (begin
                 (cmd-loop-once self i exit)
                 (loop (+ i 1))))))))))

(define (cmd-loop-once (self <cmd-loop>) i exit)
  ;; e.g., warn about unbound vars...
  (run-hooks *cmd-loop-prompt-hook*)
  ;;
  (call-with-current-continuation
   (lambda (restart)
     (set-cmd-loop-restart! self restart)
     (let ((ip (input-port self)))
       (skip-whitespace ip)
       (if (instance? ip <edit-input-port>)
           (begin
             (set-use-secondary?! ip #f)
             (set-secondary-prompt! ip " ")
             (if (dirty? (environment self))
                 (begin
                   (set-completions! ip
                                     (table-keys->list 
                                      (table (environment self))))
                   (set-dirty?! (environment self) #f)))
             (if (prompt self)
                 (set-primary-prompt! ip
                                      (format #f (prompt self) i)))))
       (let ((item (read-and-eat-whitespace ip)))
         ;; flush any remaining whitespace
         (if (instance? ip <edit-input-port>)
             (begin
               (set-primary-prompt! ip "")
               (set-secondary-prompt! ip "")))
         (if (eof-object? item)
             (begin
               (newline)
               (exit))
             (fluid-let ((*source-point* (vector 'repl i item)))
               (if (not (run-special-command item (environment self)))
                   (eval-print item (environment self))))))))))

;;; read a datum from the input, and eat any whitespace that follows it
;;; (this way, you can type:
;;;    ==>(read-char)
;;;  and not get #\newline every time

(define (read-and-eat-whitespace port)
  (let ((datum (read port)))
    (skip-whitespace port)
    datum))

(define (eval-string string envt)
  (with-objects-from-port
   (open-input-string string)
   (lambda (o)
     (eval-in-envt o envt))))

(define (eval-in-envt s-expr envt)
  (if (and (pair? s-expr)
	   (eq? (car s-expr) 'unquote))
      ;; process an environment-meta command
      (if (run-special-command s-expr envt)
	  (values)
	  (error "eval: ~s is invalid" s-expr))
      (let ((thunk (expr->thunk s-expr envt)))
	(if *trace-bci*
	    (set-bci-trace-flag! #t))
	(if *trace-apply*
	    (set-apply-trace-flag! #t))
	(bind ((#rest all (backstop (list 'eval s-expr envt) thunk)))
	  (if *trace-bci*
	      (set-bci-trace-flag! #f))
	  (if *trace-apply*
	      (set-apply-trace-flag! #f))
	  (list->values all)))))

(define (expr->thunk s-expr envt)
  (let ((ic (compile s-expr envt envt 'top)))
    (cond
     ((instance? ic <icode>)
      (wrap-tl-expr ic))
     (ic
      (lambda () ic))
     (else
      (lambda () (values))))))

(define (eval-print s-expr envt)
  (let ((o (current-output-port)))
    (bind ((#rest vals (call-with-markup o
                                         '(repl-eval)
                                         (lambda ()
                                           (eval-in-envt s-expr envt)))))
      ;; bind the results to % and %%
      (if (pair? vals)
          (shift-in envt vals))
      ;; display the results
      (call-with-markup o '(repl-results)
                        (lambda ()
                          (display-values vals)))
      vals)))

(define (lookup-or-new envt name)
  (let ((b (lookup envt name)))
    (if b
	b
	(let ((b (make <top-level-var>
		       name: name
		       value: '#unbound
		       write-prot: #f)))
	  (bind! envt b)
	  b))))

(define (shift-in envt vals)
  (let ((%-var (lookup-or-new envt '%))
	(%%-var (lookup-or-new envt '%%)))
    (set-value! %%-var (value %-var))
    (set-value! %-var (car vals))))

(define *display-limit* 50)

(define (current-display-limit)
  *display-limit*)

(define (set-display-limit! w)
  (set! *display-limit* w))

(define (display-values v)
  (let ((line-len (current-display-limit)))
    (if (and (pair? v) (null? (cdr v)))
	(let ((p (current-output-port)))
	  (write-string p "value := ")
          (call-with-markup p 
                            '(repl-value)
                            (lambda ()
                              (display (object->bounded-string 
                                        (if line-len 
                                            (+ line-len 3) 
                                            #f) 
                                        (car v))
                                       p)))
	  (newline p))
	(let loop ((i 0) (v v))
	  (if (not (null? v))
              (let ((p (current-output-port)))
		(format p "value[~d] := " i)
                (call-with-markup p 
                                  (list 'repl-value i)
                                  (lambda ()
                                    (display 
                                     (object->bounded-string line-len 
                                                             (car v))
                                     p)))
		(newline p)
		(loop (add1 i) (cdr v))))))))

(define (show-aml asm)
  (format #t "===== wrapping aml =====\n")
  (for-each (lambda (s)
	      (format #t "\t~s\n" s))
	    asm))

(define *show-aml* #f)

(define (wrap-tl-expr icode)
  (let* ((cc (make-code-ctx '((function-scope eval))))
	 (asm (expr->aml icode cc)))
    (if *show-aml*
	(show-aml asm))
    (make <closure>
	  environment: '()
	  template: (aml->template asm cc))))

(define (wrap-tl-proc prop-list icode)
  (let* ((cc (make-code-ctx prop-list))
	 (asm (procedure->aml icode '() cc)))
    (if *show-aml*
	(show-aml asm))
    (make <closure>
	  environment: '()
	  template: (aml->template asm cc))))
