(define-class <debug-cmd-loop> (<object>)
  frame-list
  (cmd-loop-exit init-value: #f)
  environment)

(define (get-frame (self <debug-cmd-loop>) k)
  (let loop ((l (frame-list self)))
    (if (null? l)
        #f
        (if (eq? k (index (car l)))
            (car l)
            (loop (cdr l))))))

(define (debug)
  (let ((c (vassq 'condition: (properties *cmd-loop*))))
    (if c
        (post-mortem (vector-ref (properties *cmd-loop*) c))
        (debug* (make <debug-cmd-loop>
                      environment: (top-level-envt *default-user-initial*)
                      frame-list: (frames))))))

(define-method post-mortem ((self <partial-continuation>) #optional ds)
  (debug* (make <debug-cmd-loop>
                environment: (top-level-envt *default-user-initial*)
                frame-list: (vm-state->frame-list 
                             self
                             (or ds (get-dynamic-state-reg))
                             'eval))))

(define-method post-mortem ((self <condition>))
  (let* ((stackc (assq 'stack (properties self)))
         (stack (if stackc (cdr stackc) #f)))
    (if stack
        (debug*
         (make <debug-cmd-loop>
               environment: (top-level-envt *default-user-initial*)
               frame-list: (condition-frames* stack 'eval)))
        (error "No stack available"))))

;(define pm post-mortem)
  
(define (debug* ds)
  (call-with-current-continuation
   (lambda (exit)
     (set-cmd-loop-exit! ds exit)
     (with-edit-port
      (current-input-port)
      (current-output-port)
      (current-error-port)
      (lambda ()
        (do-debug ds))))))

(define $fin-w-call-pc
  (bind ((module-ptr (find-linked-module "RScheme"))
         (part-ptr (find-part-in-linked-module module-ptr 9501))
         (entry-ptr fundesc-ptr (find-code-ptr-in-part part-ptr 0))
         (name monotones (get-c-function-descr fundesc-ptr)))
    (cadr monotones)))

(define (do-debug (self <debug-cmd-loop>))
  (let ((ip (current-input-port))
        (op (current-output-port))
        (nf (length (frame-list self)))
        (k (- (length (frame-list self)) 1))
        (shown #f))
    (format #t "RScheme Online Debugger --- enter 'help' for help\n")
    (let loop ()
      (if (not (eq? shown k))
          (let ((f (get-frame self k)))
            (if f
                (print-1-frame (get-frame self k) op)
                (format #t "(No more stack frames)\n"))
            (format #t "===================================\n")
            (set! shown k)))
      (set-primary-prompt! ip (format #f "(debug ~d) " k))
      (set-use-secondary?! ip #f)
      (let ((cmd (read-and-eat-whitespace ip))
            (curf (get-frame self k)))
        ;;
        (case cmd
          ((exit q quit)
           ((cmd-loop-exit self)))
          ((h ? help)
           (format #t "Debugger Help:\n")
           (format #t "--------------\n")
           (format #t "  quit           ; quit debugger\n")
           (format #t "  up             ; go up the stack\n")
           (format #t "  down           ; go down the stack\n")
           (format #t "  list           ; list code\n")
           (format #t "  print          ; print expression value\n")
           (format #t "  bt             ; backtrace\n")
           (format #t "  <n>            ; go directly to frame #<n>\n"))
          ((d down)
           (set! k (min (- nf 1) (+ k 1))))
          ((u up)
           (set! k (max 0 (- k 1))))
          ((l list)
           (if curf
               (show-listing curf)
               (format #t "(No more stack frames)\n")))
          ((p print)
           (let ((expr (read-and-eat-whitespace ip)))
             (print
              (eval-in-envt (if curf
                                (replace-reg-ref expr curf)
                                expr)
                            (environment self)))))
          ((bt)
           (for-each (lambda (f)
                       (print-1-frame f op))
                     (frame-list self)))
          (else
           (cond
            ((eof-object? cmd)
             (newline)
             ((cmd-loop-exit self)))
            ((fixnum? cmd)
             (set! k (min (- nf 1) (max 0 cmd))))
            )))
        ;;
        (loop)))))

(define (replace-reg-ref expr ctx)
  (cond
   ((symbol? expr)
    (if (string-ci=? (substring (symbol->string expr) 0 3) "reg")
        (list 'quote (get-reg-value ctx 
                                    (string->number 
                                     (substring (symbol->string expr) 3))))
        expr))
   ((pair? expr)
    (cons (replace-reg-ref (car expr) ctx)
          (replace-reg-ref (cdr expr) ctx)))
   ((slice? expr)
    (list 'quote (get-lexvar-value ctx
                                   (slice-start expr)
                                   (slice-end expr))))
   (else
    expr)))


(define (disassembled-listing pc)
  (if (partial-continuation-bci? pc)
      (disassemble (gvec-ref (pc-template-reg pc) 3)
                   (pc-template-reg pc)
                   (pc-bci-program-counter pc))
      (bind ((func-desc (linkage-info (pc-template-reg pc)))
             (func-name monotones part-desc (get-c-function-descr func-desc))
             (part-name tag fns module-desc (get-c-part-descr part-desc))
             (module-name (get-c-module-descr module-desc)))
        (if (memq (pc-jump-addr pc) monotones)
            (begin
              (format #t "(don't know how to list native code for monotone[~d]\n"
                      (- (length monotones)
                         (length (memq (pc-jump-addr pc) monotones))))
              (format #t " of function ~s in\n part ~s of module ~s)\n"
                      func-name
                      part-name
                      module-name))
            (begin
              (format #t "(don't know how to list native code for\n")
              (format #t " unidentifiable monotone supposedly part of\n")
              (format #t " function ~s in part ~s\n of module ~s)\n"
                      func-name
                      part-name
                      module-name))))))

(define-method get-reg-value ((self <continuation-frame>) regnum)
  (list-ref (pc-regs (delegate self)) regnum))
  
(define-method get-lexvar-value ((self <continuation-frame>) frame slot)
  (pc-envt-ref (delegate self) frame slot))

(define-method show-listing ((self <continuation-frame>))
  (disassembled-listing (delegate self)))

(define-method show-listing ((self <apply-frame>))
  (format #t "(don't know how to list an application)\n"))

;;;

