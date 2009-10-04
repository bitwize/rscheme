(define-class <message-table> (<table>)
  (name type: <symbol>)
  (id type: <fixnum>)
  (message-index type: <hash-integer-table>))

  
(define-class <message> (<object>) :abstract
  (properties type: <vector> init-value: '#() :sealed)
  (owner type: <message-table>)
  (id type: <fixnum>)
  (source init-value: #f)
  default-text)

(define-method print ((self <message-table>) #optional show-source?)
  (format #t "=============== Message Table ~03d (~a) ===============\n" 
          (id self) 
          (name self))
  (let ((ix (message-index self)))
    (for-each
     (lambda (k)
       (let (((m <message>) (table-lookup ix k)))
         (if (and show-source? (source m))
             (format #t "~a:~a: " 
                     (with-module
                         paths
                       (file-within-dir (string->file (car (source m)))))
                     (cadr (source m))))
         (format #t "~a ~#@70s\n"
                 (name m)
                 (default-text m))))
     (sort (key-sequence ix) <))))

(define-syntax (other-message-enabled? self)
  (if (eq? (vector-length (properties self)) 0)
      #t
      (if (vmemq 'disable (properties self))
          #f
          #t)))

(define-class <fatal-message> (<message>))
(define-class <error-message> (<message>))
(define-class <warning-message> (<message>))
(define-class <debug-message> (<message>))
(define-class <notice-message> (<message>))
(define-class <info-message> (<message>))

(define-method type ((self <fatal-message>)) 'fatal)
(define-method type ((self <error-message>)) 'error)
(define-method type ((self <warning-message>)) 'warning)
(define-method type ((self <info-message>)) 'info)
(define-method type ((self <notice-message>)) 'notice)
(define-method type ((self <debug-message>)) 'debug)

(define-method type-char ((self <message>)) #\?)

(define-method type-char ((self <fatal-message>)) #\F)
(define-method type-char ((self <error-message>)) #\E)
(define-method type-char ((self <warning-message>)) #\W)
(define-method type-char ((self <info-message>)) #\I)
(define-method type-char ((self <notice-message>)) #\N)
(define-method type-char ((self <debug-message>)) #\D)

(define-method table-lookup ((self <message-table>) (key <fixnum>))
  (table-lookup (message-index self) key))

(define-method key-sequence ((self <message-table>))
  (key-sequence (message-index self)))

(define-method value-sequence ((self <message-table>))
  (value-sequence (message-index self)))

(define-method table-insert! ((self <message-table>) 
			      (key <fixnum>) 
			      (value <message>))
  (table-insert! (message-index self) key value))


(define (pick-messages (tbl <message-table>) q-id q-type)
  (cond
   ;;
   ((and q-id (not q-type))
    (cond
     ((table-lookup tbl q-id) => list)
     (else '())))
   ;;
   ((and q-id q-type)
    (select (lambda (m)
              (eq? (type m) q-type))
            (pick-messages tbl q-id #f)))
   (q-type
    (select (lambda (m)
              (eq? (type m) q-type))
            (value-sequence (message-index tbl))))
   (else
    (value-sequence (message-index tbl)))))

;;

(define (set-message-disabled! (self <message>))
  (let* (((props <vector>) (properties self))
         (k (vassq 'disable props)))
    (if (not k)
        (set-properties! self (vector-append (vector 'disable #t) props)))))

(define (set-message-enabled! (self <message>))
  (let* (((props <vector>) (properties self))
         (k (vassq 'disable props)))
    (cond
     ((eq? k 1)
      (set-properties! self (subvector props (+ k 1))))
     (k
      (set-properties! self (vector-append (subvector props 0 (- k 1))
                                           (subvector props (+ k 1))))))))

(define (message-table-config (tbl <message-table>)
                              #key 
                              (id default: #f) 
                              (type default: #f)
                              (enable default: #f)
                              (disable default: #f))
  (let ((set (pick-messages tbl id type)))
    (if enable
        (for-each set-message-enabled! set)
    (if disable
        (for-each set-message-disabled! set))
    set)))


(define-macro (message-enable id)
  `(message-table-config *messages* id: ,id enable: #t))

(define-macro (message-disable id)
  `(message-table-config *messages* id: ,id disable: #t))
  
(define-method name ((self <message>))
  (format #f "~d-~03d~c"
	  (id (owner self)) 
	  (id self)
          (type-char self)))

(define (alloc-new-id (tbl <message-table>))
  (table-size (message-index tbl)))

(define-method finish-message-init ((self <message>))
  (values))     ; nothing by default

(define (add-message! tbl id text type source #optional (plist default: '()))
  (let* ((props (if (null? plist) '#() (list->vector plist)))
         ((m <message>) (case type
                          ((fatal) (make <fatal-message>
                                         properties: props
                                         owner: tbl
                                         id: id
                                         source: source
                                         default-text: text))
                          ((error) (make <error-message>
                                         properties: props
                                         owner: tbl
                                         id: id
                                         source: source
                                         default-text: text))
                          ((warning) (make <warning-message>
                                           properties: props
                                           owner: tbl
                                           id: id
                                           source: source
                                           default-text: text))
                          ((debug) (make <debug-message>
                                         properties: props
                                         owner: tbl
                                         id: id
                                         source: source
                                         default-text: text))
                          ((info) (make <info-message>
                                         properties: props
                                         owner: tbl
                                         id: id
                                         source: source
                                         default-text: text))
                          ((notice) (make <notice-message>
                                          properties: props
                                          owner: tbl
                                          id: id
                                          source: source
                                          default-text: text)))))
    (let ((o (table-lookup tbl id)))
      (if (and o (not (equal? (default-text o) (default-text m))))
          (with-module compiler
            (warning "Message ID ~a (~s) is being redefined"
                     (name o)
                     (default-text o)))))
    (table-insert! tbl id m)
    (finish-message-init m)
    m))

(define-class <message-out-port> (<output-port>)
  (message-prefix type: <string> init-value: "")
  (underlying-output-port type: <output-port>))

(define-method output-port-write-char ((self <message-out-port>) ch)
  (output-port-write-char (underlying-output-port self) ch)
  (if (eq? ch #\newline)
      (write-string (underlying-output-port self) (message-prefix self))))

(define (message->string (self <message>) (argv <vector>) plc)
  (let ((p (open-output-string)))
    (display-message p self argv plc)
    (close-output-port p)))

(define-thread-var *current-message-queue*)

(define (current-message-dest)
  (or *current-message-queue* (current-output-port)))

(define (generate-keyword-formatter (text <string>) (args <list>))
  (let ((kargv (list->vector args)))
    (let loop ((control (parse-format-string text))
               (r '()))
      (cond
       ((null? control)
        (reverse r))
       ;;
       ((string? (car control))
        (loop (cdr control)
              (cons `(write-string port ,(car control)) r)))
       ;;
       (else
        (bind ((x (car control))
               (fmt-ch (if (or (ascii-char? x)
                               (not (braced-modifier x))
                               (string=? (braced-modifier x) ""))
                           (error "No argument name specified for ~s" x)
                           (format-character x)))
               (num-args proc ((cdr (or (assq fmt-ch *global-formatters*)
                                        (error "Unknown format character ~s"
                                               fmt-ch)))
                               x))
               (i (or (vmemq (string->symbol (braced-modifier x)) kargv)
                      (error "No keyword for ~~{~a}~a supplied" 
                             (braced-modifier x)
                             (format-character x)))))
          ;;
          (case num-args
            ((0)
             (loop (cdr control) (cons `(',proc port) r)))
            ((1)
             (loop (cdr control) (cons `(',proc port (vector-ref argv ,i)) r)))
            (else
             (error "Can't handle ~d-argument formatter w/keywords" 
                    num-args)))))))))
  
(define *compiled-message-cache* (make-object-table))
(define *compiled-stripped-message-cache* (make-object-table))

(define (get-stripped-message-displayer (self <message>))
  (or (table-lookup *compiled-stripped-message-cache* self)
      (recompile-message-displayer self #t)))
      
(define (recompile-message-displayer (self <message>) #optional stripped?)
  (let* ((n (string-append (name self) " "))
         (text (default-text self))
         (kargs (let* (((p <vector>) (properties self)))
                  (if (vmemq 'keyword-args p)
                      (vector-ref p (vassq 'args p))
                      #f)))
         (proc (if (not kargs)
                   ;; positional arguments
                   (if stripped?
                       (lambda (port argv)
                         (apply format port text (vector->list argv)))
                       (lambda (port argv)
                         (write-string port n)
                         (apply format port text (vector->list argv))
                         (newline port)))
                   ;; keyword arguments
                   (if stripped?
                       (eval `(lambda (port argv)
                                ,@(generate-keyword-formatter 
                                   text
                                   kargs)))
                       (eval `(lambda (port argv)
                                ,@(generate-keyword-formatter 
                                   (string-append n text "\n") 
                                   kargs)))))))
    (table-insert! (if stripped?
                       *compiled-stripped-message-cache*
                       *compiled-message-cache*)
                   self proc)
    proc))

(define-method display-message ((port <string-output-port>)
                                (self <message>) (argv <vector>) plc)
  ((or (table-lookup *compiled-message-cache* self)
       (recompile-message-displayer self))
   port argv))

(define-method display-message ((port <output-port>)
                                (self <message>) (argv <vector>) plc)
  (let ((tmp (open-output-string)))
    (display-message tmp self argv plc)
    (write-string port (close-output-port tmp))
    (values)))

;; (alloc-message [([:keyword] ARG ...)] TYPE TABLEID ID TEXT)
;; (alloc-message [([:keyword] ARG ...)] TYPE ID TEXT)
;; (alloc-message [([:keyword] ARG ...)] TYPE TEXT)

(define-macro (alloc-message . stuff)
  ;;
  (define (source-location)
    (with-module compiler
      (compile-point-file-and-line $dynamic-envt)))
  ;;
  (define (get-module-message-table)
    (with-module compiler
      (let ((var (lookup-aliased '*messages* $envt $envt)))
        (if (not var)
            (error "message table not defined"))
        (value var))))
  ;;
  (define (get-message-table)
    (if (= (length stuff) 4)
        (get-message-table-by-id (cadr stuff))
        (get-module-message-table)))
  ;;
  (let ((msgcat #f)
        (type (car stuff))
        (text #f)
        (id #f)
        (save-args #f)
        (keyword-args #f))
    ;;
    (if (list? (car stuff))
        (begin
          (set! save-args type)
          ;;
          ;; strip off the :keyword arguments prefix flag
          (if (and (pair? save-args)
                   (eq? (car save-args) ':keyword))
              (begin
                (set! keyword-args #t)
                (set! save-args (cdr save-args))))
          ;;
          (set! stuff (cdr stuff))
          (set! type (car stuff))))
    ;;
    (set! msgcat (get-message-table))
    ;;
    (if (not (memq type '(fatal error warning debug notice info)))
        (error "invalid message type `~s'" type))
    ;;
    (cond
     ((and (= (length stuff) 2)
           (string? (cadr stuff)))
      (set! id (alloc-new-id msgcat))
      (set! text (cadr stuff)))
     ((and (= (length stuff) 3)
           (fixnum? (cadr stuff))
           (string? (caddr stuff)))
      (set! id (cadr stuff))
      (set! text (caddr stuff)))
     ((and (= (length stuff) 4)
           (fixnum? (cadr stuff))
           (fixnum? (caddr stuff))
           (string? (cadddr stuff)))
      (set! id (caddr stuff))
      (set! text (cadddr stuff)))
     (else
      (error
       "alloc-message: expected `type id text' or just `type text'\n> ~s" 
       stuff)))
    (let ((m (add-message! msgcat id text type (source-location) 
                           (append
                            (if save-args (list 'args save-args) '())
                            (if keyword-args (list 'keyword-args #t) '())))))
      (list 'quote m))))
		  
(define (make-message-table (name <symbol>) (id <fixnum>))
  (make <message-table>
	name: name
	id: id
	message-index: (make-table eq? integer->hash)))

(define *message-tables* (make-symbol-table))
(define *message-tables-by-id* (make-fixnum-table))

(define (get-message-table-by-id (id <fixnum>))
  (or (table-lookup *message-tables-by-id* id)
      (error "message table id `~d' not defined" id)))

(define (get-message-table name)
  (or (table-lookup *message-tables* name)
      (error "message table `~s' not already defined" name)))

(define (setup-message-table name id)
  (let ((t (make-message-table name id)))
    (table-insert! *message-tables* name t)
    (table-insert! *message-tables-by-id* id t)
    t))

;; provide standard RScheme tables
;; (for modules that don't want to use their own)

(setup-message-table 'rs 110)
(setup-message-table 'sys 120)

(define (make-anonymous-message-table)
  ;;(make-message-table 'anon 800)
  (or (table-lookup *message-tables-by-id* 800)
      (setup-message-table 'anon 800)))

(define-macro (define-message-table name . r)
  (let ((m (if (pair? r)
	       (setup-message-table name (car r))
	       (get-message-table name))))
    `(define *messages* ',m)))

(define (fmt-msg msg argv . more)
  (message->string msg
		   argv 
		   (if (null? more)
		       *message-prefix*
		       (car more))))

(define-macro (fm . args)
  (bind ((msg args xtra (foo 'notice args))
	 (mn (gensym)))
    `(let ((,mn (alloc-message ,args ,@msg)))
       (',fmt-msg ,mn (vector ,@args) ,@xtra))))

(define message-id id)
(define message-type type)
(define message-type-char type-char)
(define message-source source)
(define message-default-text default-text)

(define (message-table-id (self <message>))
  (id (owner self)))

;(&module (export fm))

(define-macro (define-external-msg major minor type text . args)
  (define (source-location)
    (with-module compiler
      (compile-point-file-and-line $dynamic-envt)))
  ;;
  (add-message! (get-message-table-by-id major)
                minor
                text
                type
                (source-location)
                (if (and (pair? args) (eq? (car args) '#key))
                    (list 'args (cdr args) 'keyword-args #t)
                    (list 'args args)))
  '(values))

(define (show-message (m <message>) (argv <vector>))
  (display-message (current-message-dest) m argv #f)
  (values))
