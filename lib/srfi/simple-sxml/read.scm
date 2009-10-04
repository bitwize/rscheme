(define (sxml:debug-print msg . args)
  (with-module iolib
    (format #t "---SXML-DEBUG--- ")
    (apply format #t msg args)
    (newline))
  (begin))


(define (sxml-scan-while port . tests)
  (let loop ((l '())
             (t tests))
    (let ((ch (peek-char port)))
      (if ((car t) ch)
          (loop (cons (read-char port) l)
                (if (null? (cdr t))
                    t
                    (cdr t)))
          (list->string (reverse l))))))
  
(define (sxml-char-class-letter? ch)
  (char-alphabetic? ch))

(define (sxml-char-class-whitespace? ch)
  (char-whitespace? ch))

(define (sxml-char-class-namestart? ch)
  (or (sxml-char-class-letter? ch)
      (char=? ch #\_)
      (char=? ch #\:)))

(define (sxml-char-class-digit? ch)
  (memv ch '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)))

(define (sxml-char-class-combining? ch)
  ;; all these are unicode characters
  #f)

(define (sxml-char-class-extender? ch)
  ;; all these are unicode characters
  #f)

(define (sxml-char-class-namechar? ch)
  (or (sxml-char-class-letter? ch)
      (sxml-char-class-digit? ch)
      (memv ch '(#\. #\- #\_ #\:))
      (sxml-char-class-combining? ch)
      (sxml-char-class-extender? ch)))

(define (sxml-scan-name port)
  (sxml-scan-while port
                   sxml-char-class-namestart?
                   sxml-char-class-namechar?))

(define (sxml-skip-space port)
  (sxml-scan-while port
                   sxml-char-class-whitespace?))

(define (sxml-scan-pi port)
  (read-char port)      ; skip the '?'
  (let* ((pi-target (sxml-scan-name port))
         (skip (sxml-skip-space port)))
    (letrec ((body (lambda (l)
                     (let ((ch (read-char port)))
                       (if (char=? ch #\?)
                           (sawq l)
                           (body (cons ch l))))))
             (sawq (lambda (l)
                     (let ((ch (read-char port)))
                       (if (char=? ch #\>)
                           (list->string (reverse l))
                           (if (char=? ch #\?)
                               (sawq (cons #\? l))
                               (body (cons ch (cons #\? l)))))))))
      ;;
      (let ((b (body '())))
        (if (string-ci=? pi-target "xml")
            (list '*XML* b)
            (if (string=? b "")
                (list '*PI* (string->symbol pi-target))
                (list '*PI* (string->symbol pi-target) b)))))))

(define (sxml-scan-attr-value port)
  (let ((delim (read-char port)))
    (if (not (or (char=? delim #\')
                 (char=? delim #\")))
        (error "Expected attribute delimiter to be #\\' or #\\\":" delim))
    ;;
    (let loop ((lst '()))
      (let ((ch (read-char port)))
        (if (char=? ch delim)
            (list->string (reverse lst))
            (if (char=? ch #\&)
                (let ((next (sxml-scan-entityref port)))
                  (case (car next)
                    ((text)
                     (loop (append (reverse (string->list (cadr next))) lst)))
                    (else
                     (error "Only standard entities in attr values are allowed"))))
                (loop (cons ch lst))))))))
                                          
              

(define (sxml-scan-attributes port)
  (let loop ((attrs '()))
    (sxml-skip-space port)
    (if (memv (peek-char port) '(#\> #\/))
        ;; why are we doing this after instead of during...?
        ;; so that it's easier to handle xmlns= if we want to...
        (reverse (map (lambda (a)
                        (list (internalize-name (car a)) (cdr a)))
                      attrs))
        (let ((key (sxml-scan-name port)))
          (sxml-skip-space port)        ; optional space
          (required-read-char port #\=)
          (sxml-skip-space port)        ; more optional space
          (let ((value (sxml-scan-attr-value port)))
            (loop (cons (cons key value) attrs)))))))
        

(define (required-read-char port ch)
  (let ((got (read-char port)))
    (if (not (char=? got ch))
        (error "Expected thing #1, got thing #2" ch got))))

(define (internalize-name local)
  (string->symbol local))

(define (preform-element tag attrs)
  (if (null? attrs)
      (list (internalize-name tag))
      (list (internalize-name tag) (cons '@ attrs))))

(define (sxml-scan-etag port)
  ;; already ate the '<'
  (read-char port)      ; eat the '/'
  (let ((tag (sxml-scan-name port)))
    (sxml-skip-space port)
    (required-read-char port #\>)
    (list 'etag (internalize-name tag))))

(define (check-tag-match estart new)
  (if (not (eq? (cadr estart) (cadr new)))
      (error "start-tag/end-tag mismatch")))
      

(define (sxml-scan-stag port)
  ;; we already ate the '<'
  (let ((tag (sxml-scan-name port)))
    (sxml-skip-space port)
    (let ((attrs (sxml-scan-attributes port)))
      (let ((ch (read-char port)))
        (if (char=? ch #\/)
            ;; empty element tag
            (begin
              (required-read-char port #\>)
              (cons 'setag (preform-element tag attrs)))
            (if (char=? ch #\>)
                (cons 'stag (preform-element tag attrs))
                (error "Expected '>' or '/>'")))))))

(define (sxml-scan-entityref port)
  (read-char port)      ; skip the '&'
  (let ((n (sxml-scan-name port)))
    (required-read-char port #\;)
    (cond
     ((string-ci=? n "lt") '(text "<"))
     ((string-ci=? n "gt") '(text ">"))
     ((string-ci=? n "apos") '(text "'"))
     ((string-ci=? n "quot") '(text "\""))
     ((string-ci=? n "amp") '(text "&"))
     (else (list '*ENTITY* n)))))

(define (sxml-scan-token port)
  (case (peek-char port)
    ((#\<)
     (read-char port)
     (case (peek-char port)
       ((#\/) (sxml-scan-etag port))
       ((#\!) (sxml-scan-special port))
       ((#\?) (sxml-scan-pi port))
       (else (sxml-scan-stag port))))
    ((#\&) (sxml-scan-entityref port))
    (else
     (if (eof-object? (peek-char port))
         '(eof)
         (let loop ((accum (list (read-char port))))
           (let ((ch (peek-char port)))
             (if (or (eof-object? ch)
                     (memq ch '(#\& #\<)))
                 (list (if (all-whitspace? accum)
                           'whitespace
                           'text)
                       (list->string (reverse accum)))
                 (loop (cons (read-char port) accum)))))))))

(define (all-whitspace? lst)
  (if (null? lst)
      #t
      (if (char-whitespace? (car lst))
          (all-whitspace? (cdr lst))
          #f)))

(define (call-with-sxml-scanner port proc)
  (let ((buffer #f))
    (proc
     ;; read
     (lambda ()
       (if buffer
           (let ((b buffer))
             (set! buffer #f)
             b)
           (sxml-scan-token port)))
     ;; peek
     (lambda ()
       (if (not buffer)
           (set! buffer (sxml-scan-token port)))
       buffer))))


(define (sxml-parse read peek)
  (letrec ((prolog (lambda ()
                     (sxml:debug-print "prolog: ~s" (peek))
                     (case (car (peek))
                       ((*XML* *PI*)
                        (prolog1 (list (read))))
                       ;;
                       ((whitespace)
                        (read)
                        (prolog1 '()))
                       ;;
                       ((*DOCTYPE*)
                        (prolog2 '()))
                       ;;
                       (else
                        (prolog1 '())))))
             ;;
             (prolog1 (lambda (preamble)
                       (sxml:debug-print "prolog1: ~s" (peek))
                       (case (car (peek))
                         ((*PI*)
                          (prolog1 (cons (read) preamble)))
                         ((whitespace)
                          (read)
                          (prolog1 preamble))
                         ((*COMMENT*)
                          (prolog1 (cons (read) preamble)))
                         (else
                          (prolog2 preamble)))))
             ;;
             (prolog2 (lambda (preamble)
                        (sxml:debug-print "prolog2: ~s" (peek))
                        (case (car (peek))
                          ((*DOCTYPE*)
                           (prolog3 (cons (read) preamble)))
                          (else
                           (rootelem preamble)))))
             ;;
             (prolog3 (lambda (preamble)
                        (sxml:debug-print "prolog3: ~s" (peek))
                        (case (car (peek))
                          ((*PI* *COMMENT*)
                           (prolog3 (cons (read) preamble)))
                          ((whitespace)
                           (read)
                           (prolog3 preamble))
                          (else
                           (rootelem preamble)))))
             ;;
             (rootelem (lambda (preamble)
                         (sxml:debug-print "rootelem: ~s" (peek))
                         (let ((topify (if (null? preamble)
                                           (lambda (root)
                                             `(*TOP* ,root))
                                           (lambda (root)
                                             `(*TOP* (@ ,@(reverse preamble))
                                                     ,root)))))
                           ;;
                           (case (car (peek))
                             ((stag)
                              (let ((root (element)))
                                (epilog)
                                (topify root)))
                             ((setag)
                              (let ((root (read)))
                                (epilog)
                                (topify (cdr root))))
                             (else
                              (error "Expected STAG or SETAG at document root"))))))
             ;;
             
             (element (lambda ()
                        (let ((start (read))) 
                          (let loop ((children '()))
                            (sxml:debug-print "element child: ~s" (peek))
                            (case (car (peek))
                              ((etag)
                               (check-tag-match start (read))
                               (append (cdr start) (reverse children)))
                              ((stag)
                               (loop (cons (element) children)))
                              ((setag)
                               (loop (cons (cdr (read)) children)))
                              ((text whitespace)
                               (loop (cons (cadr (read)) children)))
                              ((*ENTITY* *PI* *COMMENT*)
                               (loop (cons (read) children)))
                              (else
                               (error "wierd thing as child")))))))
             ;;
             (epilog (lambda ()
                       (sxml:debug-print "epilog: ~s" (peek))
                       (case (car (peek))
                         ((*PI* *COMMENT* whitespace)
                          (read)
                          (epilog)))))
             )
    (prolog)))

;;;

(define (read-sxml* port)
  (call-with-sxml-scanner port sxml-parse))
   
(define (read-sxml . args)
  (case (length args)
    ((0) (read-sxml* (current-input-port)))
    ((1) (read-sxml* (car args)))
    (else (error "read-sxml: wrong # args"))))
  
(define (string->sxml str)
  (read-sxml* (open-input-string str)))
