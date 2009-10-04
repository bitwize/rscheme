
(define-class <option-set> (<object>)
  (will-list init-value: '())
  (wont-list init-value: '()))

(define (make-option-set)
  (make <option-set>))

(define-class <telnet-session> (<object>)
  (status init-value: '())
  nvt
  filter-input-port
  filter-output-port
  (local-options init-function: make-option-set)
  (remote-options init-function: make-option-set))

;;

(define-class <telnet-input-filter> (<input-port>)
  (in-command? init-value: #f)
  (buffered init-value: #f)
  (underlying-port type: <input-port>)
  (owner type: <telnet-session>))

;;

(define (open-telnet-session socket)
  (let ((tn (make <telnet-session>
                  nvt: #f
                  filter-input-port: #f
                  filter-output-port: (output-port socket))))
    (set-filter-input-port! tn (make <telnet-input-filter>
                                     owner: tn
                                     underlying-port: (input-port socket)))
    (set-nvt! tn (open-nvt (filter-input-port tn) 
                           (filter-output-port tn)))
    tn))

(define-method input-port ((self <telnet-session>))
  (input-port (nvt self)))

(define-method output-port ((self <telnet-session>))
  (output-port (nvt self)))

;;;

(define (optionset-lookup (self <option-set>) option)
  (cond
   ((memq option (will-list self))
    #t)
   ((memq option (wont-list self))
    #f)
   (else
    'unknown)))

(define (optionset-set! (self <option-set>) option sense)
  (if sense
      (begin
        (set-will-list! self (cons option (delq option (will-list self))))
        (set-wont-list! self (delq option (wont-list self))))
      (begin
        (set-wont-list! self (cons option (delq option (wont-list self))))
        (set-will-list! self (delq option (will-list self))))))

;;;
;;;  Decide what we think about an incoming DO or DONT request
;;;  (this is only called if this represents a change to our
;;;  option set)
;;;

(define-method consider-option-request ((self <telnet-session>) option sense)
  (case option
    ((echo) #t) ; request granted
    (else #f))) ; request denied

;;;
;;;  Decide what we think about an incoming WILL or WONT request
;;;  (this is only called if this represents a change from our
;;;  current understanding of the remote side's option set)
;;;

(define-method consider-option-declaration ((self <telnet-session>) 
                                            option 
                                            sense)
  (case option
    ((echo terminal-type new-environ) #t) ; declaration OK by us
    (else #f))) ; declaration not OK by us

(define (telnet-await-subnegotiation (self <telnet-session>) option)
  (let loop ((buf '()))
    (bind ((ch opt sn (telnet-getch* (filter-input-port self))))
      (if (eq? ch #f)
          (if (eq? opt option)
              (begin
                (if (not (null? buf))
                    (wm 556 "discarding ~s while waiting for ~s" buf option))
                sn)
              (begin
                (if opt
                    (wm 557 "discarding ~s=~s waiting for ~s" opt sn option))
                (loop buf)))
          (if (eof-object? ch)
              (em 701 "EOF awaiting subnegotiation for ~s" option)
              (loop (cons ch buf)))))))
  
(define (telnet-await-remote-option-ack (self <telnet-session>) option)
  (let loop ((buf '()))
    (let ((s (optionset-lookup (remote-options self) option)))
      (if (eq? s 'unknown)
          (let ((n (telnet-getch* (filter-input-port self))))
            (if n
                (if (eof-object? n)
                    (em 702 "EOF awaiting option ack for ~s" option)
                    (loop (cons n buf)))
                (loop buf)))
          (begin
            (if (not (null? buf))
                (wm 555 "discarding ~s" buf))
            s)))))

(define-method process-telnet-command ((self <telnet-session>) cmd arg)
  (case cmd
    ((ayt)
     (format (output-port self) 
             "#[telnetd: server ~s not ~s, client ~s not ~s]\n"
             (will-list (local-options self))
             (wont-list (local-options self))
             (will-list (remote-options self))
             (wont-list (remote-options self))))
    ((do dont)
     (let ((arg (option->symbol arg))
           (sense (eq? cmd 'do)))
       (if (not (eq? (optionset-lookup (local-options self) arg) sense))
         (if (consider-option-request self arg sense)
             (begin
               (optionset-set! (local-options self) arg sense)
               (telnet-declare-option self arg sense))
             (telnet-declare-option self arg (not sense))))))
    ((will wont)
     (let ((arg (option->symbol arg))
           (sense (eq? cmd 'will)))
       (if (not (eq? (optionset-lookup (remote-options self) arg) sense))
           (if (consider-option-declaration self arg sense)
               (begin
                 (optionset-set! (remote-options self) arg sense)
                 (telnet-request-option self arg sense))
               (begin
                 (telnet-request-option self arg (not sense)))))))
    (else
     (format (output-port self) "#[telnetd: what? ~s ~s]\n" cmd arg))))

;;;
;;;  send a WILL or a WONT command
;;;

(define (telnet-declare-option (self <telnet-session>) 
                               option #optional (will? default: #t))
  (write-string
   (filter-output-port self)
   (string #\d255 
           (if will? #\d251 #\d252) 
           (if (integer? option)
               (integer->char option)
               (integer->char 
                (cdr (or (assq option $telnet-options)
                         (em 304 "unknown telnet option ~s" option)))))))
  (flush-output-port (filter-output-port self)))
  
;;;
;;;  send a DO or a DONT command
;;;

(define (telnet-request-option (self <telnet-session>) 
                               option #optional (do? default: #t))
  (write-string
   (filter-output-port self)
   (string #\d255 
           (if do? #\d253 #\d254) 
           (if (integer? option)
               (integer->char option)
               (integer->char 
                (cdr (or (assq option $telnet-options)
                         (em 304 "unknown telnet option ~s" option)))))))
  (flush-output-port (filter-output-port self)))

;;

(define (telnet-subnegotiate-option (self <telnet-session>)
                                    option
                                    (content <string>))
  (let ((option (if (integer? option)
                    option
                    (cdr (or (assq option $telnet-options)
                             (em 305 "unknown telnet option: ~s" option))))))
    (write-string
     (filter-output-port self)
     (string-append
      (string #\d255 #\d250 (integer->char option))
      content
      (string #\d255 #\d240)))
    (flush-output-port (filter-output-port self))))

(define (option->symbol option)
  (case option
    ((1) 'echo)
    ((24) 'terminal-type)
    ((34) 'linemode)
    ((36) 'old-environ)
    ((39) 'new-environ)
    (else option)))

(define $telnet-options
  '((echo . 1)
    (terminal-type . 24)
    (linemode . 34)
    (old-environ . 36)
    (new-environ . 39)))
    
#|
  Some telnet option codes:


        1       echo                         (RFC 857)
        3       suppress go ahead            (RFC 858)
        5       status                       (RFC 859)
        6       timing mark                  (RFC 860)
        24      terminal type                (RFC 1091)
        31      window size                  (RFC 1073)
        32      terminal speed               (RFC 1079)
        33      remote flow control          (RFC 1372)
        34      linemode                     (RFC 1184)
        36      environment variables [old]  (RFC 1408)
        39      new environ                  (RFC 1572)
|#

(define-method input-port-char-ready? ((self <telnet-input-filter>))
  (if (buffered self)
      #t
      (char-ready? (underlying-port self))))
      
(define (telnet-getch (self <telnet-input-filter>))
  (or (telnet-getch* self)
      (telnet-getch self)))

(define (telnet-getch* (self <telnet-input-filter>))
  (let ((ch (input-port-read-char (underlying-port self))))
    (if (eq? ch #\xFF)
        (let ((cmd (input-port-read-char (underlying-port self))))
          (case cmd
            ((#\d255)
             cmd)               ; stuffed #xFF
            ((#\d251 #\d252 #\d253 #\d254)
             (let ((ocode (input-port-read-char (underlying-port self))))
               (if (char? ocode)
                   (begin
                     (process-telnet-command (owner self)
                                             (vector-ref 
                                              '#(will wont do dont)
                                              (- (char->integer cmd) 251))
                                             (char->integer ocode))
                     #f)
                   ocode)))
            ((#\d241)           ; noop
             #f)
            ((#\d240)           ; end of subnegotiation
             (em 601 "Bare SE not implemented"))
            ((#\d242)
             (em 602 "telnet: Data Mark not implemented"))
            ((#\d243)
             (process-telnet-command (owner self) 'break '())
             #f)
            ((#\d244)
             (process-telnet-command (owner self) 'interrupt '())
             #f)
            ((#\d245)
             (process-telnet-command (owner self) 'abort-output '())
             #f)
            ((#\d246)
             (process-telnet-command (owner self) 'ayt '())
             #f)
            ((#\d247)
             (process-telnet-command (owner self) 'erase-character '())
             #f)
            ((#\d248)
             (process-telnet-command (owner self) 'erase-line '())
             #f)
            ((#\d249)
             (process-telnet-command (owner self) 'go-ahead '())
             #f)
            ((#\d250)           ; subnegotiation
             (bind ((opt subn (process-subnegotiation self)))
               (dm 109 "subnegotiation for option ~s: ~#@*30s" opt subn)
               (values #f opt subn)))
            (else
             (em 411 "telnet: unknown TELNET command ~03d"
                 (char->integer cmd)))))
        (if (eq? ch #\C-d)
            $eof-object
            ch))))

(define (process-subnegotiation (self <telnet-input-filter>))
  (let* ((in (underlying-port self))
         (option (input-port-read-char in)))
    ;(dm 100 "subneg ~s" option)
    (if (eof-object? option)
        (em 313 "EOF during subnegotiation preamble"))
    (let loop ((content '()))
      (let ((ch (input-port-read-char in)))
        ;(dm 101 "subneg ~03d ~s" (char->integer ch) ch)
        (if (eq? ch #\d255)
            (let ((next (input-port-read-char in)))
              ;(dm 102 "next ~s -- accum ~s" next content)
              (if (eq? next #\d240)
                  (values (option->symbol (char->integer option))
                          (list->string (reverse! content)))
                  (if (eof-object? next)
                      (em 311 "EOF during subnegotiation IAC")
                      (em 310 "IAC w/o SE during subnegotiation (saw IAC ~03d)"
                          (char->integer next)))))
            (if (eof-object? ch)
                (em 312 "EOF during subnegotiation content")
                (loop (cons ch content))))))))
                  
         

(define-method input-port-read-char ((self <telnet-input-filter>))
  (if (buffered self)
      (let ((ch (buffered self)))
        (set-buffered! self #f)
        ;(dm 901 "read from buffer: ~s" ch)
        ch)
      (let ((ch (telnet-getch self)))
        ;(dm 902 "read new: ~s" ch)
        ch)))


(define-method input-port-peek-char ((self <telnet-input-filter>))
  (if (not (buffered self))
      (set-buffered! self (telnet-getch self)))
  ;(dm 903 "peek in buffer: ~s" (buffered self))
  (buffered self))

;;;

(define (telnet-get-terminal-type (self <telnet-session>))
  (telnet-request-option self 'terminal-type #t)
  (if (telnet-await-remote-option-ack self 'terminal-type)
      (begin
        (telnet-subnegotiate-option self 'terminal-type "\001")
        (let ((tt (telnet-await-subnegotiation self 'terminal-type)))
          (if (char=? (string-ref tt 0) #\x00)
              (let ((type (substring tt 1)))
                (dm 610 "Terminal type: ~s" type)
                type)
              #f)))
      #f))

;;;

(define (telnet-get-environment (self <telnet-session>))
  (telnet-request-option self 'new-environ #t)
  (if (telnet-await-remote-option-ack self 'new-environ)
      (begin
        (telnet-subnegotiate-option self 'new-environ "\001\003")
        (let ((x (telnet-await-subnegotiation self 'new-environ)))
          (if (char=? (string-ref x 0) #\x00)
              (parse-environ-is (substring x 1))
              (em 410 "NEW-ENVIRON response not an IS (got ~03d)"
                  (string-ref x 0)))))
      '()))

(define (parse-environ-is (str <string>))
  ;;
  (define (unescape from to)
    (let ((s (substring str from to)))
      (if (string-search s #\x02)
          (string-join "" (string-split s #\x02))
          s)))
  ;;
  (letrec ((result '())
           (add-result (lambda (type name value)
                         (set! result (cons (list (list type name) value) 
                                            result))))
           (start (lambda (i)
                    (if (< i (string-length str))
                        (case (string-ref str i)
                          ((#\x00)
                           (getname (+ i 1) 'var (+ i 1)))
                          ((#\x03)
                           (getname (+ i 1) 'uservar (+ i 1)))
                          (else
                           (em 302 "NEW-ENVIRON response parse error")))
                        (reverse result))))
           (getname (lambda (i type name-start)
                      (if (< i (string-length str))
                          (case (string-ref str i)
                            ((#\x00 #\x03)
                             (add-result type (unescape name-start i) #f)
                             (start i))
                            ((#\x01)
                             (getvalue (+ i 1) 
                                       type
                                       (unescape name-start i)
                                       (+ i 1)))
                            ((#\x02)
                             (getname (+ i 2) type name-start))
                            (else
                             (getname (+ i 1) type name-start)))
                          (begin
                            (add-result type (unescape name-start i) #f)
                            (start i)))))
           (getvalue (lambda (i type name value-start)
                       (if (< i (string-length str))
                           (case (string-ref str i)
                             ((#\x00 #\x03)
                              (add-result type name (unescape value-start i))
                              (start i))
                             ((#\x02)
                              (getvalue (+ i 2) type name value-start))
                             (else
                              (getvalue (+ i 1) type name value-start)))
                           (begin
                             (add-result type name (unescape value-start i))
                             (start i))))))
    (start 0)))
