
(define-class <log-fileset> (<object>)
  (write-access-lock)
  (fileset-directory type: <directory-name>)
  (current-file-port init-value: #f)
  (current-file-name type: <file-name>)
  (rollover-file-name type: <string>)
  (max-file-size type: <fixnum> init-value: 10000000)
  (max-keep-rollovers type: <fixnum> init-value: 10))

(define (open-log-fileset #key 
                          (basefile default: #f)
                          (directory default: "/tmp")
                          (pattern default: '("log" "log.~d"))
                          (max-size default: 10000000)
                          (max-files default: 10))
  (let ((d (if basefile
               (or (file-directory (string->file basefile)) 
                   (string->dir "."))
               (string->dir directory))))
    (make <log-fileset>
          write-access-lock: (make-semaphore 
                              (format #f "~a-log-lock" (or basefile directory))
                              1)
          fileset-directory: d
          current-file-name: (if basefile
                                 (string->file basefile)
                                 (append-path d (string->file (car pattern))))
          rollover-file-name: (if basefile
                                  (string-append basefile ".~d")
                                  (cadr pattern))
          max-file-size: max-size
          max-keep-rollovers: max-files)))

(define (rollover-file (self <log-fileset>) (i <fixnum>))
  (pathname->os-path
   (append-path (fileset-directory self)
                (string->file
                 (format #f (rollover-file-name self) i)))))

(define (do-file-rollovers (self <log-fileset>))
  (let loop ((k (- (max-keep-rollovers self) 1))
             (to #f))
    (if (>= k 0)
        (let ((from (if (= k 0)
                        (pathname->os-path (current-file-name self))
                        (rollover-file self k))))
          ;(format #t "rename ~s => ~s\n" from to)
          (if (stat from)
              (if to
                  (rename from to)
                  (unlink from)))
          (loop (- k 1) from)))))
                                                   
(define (open-output-log-file (self <log-fileset>))
  (let* ((fn (pathname->os-path (current-file-name self)))
         (sb (stat fn)))
    ;; do any necessary rollovers
    (if (and sb (>= (stat-size sb) (max-file-size self)))
        (let ((f (open-output-append-file fn)))
          (format f "~a: ******** rollover (size ~d >= ~d)\n"
                  (time->msg-time (time))
                  (stat-size sb)
                  (max-file-size self))
          (flush-output-port f)
          (close-output-port f)
          (do-file-rollovers self)))
    ;; open the output file
    (let ((f (open-output-append-file fn)))
      (set-current-file-port! self f)
      f)))

(define-method with-output-to-log-file ((self <output-port>) thunk)
  (with-output-to-port self thunk))

(define-method with-output-to-log-file ((self <log-fileset>) thunk)
  (semaphore-wait (write-access-lock self))
  (let ((p (open-output-log-file self)))
    ;;
    (handler-case
     (with-output-to-port p thunk)
     ((<condition> condition: c)
      (close-output-port p)
      (set-current-file-port! self #f)
      (semaphore-signal (write-access-lock self))
      (signal c)))
    ;;
    (flush-output-port p)
    (close-output-port p)
    (set-current-file-port! self #f)
    ;;
    (semaphore-signal (write-access-lock self))))

(define-class <log-fileset-msg-dest> (<message-dest>)
  underlying-log-fileset)

(define-method log-file-message-dest ((self <log-fileset>))
  (make <log-fileset-msg-dest>
        underlying-log-fileset: self))

(define-method log-file-message-dest ((self <message-dest>))
  self)

(define-method display-message ((self <log-fileset-msg-dest>)
                                (msg <message>)
                                (argv <vector>)
                                place)
  (let ((t (time->msg-time (time)))
        (s (open-output-string)))
    (display-message s msg argv place)
    (let ((l (string-split (close-output-port s) #\newline)))
      (with-output-to-log-file
       (underlying-log-fileset self)
       (lambda ()
         (let ((p (current-output-port)))
           (let loop ((l l))
             (if (or (null? l)
                     (and (string=? (car l) "") (null? (cdr l))))
                 (values)
                 (begin
                   (format p "~a: ~a\n" t (car l))
                   (loop (cdr l)))))))))))
    
#|
(define *l* (open-log-fileset max-files: 3 max-size: 1000))


             
(define (t)
  (with-message-dest 
   (log-file-message-dest *l*)
   (lambda ()
     (dm 101 "This is a test")
     (dm 102 "More testing")
     (dm 103 "Blah...\nblah...\nblah..."))))

  
|#
