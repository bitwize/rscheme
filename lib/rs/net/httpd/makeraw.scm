,(use syscalls
      util.xml
      regex
      rs.net.httpd
      rs.net.pem)

(define (load-raw path)
  (let ((sb (stat path)))
    (case (stat-type sb)
      ((regular)
       (load-raw-literal path))
      ((directory)
       `(directory (@ (listable "1"))
                   ,@(map (lambda (f)
                            `(entry (@ (name ,f))
                                    ,(load-raw (string-append path "/" f))))
                          (select (lambda (f)
                                    (loadable? 
                                     f 
                                     (stat (string-append path "/" f))))
                                  (scandir path))))))))

(define (loadable? (name <string>) stat)
  (and stat
       (not (member name '("." "..")))
       (memq (stat-type stat) '(regular directory))
       (not (char=? (string-ref name (- (string-length name) 1)) #\~))))

(define (load-raw-literal path)
  (let ((data (file->string path))
        (encoding '()))
    (if (has-binary-content? data)
        (begin
          (set! encoding '((encoding "pem")))
          (set! data (pem-encode data))))
    `(literal (@ (mimetype ,(guess-mime-type path)) ,@encoding)
              ,data)))

            
(define *binary-pattern* (reg-expr->proc '(not (or #\cr #\lf #\tab
                                                   (range #\space #\~)))))

(define (has-binary-content? str)
  (and (*binary-pattern* str) #t))

(define (disk->raw-image source dest)
  (let ((x `(webspace ,(load-raw source))))
    (call-with-output-file
        dest
      (lambda (port)
        (write-sxml x port)))))


