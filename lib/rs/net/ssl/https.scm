;;;
;;;  This file illustrates how to use rs.net.ssl to implement
;;;  an https server (although rs.net.httpd will at some point
;;;  incorporate this directly into it)
;;;

,(use rs.net.httpd
      syscalls
      rs.sys.threads.manager
      util.xml
      rs.net.ssl)

(define *space* (make-web-space))
(define *top* (make-uri-directory))

(uri-link-add! *space* "" *top*)
(uri-link-add! *top* "foo.txt" (make-uri-literal-node
                                "Hello, world\n"
                                mime-type: "text/plain"))

(define (echo-page path req)
  (values
   (call-with-output-string
    (lambda (p)
      (format p "Hello, my friend\n\n")
      (let ((s (underlying-http-socket (current-http-connection))))
        (if (and (has-certificate? s) (certificate s))
            (format p "You presented: ~s\n" (certificate s))
            (format p "You did not present a certificate :-(\n")))
      (format p "\n\nSee you later\n")))
   "text/plain"))

(define-class <chunky-content> (<object>)
  items)

(define-method content-length ((self <chunky-content>))
  (reduce + 0 (map string-length (items self))))

(define-method send-content ((self <chunky-content>) dest)
  (for-each
   (lambda (item)
     (write-string dest item))
   (items self)))

(define (make-chunked-encoding chunks)
  (let ((q (make-dequeue)))
    (for-each
     (lambda ((c <string>))
       (dequeue-push-back! q (~ "\r\n~x\r\n" (string-length c)))
       (dequeue-push-back! q c))
     chunks)
    (dequeue-push-back! q "\r\n0\r\n\r\n")
    ;;
    (make <chunky-content>
          items: (vector->list (dequeue-state q)))))

(define (chunkify str)
  (if (< (string-length str) 4096)
      str
      (make-chunked-encoding
       (map (lambda (k)
              (let* ((i (* k 4096))
                     (j (+ i 4096)))
                (if (< j (string-length str))
                    (substring str i j)
                    (substring str i))))
            (range (quotient (+ (string-length str) 4095) 4096))))))

(define (echo-big-page path req)
  (let ((n (if (pair? path)
               (string->number (car path))
               500)))
    (values
     (chunkify
      (sxml->string
       `(html
         (head
          (title "Big Page Response"))
         (body
          (h1 "Big Page Response")
          (p "Hello, my friend") "\n\n"
          (h2 "Presented Certificate")
          ,(cond
            ((certificate (underlying-http-socket (current-http-connection)))
             => (lambda (c)
                  `(p "You presented: " 
                      (b ,(to-string c))
                      ".  Thank you!")))
            (else
             `(i "You did not present a certificate :-(")))
          "\n"
          (h2 "See you later") "\n"
          ,@(map (lambda (appx)
                   `(div
                     (h2 "Appendix " ,appx) "\n"
                     ,@(map (lambda (j)
                              `(p "Some information about "
                                  ,(~ "~a[~a]" appx j)))
                            (range n))
                     (p "ok?")))
                 (map string (string->list "ABCDEFGHIJKLMNOPQRSTUVWXYZ")))))))
     "text/html")))

(uri-link-add! *top* "echo"
               (make-uri-simple-script echo-page))
(uri-link-add! *top* "echobig"
               (make-uri-simple-script echo-big-page))

;;;
;;;  Now, we can:
;;;
;;;     curl https://localhost:9666/foo.txt
;;;

(define (process-ssl-connection (ssl <ssl-socket>))
  (format #t "Processing: ~s\n" ssl)
  (let ((pin (plaintext-in ssl))
        (pout (plaintext-out ssl)))
     ;;
    (handle-http-socket-connection *space* 0 ssl (current-output-port) #t)))

(define (https-server)
  (let ((sock (open-server-socket 9666)))
    (let loop ()
      (bind ((cnx peer (get-next-client (service sock)))
             (ssl (make-sslmgr cnx peer '("-Tsecurity/root.cert"
                                          "-Asecurity/root.cert"
                                          "-e"
                                          "-csecurity/server.cert"
                                          "-ksecurity/server.private"))))
        (thread-resume
         (make-thread
          (lambda ()
            (handler-case
             (format #t "Certificate is => ~s\n" (certificate ssl))
             ((<terminated-thread-exception>)
              (format #t "Never got a certificate!\n"))))))
        ;;
        (process-ssl-connection ssl))
      (loop))))
