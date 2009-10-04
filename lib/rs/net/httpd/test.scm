,(use rs.net.httpd
      rs.net.console
      graphics.gd
      tables)

(load "ibis-chart.scm")

(define *space* (make-web-space))

(define *top* (make-uri-directory))

(uri-link-add! *space* "" *top*)


(uri-link-add! *top* "foo.html" (make-uri-disk-node "static.html"))
(uri-link-add! *top* "bar.png" (make-uri-disk-node "testimage.png"))

(define (hello path req)
  (call-with-output-string
   (lambda (p)
     (format p "OK, folks ~s\n" (car path)))))

(define (timage path req)
  (let ((p (open-output-string))
        (img (gd-image-create-from-png "baseline.png"))
        (x (or (and (pair? path) (string->number (car path))) 0)))
    (gd-image-line img x 0 50 50 (gd-image-color-resolve img 255 0 0))
    (gd-image-line img x 1 50 51 (gd-image-color-resolve img 255 255 255))
    (gd-image-png-ctx img (open-gd-io-ctx p))
    (values (close-output-port p) "image/png")))

(uri-link-add! *top* "hello" (make-uri-simple-script (& hello)))
(uri-link-add! *top* "image" (make-uri-simple-script (& timage)))

(define (make-space . items)
  (let ((s (make-web-space)))
    (let loop ((i items))
      (if (null? i)
          s
          (begin
            (uri-link-add! s (car i) (cadr i))
            (loop (cddr i)))))))

                 #|(list "X" 
                            "127.0.0.1:1234" 
                            (make-space
                             "" (make-uri-redirect 
                                 "http://www.rscheme.org/rs/bld/0.7.3.3")))
                |#
(define (go)
  (start-http-server
   (list
    (list "X" "127.0.0.1:1234" 
          (make-space "" (make-uri-disk-dir "/u/donovan/www"))
          "/tmp/web__1234")
    (list "Y" 1235 *space* "/tmp/web__1235")))
  (start-console-server "127.0.0.1:1236" dmk "/tmp/web__1236"))

(define *ibis-trees* (make-fixnum-table))

(table-insert! *ibis-trees* 1 *test-ibis-tree*)
(table-insert! *ibis-trees* 2 '(? "We shall foo" ()))

(define (ibimage path req)
  (let ((t (table-lookup *ibis-trees* (string->number (car path)))))
    (if t
        (values (gen-ibis-chart t) "image/png")
        (values "?huh" "text/plain"))))

(define *ibis* (make-uri-directory))
(uri-link-add! *top* "ibis" *ibis*)
(uri-link-add! *ibis* "g" (make-uri-simple-script (& ibimage)))

(define *nodes* (make-uri-directory))
(uri-link-add! *ibis* "n" *nodes*)

(define (ibis-node path req)
  (let ((t (table-lookup *ibis-trees* (string->number (car path)))))
    (call-with-output-string
     (lambda (p)
       (bind ((ipng imap (gen-ibis-chart t)))
         (format p "<html>
  <head>
    <title>~a</title>
  </head><body bgcolor='white'>
  <h1>~a</h1>" (cadr t) (cadr t))
         (format p "<img src='../g/~a' ISMAP USEMAP='#hot'>"
                 (car path))
         (format p "<map name='hot'>\n")
         (for-each 
          (lambda (i)
            (format #t "==> ~s\n" i)
            (bind ((shape x y w h (list->values (car i))))
                  (format p "  <area SHAPE=RECT COORDS='~d,~d ~d,~d' HREF='~d' ALT=\"~a\">\n"
                          x y (+ x w) (+ y h)
                          y
                          (cadr i))))
          (items imap))
         (format p "</map>\n")
         (format p "\n\n</body></html>"))))))


;(uri-link-add! *nodes* "1" (make-uri-disk-node "stati1.html"))
(uri-link-add! *nodes* "1" (make-uri-simple-script 
                            (lambda (p r)
                              (ibis-node (cons "1" p) r))))
(uri-link-add! *nodes* "2" (make-uri-disk-node "stati2.html"))

(define dmk (make-basic-auth-realm "Donovan's Area"))
(add-realm-user! dmk "donovan" "private" 'donovan)

(uri-link-add! *top* "~donovan" (make-auth-area 
                                 dmk
                                 (make-uri-disk-dir "/u/donovan/www")))

(uri-link-add! *top* "rscheme.org" (make-uri-redirect "http://www.rscheme.org/"))

(uri-link-add! *top* "imported" 
               (make-uri-cached-raw "rawtest.xml" "/tmp/rawtest-cache.sto"))

(uri-link-add! *top* "doc"
               (make-uri-cached-raw "docraw.xml" "/tmp/doc.sto"))

(define (tc)
  (start-console-server "1239" dmk))

(define (blah)
  (lambda (x) x))
