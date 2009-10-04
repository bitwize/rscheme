,(use regex tables)

;;;
;;;  An implementation of HTTP/1.0
;;;

(define *spaces-regex* (reg-expr->proc '(+ space)))

(define (split-words str)
  (string-split str *spaces-regex*))

(define *method-table* (make-string-ci-table))
(define *response-keywords* (make-symbol-table))

(table-insert! *response-keywords* 'connection "Connection")
(table-insert! *response-keywords* 'content-length "Content-Length")
(table-insert! *response-keywords* 'content-type "Content-Type")
(table-insert! *response-keywords* 'cache-control "Cache-Control")
(table-insert! *response-keywords* 'expires "Expires")
(table-insert! *response-keywords* 'last-modified "Last-Modified")

(table-insert! *method-table* "OPTIONS" 'options)
(table-insert! *method-table* "GET" 'get)
(table-insert! *method-table* "HEAD" 'head)
(table-insert! *method-table* "POST" 'post)
(table-insert! *method-table* "PUT" 'put)
(table-insert! *method-table* "DELETE" 'delete)
(table-insert! *method-table* "TRACE" 'trace)
(table-insert! *method-table* "CONNECT" 'connect)

;;; These support WebDAV...

(table-insert! *method-table* "PATCH" 'patch)
(table-insert! *method-table* "PROPFIND" 'propfind)
(table-insert! *method-table* "PROPPATCH" 'proppatch)
(table-insert! *method-table* "MKCOL" 'mkcol)
(table-insert! *method-table* "COPY" 'copy)
(table-insert! *method-table* "MOVE" 'move)
(table-insert! *method-table* "LOCK" 'lock)
(table-insert! *method-table* "UNLOCK" 'unlock)

(define (parse-request-header (client <http-connection>) (tstart <pair>))
  (let* ((req (strip-cr (read-line (input-port (socket client)))))
         (t0 (time)))
    (set-car! tstart t0)
    (if (eof-object? req)
        #f
        (parse-request-header* client req t0))))

(define (http-method (client <http-connection>) req t0 method parts)
  (bind ((uri version (case (length parts)
                        ((3) (values (cadr parts) (caddr parts)))
                        ((2) (values (cadr parts) "HTTP/0.9"))
                        (else 
                         (wm 183 "Bad request line ~s" req)
                         (httpd-bail 400 "Bad Request; Incomprehensible"))))
         (uri query (strip-query uri)))
    (dm 154 "uri = ~s" uri)
    (dm 157 "query = ~s" query)
    (dm 155 "version = ~s" version)
    ;;
    (http-read-headers (input-port (socket client))
                       (list (cons '%start-time t0)
                             (cons 'request-version version)
                             (cons 'request-path 
                                   (map url-decode-step
                                        (string-split uri #\/)))
                             (cons 'request-query query)
                             (cons 'request-type method)))))

(define (method-table-for-connection (self <http-connection>))
  (let ((b (http-binding self)))
    (if b
        (method-table b)
        *method-table*)))

(define (parse-request-header* (client <http-connection>) req t0)
  (dm 151 "Request Line ~@*#1000s" req)
  (if (eof-object? req)
      (begin
        (wm 181 "Missing request line")
        (httpd-bail 400 "Bad Request; Missing")))
  (let* ((parts (split-words req))
         (method (table-lookup (method-table-for-connection client)
                               (car parts))))
    (if (not method)
        (begin
          (wm 182 "Unknown method ~s" (car parts))
          (httpd-bail 501 "Not Implemented; ~s" (car parts))))
    ;;
    (dm 152 "Method ~s" method)
    (dm 153 "Request parts ~s" (cdr parts))
    (if (symbol? method)
        (http-method client req t0 method parts)
        (begin
          (method client req t0 parts)
          (values)))))


(define (http-read-headers port #optional initial)
  (let loop ((h (or initial '())))
    (let ((line (strip-cr (read-line port))))
      (if (or (eof-object? line)
              (eq? (string-length line) 0))
          (reverse! h)
          (bind ((s e key value (*header-line* line)))
            (if s
                (begin
                  (dm 156 " http header ~s => ~s" key value)
                  (loop (cons (cons (header-field-symbol key) value) h)))
                (begin
                  (wm 184 " bad header: ~s" line)
                  (loop (cons (cons 'unknown line) h)))))))))

  
(define (strip-query uri)
  (let ((x (string-search uri #\?)))
    (if x
        (values (substring uri 0 x) (substring uri (+ x 1)))
        (values uri #f))))

(define (strip-cr str)
  (if (string? str)
      (let (((n <fixnum>) (string-length str)))
        (if (and (fixnum>? n 0)
                 (char=? (string-ref str (sub1 n)) #\cr))
            (substring str 0 (sub1 n))
            str))
      str))
      
(define *header-line*
  (reg-expr->proc
   '(prefix
     (seq 
      (save (+ (not (or space #\:))))
      #\:
      (+ space)
      (save (+ any))))))

(define (header-field-symbol str)
  (string->symbol
   (list->string (map char-downcase (string->list str)))))

(define (send-header (self <http-response>) #optional (flush? default: #t))
  (if (header-sent? self)
      ; an error for now; may relax later...
      (error "Header already sent for ~s" self))
  ;;
  (set-header-sent?! self #t)
  ;;
  (if (keep-alive? self)
      (set-header-field! self 'connection "keep-alive"))
  ;;
  (let ((p (output-port (socket (client (request self))))))
    ;;
    (define (response-line fmt . args)
      (let ((l (apply format #f fmt args)))
        (dm 130 "<< ~a" l)
        (write-string p l)
        (write-string p "\r\n")))
    ;;
    (response-line "HTTP/1.1 ~03d ~a" 
                   (status-code self)
                   (reason-phrase self))
    (for-each (lambda (h)
                (let ((k (car h))
                      (v (cdr h)))
                  (response-line "~a: ~a"
                                 (or (table-lookup *response-keywords* k) k)
                                 v)))
              (reverse (header-fields self)))
    (format p "\r\n")
    (if flush? 
        (flush-output-port p))))

(define (suppress-body-content! (self <http-response>))
  ;; set the length, but don't actually have any content
  (set-header-field! self 
                     'content-length
                     (content-length (content self)))
  (set-content! self #f))

(define-method content-mtime ((self <object>))
  #f)


(define (flush-http-response (self <http-response>))
  (if (not (status-code self))
      (set-result! self 200 "OK"))
  ;;
  (if (not (header-sent? self))
      (begin
        (if (content self)
            (begin
              (if (not (assq 'content-length (header-fields self)))
                  (set-header-field! self 
                                     'content-length 
                                     (content-length (content self))))
              (if (not (assq 'last-modified (header-fields self)))
                  (let ((t (content-mtime (content self))))
                    (if t
                        (set-header-field! 
                         self
                         'last-modified
                         (time->string t "%a, %d %b %Y %H:%M:%S GMT" #f)))))))
        (send-header self #f)))
  (let ((p (output-port (socket (client (request self))))))
    (if (content self)
        (begin
          (send-content (content self) p)
          (set-content! self #f))) ; mark content as sent
    (flush-output-port p)))

;;;

(define-method send-content ((self <string>) dest)
  (write-string dest self))

;;;

(define-class <sxml-content> (<object>)
  (sxml-content type: <pair>)
  (cached-content-length init-value: #f)
  (cached-content init-value: #f))

(define (wrapped-sxml-content (self <sxml-content>))
  (let ((d (sxml-content self)))
    ;; if it's a whole document, then let it through untweaked.
    ;; otherwise, wrap it in some HTML stuff.  This allows the
    ;; caller to set the DOCTYPE and add an XML decl if they want
    (if (and (pair? d) (eq? (car d) '*TOP*))
        d
        `(*TOP* ,$html-doctype-decl
                ,d
                "\n"
                (*COMMENT* "rs.net.httpd")))))

(define-method content-length ((self <sxml-content>))
  (or (cached-content-length self)
      (let (((p <string>) (sxml->string (wrapped-sxml-content self))))
        (set-cached-content! self p)
        (set-cached-content-length! self (string-length p))
        (string-length p))))

(define-method send-content ((self <sxml-content>) dest)
  (if (cached-content self)
      (write-string dest (cached-content self))
      (write-sxml (wrapped-sxml-content self) dest)))

(define $html-doctype-decl
  '(*DECL* DOCTYPE HTML 
           PUBLIC "-//W3C//DTD HTML 4.01//EN" 
           "http://www.w3.org/TR/html4/strict.dtd"))

(define (sxml->content s)
  (make <sxml-content>
        sxml-content: s))

;;;

(define-class <port-content> (<object>)
  underlying-input-port
  content-length
  (owner? type: <boolean> init-value: #f)
  (content-mtime init-value: #f))

(define-method content-length ((self <string>))
  (string-length self))

(define-method send-content ((self <port-content>) dest)
  (let ((src (underlying-input-port self)))
    (let loop ((n (content-length self)))
      (if (> n 0)
          (let ((buf (read-string src (min n 65500))))
            (write-string dest buf)
            (loop (- n (string-length buf))))
          (if (owner? self)
              (close-input-port src))))))

(define (file->content f)
  (let* ((sb (or (stat f)
                 (error "could not stat ~s" f)))
         (n (stat-size sb)))
    (if (< n 1000)
        (file->string f)
        (make <port-content>
              content-mtime: (stat-mtime sb)
              underlying-input-port: (open-input-file f)
              owner?: #t
              content-length: n))))


;;; decode the VALUE part of a form field in
;;; an application/x-www-form-urlencoded block

(define *form-esc* (reg-expr->proc '(or "+" 
                                        (seq "%" (save 
                                                  (seq hex-digit 
                                                       hex-digit))))))

(define-macro (filter-string str regex regex-extras (port index) . body)
  `(let ((,port #f))
     (let loop ((k 0))
       (bind ((,index e ,@regex-extras (,regex ,str k)))
         (if ,index
             (begin
               (if (not ,port)
                   (set! ,port (open-output-string)))
               (write-string ,port (substring ,str k ,index))
               ,@body
               (loop e))
             (if (eq? k 0)
                 ,str
                 (begin
                   (write-string ,port (substring ,str k))
                   (get-output-string ,port))))))))
  

(define (form-decode value)
  (filter-string value
                 *form-esc* (save)
                 (o s)
                 (if save
                     (write-char (integer->char (string->number save 16)) o)
                     (write-char #\space o))))

(define *form-desc* (reg-expr->proc '(not (or alpha digit))))

(define (form-encode value)
  (filter-string value
                 *form-desc* ()
                 (out i)
                 (let ((ch (string-ref value i)))
                   (if (eq? ch #\space) 
                       (write-string out "+")
                       (format out "%~02x" (char->integer ch))))))

;;; decode one step of a URL

(define *url-esc* (reg-expr->proc '(seq "%" (save (seq hex-digit hex-digit)))))

(define (url-decode-step (str <string>))
  (filter-string str
                 *url-esc* (save)
                 (o s)
                 (write-char (integer->char (string->number save 16)) o)))

(define *url-desc* (reg-expr->proc '(not (or alpha digit #\_ #\, #\. #\-))))

(define (url-encode-step (str <string>))
  (filter-string str
                 *url-desc* ()
                 (o i)
                 (format o "%~02x" (char->integer (string-ref str i)))))
                                     
;;;

(define-method decode-request-query ((req <http-request>))
  (let ((q (get-property req 'request-query #f)))
    (if q
        (decode-request-query q)
        '())))

(define-method decode-request-query ((q <string>))
  (if (string=? q "")
      '()
      (map (lambda (parm)
             (let ((l (string-split parm #\=)))
               (if (= (length l) 2)
                   (cons (car l) (form-decode (cadr l)))
                   (cons (car l) #t))))
           (string-split q #\&))))

(define (encode-request-query (items <list>))
  (string-join #\& (map (lambda (p)
                          (string-append (to-string (car p))
                                         "="
                                         (form-encode (cdr p))))
                        items)))
  
