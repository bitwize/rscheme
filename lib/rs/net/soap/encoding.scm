(define (soap-encode-sequence seq)
  (map soap-encode-value seq))

(define-method soap-encode-value ((self <string>))
  `(enc:string ,self))

(define-method soap-encode-value ((self <integer>))
  `(enc:int ,(to-string self)))

(define-method soap-encode-value ((self <list>))
  (cond
   ;;
   ((every? string? self)
    `(enc:Array
      (@ (enc:arrayType "xsd:string[]"))
      ,@(soap-encode-sequence self)))
   ;;
   ((every? integer? self)
    `(enc:Array
      (@ (enc:arrayType "xsd:int[]"))
      ,@(soap-encode-sequence self)))
   ;;
   (else
    `(enc:Array
      (@ (enc:arrayType "xsd:anyType[]"))
      ,@(soap-encode-sequence self)))))

(define (sxml:build-id-map sxml #optional (attr default: 'id))
  (let ((imap (make-string-table)))
    ;;
    (define (scan sxml)
      (if (and (sxml:element? sxml)
               (pair? (cdr sxml)))
          (begin
            (let ((id (assq attr (sxml:attributes sxml))))
              (if id
                  (table-insert! imap (cadr id) sxml)))
            (for-each scan (sxml:children sxml)))))
    ;;
    (scan sxml)
    ;;
    imap))

(define (soap-decode-sequence seq)
  (let ((imap #f))
    ;;
    (define (href k)
      (if (not imap)
          (begin
            (set! imap (sxml:build-id-map (cons 'all seq)))
            ;(print imap)
            ))
      (or (table-lookup imap k)
          (error "No such referent: ~s" k)))
    ;;
    (soap-decode-sequence* seq href)))

(define (soap-decode-sequence* seq href)
  (map (lambda (n)
         (soap-decode-value n href))
       (select sxml:element? seq)))

(define (soap-decode-value* item href)
  (let ((h (assq 'href (sxml:attributes item))))
    (if h
        (if href
            (soap-decode-value* (href (substring (cadr h) 1)) href)
            (error "`href' tag not supported: ~s" item))
        (case (car item)
          ((enc:int xsd:int) (list (string->number (xpath:node->string item))))
          ((enc:string xsd:string) (list (xpath:node->string item)))
          ((enc:Array xsd:Array) (soap-decode-sequence* (sxml:children item)
                                                        href))
          #|
          ((axis:fileset
            axis:snapshot
            axis:user
            axis:password
            axis:step) (list (xpath:node->string item)))
          ((axis:file) (soap-decode-sequence* (sxml:children item) href))
          ((axis:path) (cons `(@ (arrayType "xsd:anyType[3]"))
                             (soap-decode-sequence* (sxml:children item) href)))
          |#
          (else
           (error "Cannot decode SOAP type: ~s" (car item)))))))

(define (soap-decode-value item #optional href)
  (cons (car item)
        (soap-decode-value* item href)))


;;;

(define (base64-gzip-decode x)
  (uncompress
   (pem-decode
    (string-join "" (string-split x (reg-expr->proc '(+ space)))))))

(define (base64-gzip-encode x)
  (let ((o (open-output-string))
        (s (pem-encode (compress x))))
    (let loop ((i 0))
      (if (< i (string-length s))
          (let ((j (min (string-length s) (+ i 72))))
            (write-string o (substring s i j))
            (write-string o "\n")
            (loop j))
          (close-output-port o)))))
