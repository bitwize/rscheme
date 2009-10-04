
(define (t)
  (xs "<a p='1' color='red'>&lt;foo the <b>silly</b> &ldquo;cat&rdquo;</a>"))

(define (ts)
  (xs (file->string "sample.xml") #f))

(define (tso)
  (xs (file->string "sample_oo.xml") #f))

(define-macro (xpath-str node expr)
  `(xpath:node-set->string (xpath () ,node ,expr)))

(define (gmr-read)
  (with-module
      util.xpath
    (with-module
        util.xml
      (letrec-syntax ((nss (syntax-form (ns)
                             (xpath:node-set->string ns)))
                      (nsn (syntax-form (ns)
                             (string->number (xpath:node-set->string ns)))))
        (for-each
         (lambda (c)
           (let ((col (nsn (xpath () c "@Col")))
                 (row (nsn (xpath () c "@Row"))))
             (format #t "~a~d = ~s\n" 
                     (string-ref "ABCDEFGHIJKLMNOPQRSTUVWXYZ" col)
                     row
                     (nss (xpath () c "gmr:Content")))))
         ;;
         (xpath () (ts)
                "gmr:Workbook/gmr:Sheets/gmr:Sheet[gmr:Name='Sheet1']/gmr:Cells/gmr:Cell"))))))
