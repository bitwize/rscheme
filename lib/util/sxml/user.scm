
(define (parse-then-remainder (self <input-port>) nm ns-list remains)
  (let ((p (open-xml-token-string self nm)))
    ;;
    (for-each
     (lambda (ns)
       ;;
       ;; Set up the abbreviations table, which maps FQ prefixes
       ;; like 'http://schemas.xmlsoap.org/soap/envelope/' to
       ;; abbreviations that the application wants to use like 'SOAP-ENV'
       ;;
       (let (((fq <string>) (cadr ns))
             ((prefix <symbol>) (car ns)))
         (table-insert! (abbrev (namespaces p))
                        (string->symbol fq) 
                        prefix)))
     ns-list)
    ;; Compute the initial namespace, which is used to parse
    ;; XML text that doesn't have the appropriate xmlns declarations.
    ;; Note that it is impossible to use the initial-ns feature
    ;; without the abbreviation feature
    (set-mappings! (namespaces p)
                   (map (lambda (ns)
                          (list (car ns) (string->symbol (cadr ns))))
                        ns-list))
    ;;
    (let* ((top (sxml-parse p))
           (buf (buffer p))
           (k (offset p)))
      (if (and (string? buf) (< k (string-length buf)))
          (if (eq? k 0)
              (remains buf)
              (remains (substring buf k)))
          (remains ""))
      top)))


;;;    
  
(define-method port->sxml ((self <pushback-input-port>) ns)
  (parse-then-remainder
   self
   (or (name self) "literal")
   ns
   (lambda (rest)
     (if (not (string=? rest ""))
         (input-port-pushback self rest)))))

(define-method port->sxml ((self <input-port>) ns)
  (let* ((r "")
         (t (parse-then-remainder
             self
             (or (name self) "literal")
             ns
             (lambda (rest)
               (set! r rest)))))
    (values t r)))

(define (string->sxml (buf <string>) ns-list)
  (let* ((r "")
         (t (parse-then-remainder 
             (open-input-string buf)
             "literal"
             ns-list
             (lambda (rest)
               (set! r rest)))))
    (values t r)))

    
