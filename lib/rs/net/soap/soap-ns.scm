
(define *soap-local-namespaces*
  '((env "http://schemas.xmlsoap.org/soap/envelope/")
    (enc "http://schemas.xmlsoap.org/soap/encoding/")
    (xsd "http://www.w3.org/2001/XMLSchema")
    (xsi "http://www.w3.org/2001/XMLSchema-instance")
    (axis "http://xynthesis.com/soap/axis/1.0")))

(define *soap-xml-ns-decls*
  (map (lambda (ns)
         (list (symbol-append "xmlns:" (car ns)) (cadr ns)))
       *soap-local-namespaces*))
