;;;  Additional namespaces are supplied in the following format:
;;;
;;;      namespace-spec ::= (namespace-decl ...)
;;;      namespace-decl ::= ([PREFIX] LOCAL-NAME URI)
;;;
;;;     where LOCAL-NAME is the prefix (symbol) that will be used 
;;;                      in the returned SXML,
;;;           URI is the (string) which identifies the namespace,
;;;           PREFIX, if present, is the prefix (symbol) that
;;;                   occurs in the XML text.

;;;
;;;  P.S.  I'm not actually sure that XML containing undeclared prefixes
;;;        is valid, but in practice some documents (e.g., the GnuCash ledger)
;;;        contain them...

(define (ssax:port->sxml port namespace-spec)
  (let ((namespaces (map 
                     (lambda (s)
                       (if (= (length s) 3)
                           (list (car s)
                                 (cadr s)
                                 (ssax:uri-string->symbol (caddr s)))
                           (list #f 
                                 (car s)
                                 (ssax:uri-string->symbol (cadr s)))))
                     namespace-spec))
        (top-namespaces (map
                         (lambda (s)
                           (if (= (length s) 3)
                               (cdr s)
                               s))
                         namespace-spec))
        (resmap (make-symbol-table)))
    
    (define (RES-NAME->SXML (res-name <pair>))
      (let ((alt (table-lookup resmap (car res-name))))
        (symbol-append (or (table-lookup resmap (car res-name))
                           (car res-name))
                       ":" (cdr res-name))))
    ;;
    (for-each (lambda (ns)
                (table-insert! resmap (caddr ns) (cadr ns)))
              namespaces)
    ;;
    (let* ((parser (ssax:make-parser
                    NEW-LEVEL-SEED 
                    (lambda (elem-gi attributes namespaces
                                     expected-content seed)
                      '())
                    ;;
                    FINISH-ELEMENT
                    (lambda (elem-gi attributes namespaces parent-seed seed)
                      (let ((seed (ssax:reverse-collect-str-drop-ws seed))
                            (attrs
                             (attlist-fold
                              (lambda (attr accum)
                                (cons (list 
                                       (if (symbol? (car attr)) (car attr)
                                           (RES-NAME->SXML (car attr)))
                                       (cdr attr)) accum))
                              '() attributes)))
                        (cons
                         (cons 
                          (if (symbol? elem-gi) elem-gi
                              (RES-NAME->SXML elem-gi))
                          (if (null? attrs) seed
                              (cons (cons '@ attrs) seed)))
                         parent-seed)))
                    
                    CHAR-DATA-HANDLER
                    (lambda (string1 string2 seed)
                      (if (string-null? string2) (cons string1 seed)
                          (cons* string2 string1 seed)))
                    
                    DOCTYPE
                    (lambda (port docname systemid internal-subset? seed)
                      (when internal-subset?
                            (ssax:warn port
                                       "Internal DTD subset is not currently handled ")
                            (ssax:skip-internal-dtd port))
                      (ssax:warn port "DOCTYPE DECL " docname " "
                                 systemid " found and skipped")
                      (values #f '() namespaces seed))
                    
                    UNDECL-ROOT
                    (lambda (elem-gi seed)
                      (values #f '() namespaces seed))
                    
                    PI
                    ((*DEFAULT* .
                                (lambda (port pi-tag seed)
                                  (cons
                                   (list '*PI* pi-tag (ssax:read-pi-body-as-string port))
                                   seed))))
                    ))
           ;;
           (result (reverse (parser port '()))))
      ;;
      (cons '*TOP*
	    (if (null? namespace-spec) result
		(cons
		 (list '@@ (cons '*NAMESPACES* top-namespaces))
                 result))))))
