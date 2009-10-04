
(def (random-id)
  (random (expt 2 16)))

(define-class <message> (<object>)
  (id                 init-function: random-id)
  (qr                 init-value: #f)
  (opcode             init-value: 'QUERY)
  (aa                 init-value: #f)
  (tc                 init-value: #f)
  (rd                 init-value: #t)
  (ra                 init-value: #f)
  (z                  init-value: 0)
  (rcode              init-value: 'NO-ERROR)
  (question-section   init-value: '())
  (answer-section     init-value: '())
  (authority-section  init-value: '())
  (additional-section init-value: '()))

(def (message? o)
  (instance? o <message>))

(def (message-nxdomain? msg)
  (eq? 'NAME-ERROR (rcode msg)))

(define-method write-object ((self <message>) port)

  (format port
"#[<message>
  id:     ~a
  qr:     ~a
  opcode: ~a
  aa:     ~a
  tc:     ~a
  rd:     ~a
  ra:     ~a
  z:      ~a
  rcode:  ~a~%"
(id self) (qr self) (opcode self) (aa self) (tc self) (rd self) (ra self)
(z self) (rcode self))

(format port "  question-section:~%")
(for-each (fn (q) (format port "    ~a~%" (write-object-to-string q)))
	  (question-section self))

(format port "  answer-section:~%")
(for-each (fn (q) (format port "    ~a~%" (write-object-to-string q)))
	  (answer-section self))

(format port "  authority-section:~%")
(for-each (fn (q) (format port "    ~a~%" (write-object-to-string q)))
	  (authority-section self))

(format port "  additional-section:~%")
(for-each (fn (q) (format port "    ~a~%" (write-object-to-string q)))
	  (additional-section self)))

(define-enum-table opcode
  '((QUERY  . 0)
    (IQUERY . 1)
    (STATUS . 2)))

(define-enum-table rcode
  '((NO-ERROR        . 0)
    (FORMAT-ERROR    . 1)
    (SERVER-FAILURE  . 2)
    (NAME-ERROR      . 3)
    (NOT-IMPLEMENTED . 4)
    (REFUSED         . 5)))
    
  