
(define-class <token> (<object>) :abstract
  (location type: <text-location>)
  (lexeme-length type: <fixnum> init-value: 1))

(define-class <leaf-token> (<token>) :abstract)

(define-method print-data ((self <token>))
  ;; nothing
  )

(define-class <boolean-token> (<leaf-token>)
  (data type: <boolean>))

(define-class <char-token> (<leaf-token>)
  (data type: <char>))

(define-class <syntactic-keyword-token> (<leaf-token>)
  (data type: <symbol>))

(define-method print ((self <token>))
  (format #t "~a: ~a" 
	  (location self)
	  (class-name (object-class self)))
  (print-data self)
  (format #t " (~d)\n" (lexeme-length self))
  self)

(define-class <open-paren-token> (<token>))
(define-class <open-vector-token> (<token>))
(define-class <close-paren-token> (<token>))
(define-class <open-sqbracket-token> (<token>))
(define-class <close-sqbracket-token> (<token>))
(define-class <quote-token> (<token>))
(define-class <backquote-token> (<token>))
(define-class <unquote-token> (<token>))
(define-class <unquote-splicing-token> (<token>))
(define-class <dot-token> (<token>))

(define-class <identifier-token> (<leaf-token>)
  (data type: <symbol>))

(define-class <keyword-token> (<leaf-token>)
  (data type: <keyword>))

(define-class <flag-token> (<leaf-token>)
  (data type: <flag>))

(define-method print-data ((self <identifier-token>))
  (write-char #\space)
  (display (data self)))

(define-method print-data ((self <flag-token>))
  (write-char #\space)
  (display (data self)))

(define-method print-data ((self <keyword-token>))
  (write-char #\space)
  (display (data self)))

(define-class <numeric-token> (<leaf-token>)
  (data type: <number>))

(define-method print-data ((self <numeric-token>))
  (write-char #\space)
  (display (data self)))

(define-class <string-token> (<leaf-token>)
  (data type: <string>))

(define-method print-data ((self <string-token>))
  (write-char #\space)
  (write (data self)))

(define-class <curly-braced-token> (<leaf-token>)
  (data type: <string>))

;;

(&module
 (export <token> data <leaf-token> <boolean-token> <char-token>
         <syntactic-keyword-token> <open-paren-token> <open-vector-token>
         <close-paren-token> <close-sqbracket-token> <quote-token>
         <backquote-token> <unquote-splicing-token> <dot-token>
         <identifier-token> <flag-token> <keyword-token>
         <numeric-token> <string-token>
         <curly-braced-token> <unquote-token> <open-sqbracket-token>))
