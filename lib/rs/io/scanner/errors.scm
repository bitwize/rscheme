(define-class <lexical-error> (<condition>)
  (source type: <input-port>)
  (position type: <text-location>))

(define-class <invalid-character> (<lexical-error>)
  (character type: <char>))

(define-method display-object ((self <invalid-character>) port)
  (format port "~a: character ~s is invalid\n"
          (position self)
          (character self)))

(define-class <unterminated-string-token> (<lexical-error>))

(define-method display-object ((self <unterminated-string-token>) port)
  (format port "~a: string token is not terminated\n"
          (position self)))

(define-class <unterminated-curly-braced-token> (<lexical-error>))

(define-method display-object ((self <unterminated-curly-braced-token>) port)
  (format port "~a: curly-braced token is not terminated\n"
          (position self)))

(define-class <invalid-string-escape> (<lexical-error>)
  string-escape)

(define-method display-object ((self <unterminated-string-token>) port)
  (format port "~a: '~a' is not a valid string escape sequence\n"
          (position self)
          (list->string (vector->list (string-escape self)))))

(define-class <missing-string-escape-number> (<lexical-error>))
(define-class <string-escape-number-too-big> (<lexical-error>)
  (data type: <number>))

(define-method display-object ((self <missing-string-escape-number>) port)
  (format port "~a: expected a number for string escape\n"
          (position self)))

(define-method display-object ((self <string-escape-number-too-big>) port)
  (format port "~a: string escape number `~d' is out of range [0,65536)\n"
          (position self)
          (data self)))


(define-class <invalid-character-after-sharp> (<lexical-error>)
  (character type: <char>))

(define-method display-object ((self <invalid-character-after-sharp>) port)
  (format port "~a: invalid character `~s' after sharp (`#')\n"
          (position self)
          (character self)))
  
(define-class <invalid-sharp> (<lexical-error>)
  (sharp-lexeme type: <string>)
  (lexeme-length type: <fixnum>))

(define-class <unterminated-sharp> (<lexical-error>))

(define-class <invalid-char-name> (<lexical-error>)
  (char-name type: <string>))

(define-method display-object ((self <invalid-char-name>) port)
  (format port "~a: invalid character name ~s\n"
          (position self)
          (char-name self)))

(define-class <char-literal-out-of-range> (<lexical-error>)
  (char-code type: <integer>))

(define-class <unterminated-long-comment> (<lexical-error>))


(define-class <invalid-numeric-token> (<lexical-error>)
  (content type: <string>))


;;;

(&module
 (export <lexical-error> <invalid-character> <unterminated-string-token>
         <unterminated-curly-braced-token> <invalid-string-escape>
         <missing-string-escape-number> <string-escape-number-too-big> 
	 <invalid-character-after-sharp> 
         <invalid-sharp> <unterminated-sharp> <invalid-char-name>
         <char-literal-out-of-range> <unterminated-long-comment>
         <invalid-numeric-token>))
