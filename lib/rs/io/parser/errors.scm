;;;
;;;  parse errors

(define-class <parse-error> (<condition>) :abstract
  (source type: <input-port>)
  (token type: <token>))
  
(define-class <mismatched-delimiter> (<parse-error>))

(define-class <unterminated-list> (<parse-error>))
(define-class <unterminated-vector> (<parse-error>))
(define-class <misplaced-dot> (<parse-error>))
(define-class <unterminated-sqlist> (<parse-error>))
(define-class <expected-close-paren-after-tail> (<parse-error>))
(define-class <missing-modified-item> (<parse-error>)
  modifier)

(define-class <no-datum-for-sqlist> (<condition>)
  sqlist)

(define-method display-object ((self <mismatched-delimiter>) port)
  (format port "~a: mismatched delimiter ~a\n"
          (location (token self))
          (class-name (object-class (token self)))))

;;

;;;

(&module
 (export <parse-error> <mismatched-delimiter> <unterminated-list> 
         <unterminated-vector> <misplaced-dot> <unterminated-sqlist>
         <expected-close-paren-after-tail> <missing-modified-item> 
         <no-datum-for-sqlist>))
