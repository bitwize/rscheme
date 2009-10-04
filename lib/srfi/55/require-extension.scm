
(define (require-srfi-extension envt args)
  (for-each
   (lambda (a)
     (if (and (integer? a) (>= a 0))
         (let ((mname (symbol-append "srfi." (to-string a))))
           (use-in mname envt))
         (error 
          "srfi <extension-clause> argument not a non-negative integer: ~s"
          a)))
   args))

(define *extension-classes* (list (list 'srfi require-srfi-extension)))

(define (require-extension* envt clause)
  (if (pair? clause)
      (let ((a (assq (car clause) *extension-classes*)))
        (if a
            ((cadr a) envt (cdr clause))
            (error "~s: unrecognized <extension-identifier>: not one of ~s"
                   (car clause)
                   (map car *extension-classes*))))
      (error "~s: invalid <clause>: not of the form (<extension-identifier> <extension-argument> ..."
             clause)))

            
;;; XXX this doesn't work compiled, because it wants
;;; to run in the environment of the compiler

(define-rewriter (require-extension form)
  (if (not (list? (cdr form)))
      (error "~s: Not of the form (require-extension <clause> ...)" form))
  ;;
  (for-each (lambda (c)
              (require-extension* $envt c))
            (cdr form))
  '(begin))
