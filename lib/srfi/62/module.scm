(define-module srfi.62 ()
  (&module
   (import usual-inlines)
   (implements SRFI-62 srfi-62))
  ;;
  (define (scan-sharp-comment p c)
    (input-port-read p)
    (input-port-scan-token p))
  ;;
  ;;   Make #; scan and ignore the next `read' object,
  ;;   so you can say, e.g.,
  ;;
  ;;      (list '(a b #;(1 2 3) c))     => ((a b c))
  ;;
  (add-sharp-scanner! #\; scan-sharp-comment)
  ;;
  )

