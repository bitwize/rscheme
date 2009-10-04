
(define (string->keyword str)
  (symbol->keyword (string->symbol str)))

(define (string->flag str)
  (symbol->flag (string->symbol str)))

(define-constant <keyword> <symbol>)
(define-constant <flag> <symbol>)

