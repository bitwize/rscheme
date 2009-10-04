
(define-class <debug-state> (<object>)
  dynamic-state
  chain)

(define (continuation-chain (self <partial-continuation>))
  (let loop ((pc self)
             (r '()))
    (if (partial-continuation? pc)
        (loop (pc-continuation-reg pc) (cons pc r))
	(reverse! r))))

(define (capture-state #optional (skip default: 0))
  (low-level-call/cc
   (lambda (ll-continuation)
     (make <debug-state>
           dynamic-state: (get-dynamic-state-reg)
           chain: (list-tail
                   (continuation-chain
                    (ll->partial ll-continuation))
                   (+ skip 2))))))
