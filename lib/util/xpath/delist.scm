
(define-class <delist> (<object>)
  (head type: <pair> :sealed)
  (tail type: <pair> :sealed))

(define (delist-append a b)
  (if (null? a)
      b
      (if (null? b)
          a
          (let (((a <delist>) a)
                ((b <delist>) b))
            (set-cdr! (tail a) (head b))
            (make-gvec <delist> (head a) (tail b))))))

(define (delist . items)
  (if (null? items)
      '()
      (make <delist>
            head: items
            tail: (last-pair items))))


(define (list->delist items)
  (if (null? items)
      '()
      (make <delist>
            head: items
            tail: (last-pair items))))

(define-method write-object ((self <delist>) port)
  (format port "#[<delist> ~s]" (head self)))
