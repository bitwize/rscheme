
(define (column-heights num-elem num-cols)
  (let* ((all (quotient num-elem num-cols))
         (overflow (- num-elem (* num-cols all))))
    ;(format #t "all ~d, overflow ~d\n" all overflow)
    (append (map (lambda (k) (+ all 1)) (range overflow))
            (map (lambda (k) all) (range (- num-cols overflow))))))

(define (column-starts data heights)
  (let loop ((d data)
             (r '())
             (h heights))
    (if (null? h)
        (reverse r)
        (loop (list-tail d (car h))
              (cons d r)
              (cdr h)))))

(define (column-out port data screenw)
  (let* ((n (length data))
         (colw (max 10 (+ 1 (apply max (map string-length data)))))
         (num-cols (quotient (- screenw 2) colw))
         (hts (column-heights n num-cols))
         (pad (vector-map (lambda (n)
                            (make-string (- colw n) #\space))
                          (list->vector (range (+ colw 1))))))
    ;(format #t "Heights => ~s\n" hts)
    (let loop ((cols (column-starts data hts))
               (i (car hts))
               (hts hts))
      (for-each
       (lambda (p h)
         (let* ((s (if (> h 0) (car p) "")))
           (write-string port s)
           (write-string port (vector-ref pad (string-length s)))))
       cols
       hts)
      (write-string port "\n")
      (if (> i 1)
          (loop (map cdr cols)
                (- i 1) 
                (map (lambda (n) (- n 1)) hts))))))

#|
(define (t)
  (column-out (current-output-port)
              '("fold" "for" "forall" "format"
                       "foxy" "foblsdf"
                       "foblefs" "fixv")
              80))
|#

#|  
(define (column-out c)
  (let ((s (sort c string<?)))
    (if (<= (length s) 4)
        (begin
          (for-each (lambda (word)
                      (write-string out " ~-19a" word))
                    s)
          (write-string out "\n"))
        (let ((t (map (lambda (n)
                        (list-tail s n))
                      
|#