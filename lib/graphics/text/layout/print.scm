(define-method print ((self <para-context>))
  (let ((pvec (list->vector (passive self)))
        (h (make-object-table)))
    ;;
    (format #t ",~a Properties ~a\n" 
            (make-string 29 #\-)
            (make-string 29 #\-))
    (format #t "|      threshold : ~s\n" (threshold self))
    (format #t "|      active-width : ~s\n" (active-width self))
    (for-each (lambda (i)
                (if (even? i)
                    (format #t "      ~s : ~s\n" 
                            (vector-ref (properties self) i)
                            (vector-ref (properties self) (+ i 1)))))
              (range (vector-length (properties self))))
    (format #t "+----- Horizontal List -----\n")
    (for-each
     (lambda (i a)
       (format #t "|   ~-4d. " i)
       (print-hnode a (current-output-port))
       (table-insert! h a i)
       (newline))
     (range (length (hlist self)))
     (hlist self))
    ;;
    (format #t "+----- Active List -----\n")
    ;;
    (for-each
     (lambda (i a)
       (format #t "|   ~-4d. (~a) " i (machine-bits->string a))
       (print-anode a (current-output-port) pvec)
       (newline))
     (range (length (active self)))
     (active self))
    ;;
    (format #t "+----- Passive List -----\n")
    ;;
    (for-each
     (lambda (i p)
       (format #t "|   ~-4d. @ ~s   next=> ~a\n" 
               i
               (if (null? (break p))
                   (length (hlist self))
                   (table-lookup h (car (break p))))
               (if (prev-break p)
                   (~ "#~s" (vmemq (prev-break p) pvec))
                   "(none)")))
     (range (length (passive self)))
     (passive self))
    ;;
    (format #t "`~a\n" (make-string 70 #\-))
    ;;
    self))

(define-method print-hnode ((self <glue-node>) port)
  (format port "         GLUE ~a" (to-string (content self))))

(define-method print-hnode ((self <kern-node>) port)
  (format port "         KERN ~d" (width self)))

(define-method print-hnode ((self <penalty-node>) port)
  (format port "         PENALTY ~d" (penalty self)))

(define-method print-hnode ((self <ligature-node>) port)
  (format port " <~-5d> LIGATURE ~10s  (~a-~a-~a)"
          (width self)
          (expanded self)
          (font-family (font self))
          (font-style (font self))
          (font-size (font self))))

(define-method print-hnode ((self <char-node>) port)
  (format port " <~-5d> CHAR ~10s  (~a-~a-~a)"
          (width self)
          (content self)
          (font-family (font self))
          (font-style (font self))
          (font-size (font self))))

(define-method print-anode ((self <active-node>) port pv)
  (format port "Active ")
  (format port "fit=~d  type=~a  demerits=~d l=~2d"
          (fitness self)
          (case (type self)
            ((hyphenated) "hyph")
            ((unhyphenated) "unhyph"))
          (total-demerits self)
          (line-number-after self))
  (if (passive self)
      (format port "  passive=[~s]" (vmemq (passive self) pv))))

(define-method print-anode ((self <delta-node>) port pv)
  (format port "Delta ~s" (delta self)))

