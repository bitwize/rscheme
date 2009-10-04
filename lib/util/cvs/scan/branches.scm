;;;
;;;  Comprehend CVS branches
;;;

;;;  See the picture from the CVS docs on `Branches and revisions' for
;;;  a good visualization of what's going on.

;;;
;;;  `revision-sequence'
;;;
;;;  Compute the sequence of revision numbers to get from
;;;  one point to another on a revision tree (including
;;;  branches).  This procedure operates on the abstract,
;;;  infinitely-populated tree.  This works because we can
;;;  start at the "leaf" and work our way back to the ancestor.
;;;

(define (revision-sequence from inclusive-from? to inclusive-to?)
  (let ((goal (if from (string->revision-tuple from) #f))
        (start (string->revision-tuple to)))
    (let loop ((lst (if inclusive-to?
                        (list (revision-tuple->string start))
                        '()))
               (at start))
      (if (equal? at goal)
          (if inclusive-from? 
              lst
              (cdr lst))
          (let ((p (pred at)))
            (if p
                (loop (cons (revision-tuple->string p) lst) p)
                (if (eq? goal #f)
                    (if inclusive-from? 
                        lst
                        (cdr lst))
                    #f)))))))

(define (revision-tuple->string tup)
  (string-join "." (map number->string (reverse tup))))

(define (string->revision-tuple str)
  (reverse (map string->number (string-split str "."))))

(define (previous-revision r)
  (revision-tuple->string (pred (string->revision-tuple r))))

(define (pred tup)
  ;; compute the predecessor revision from a rev tuple
  ;; (a rev tuple is the backwards list of rev components,
  ;;  e.g., for "1.2.2.3", the rev tuple is (3 2 2 1))
  (let ((x (car tup)))
    (if (= x 1)
        (let ((p (cddr tup)))
          (if (null? p)
              #f
              p))
        (cons (- x 1) (cdr tup)))))


(define (chop-magic-zero rev)
  (let ((t (string->revision-tuple rev)))
    (if (and (> (length t) 2)
             (= (cadr t) 0))
        (revision-tuple->string (cddr t))
        rev)))

(define (branch-magic-revision? rev)
  (not (eq? rev (chop-magic-zero rev))))

(define (on-branch-recognizer branch)
  (let* ((b (string->revision-tuple branch))
         (check (if (branch-magic-revision? branch)
                    (cons (car b) (cddr b))
                    (cdr b))))
    (lambda (rev)
      (let ((r (string->revision-tuple rev)))
        (if (equal? check (cdr r))
            (car r)
            #f)))))
