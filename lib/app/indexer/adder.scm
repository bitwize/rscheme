
(define (assign-id (self <open-index>))
  (let (((i <fixnum>) (next-id (doc-index self))))
    (set-next-id! (doc-index self) (add1 i))
    i))

(define (email-constructor id (content <string>) aa)
  (make <inline-email-message>
    %alloc-area: aa
    id: id
    content: (area-clone aa <string> content)))

(define (add-document (self <open-index>) (doc <document>))
  (let ((id (assign-id self)))
    (set-id! doc id)
    (table-insert! (id-index (doc-index self)) id doc)
    doc))

(define (add-to-section (self <open-index>) 
			(doc <document>)
			(section <symbol>)
			(words <list>))
  ;(dm 101 "adding ~d words to ~s: ~#@*43j" (length words) section words)
  (let* ((ki (get-index-section (doc-index self) section))
	 (i (id doc))
	 (tbl (constituents ki)))
    (for-each
     (lambda (word)
       (let ((o (table-lookup tbl word)))
         ;(format #t "Adding ~s to ~#*@40s\n" word o)
         (let ((r (append-member! o i)))
           (if r
               (table-insert! tbl word r))))) ;; insert replacement value)
     words)))

;;;

(define-method append-member! ((self <boolean>) i)
  i)

(define-method append-member! ((self <fixnum>) i)
  (if (eq? self i)
      (values) ;; already in set
      (cons self i)))

(define-method append-member! ((self <pair>) i)
  (if (or (eq? i (car self))
	  (eq? i (cdr self)))
      (values) ; already in set
      (let ((s (make <bit-set>)))
	(add-member! s (car self))
	   (add-member! s (cdr self))
	   (add-member! s i)
	   s)))

(define-method append-member! ((self <bit-set>) i)
  (add-member! self i)
  ;; don't reinsert ourself
  (values))
