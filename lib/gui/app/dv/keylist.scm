;;Here's a script to print out the key bindings:

,(use rs.sys.tables sort)

(define (key<? ks1 ks2)
  (string<? (vector-ref ks1 0) (vector-ref ks2 0)))

(define (build-keyseq-map keymap)
  (let ((q (make-dequeue)))
    (build-keyseq-map* keymap q '() (list keymap))
    (dequeue-state q)))

(define (build-keyseq-map* keymap q prefix-rev seen)
  (table-for-each
   keymap
   (lambda (h k v)
     (let ((this-keyseq (cons (render-key k) prefix-rev)))
       ;(format #t "~s ==> ~s\n" this-keyseq v)
       (if (table? v)
	   (if (memq v seen)
	       (format #t "** Skipping ~s\n" this-keyseq)
	       (build-keyseq-map* v q this-keyseq (cons v seen)))
	   (dequeue-push-back! q (vector
				  (string-join #\space (reverse this-keyseq))
				  this-keyseq
				  v)))))))

(define-method render-key ((self <symbol>))
  (to-string self))

(define-method render-key ((self <char>))
  (substring (format #f "~s" self) 2))

,(use rs.util.text)

(define (print-keymap self)
  (let ((keys (sort (build-keyseq-map self) key<?)))
    (display-table
     '("Key Seq" "Binding")
     (map
      (lambda (ks)
	(list (vector-ref ks 0)
	      (to-string (name (vector-ref ks 2)))))
      (vector->list keys)))))


