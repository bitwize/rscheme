
;; this is stored in the key-state property of
;; windows that eat keystrokes

(define-class <key-state> (<object>)
  initial-keymap
  (current-keymap init-value: #f)
  owner)

(define (key-state-process-key (s <key-state>) ch state)
  (if (not (current-keymap s))
      (set-current-keymap! s (initial-keymap s)))
  (let ((ent (lookup-key-in-keystate s ch)))
    (cond
     ((procedure? ent)
      ; it was an interactive procedure to be invoked
      (call-interactive ent (owner s) (cons ch state)))
     ((pair? ent)
      ; it was a list of (new) keymaps
      (set-current-keymap! s ent))
     (else
      ; otherwise, reset the key state back to the beginning
      (set-current-keymap! s #f)))))

(define (lookup-key-in-keystate (s <key-state>) ch)
  (let loop ((lst (current-keymap s))
	     (r '()))
    (if (null? lst)
	(reverse r)
	(let ((e (table-lookup (car lst) ch)))
	  (if e
	      (if (procedure? e)
		  (if (null? r)
		      ;; only return the procedure if there wasn't
		      ;; an earlier-bound indirect keymap
		      e
		      ;; otherwise, ignore the procedure entry
		      (begin
			(format (current-error-port)
				"binding for key `~s' is inconsistent"
				ch)
			(loop (cdr lst) r)))
		  (loop (cdr lst) (cons e r)))
	      (loop (cdr lst) r))))))

;;;

(define (keymap-set-key keymap key-spec proc)
  (let loop ((km keymap)
	     (ks (if (pair? key-spec)
		     key-spec
		     (list key-spec))))
    (if (pair? (cdr ks))
	(let ((subseq (table-lookup km (car ks))))
	  (if subseq
	      (if (instance? subseq <table>)
		  (loop subseq (cdr ks))
		  (error "invalid key prefix: ~s" key-spec))
	      (let ((subseq (make-keymap)))
		(table-insert! km (car ks) subseq)
		(loop subseq (cdr ks)))))
	(if (instance? (table-lookup km (car ks)) <table>)
	    (error "attempt to override key prefix: ~s" key-spec)
	    (table-insert! km (car ks) proc)))))

(define (make-keymap)
  (make-table eq? hash-code))

;;;

(define *global-keymap* (make-keymap))
(define *graphic-keymap* (make-keymap))
(define *text-keymap* (make-keymap))

(define (global-set-key key-spec proc)
  (keymap-set-key *global-keymap* key-spec proc))

(define (graphic-set-key key-spec proc)
  (keymap-set-key *graphic-keymap* key-spec proc))

(define (textual-set-key key-spec proc)
  (keymap-set-key *text-keymap* key-spec proc))

#|
;;;
;;;  some code to build key binding documentation
;;;

,(use rs.sys.tables sort)

(define (key<? ks1 ks2)
  (string<? (vector-ref ks1 0) (vector-ref ks2 0)))

(define (build-keyseq-map keymap)
  (let ((q (make-dequeue)))
    (build-keyseq-map* keymap q '())
    (dequeue-state q)))

(define (build-keyseq-map* keymap q prefix-rev)
  (table-for-each
   keymap
   (lambda (h k v)
     (let ((this-keyseq (cons (render-key k) prefix-rev)))
       (if (table? v)
	   (build-keyseq-map* keymap v this-keyseq)
	   (dequeue-push-back! q (vector
				  (apply string-append (reverse this-keyseq))
				  this-keyseq
				  v)))))))

(define-method render-key ((self <symbol>))
  (to-string self))

(define-method render-key ((self <char>))
  (substring (format #f "~s" self) 2))

,(use rs.util.text)

(define (print-keymap self)
  (let ((keys (sort (build-keyseq-map self) key<?)))
    (print-table
     '("Key Seq" "Binding")
     (map
      (lambda (ks)
	(list (vector-ref ks 0)
	      (to-string (vector-ref ks 2))))
      keys))))

|#
