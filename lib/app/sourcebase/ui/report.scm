
(define-generic-function render-report-line-item)

(define-method render-report-line-item ((self <file-system>))
  (format #t "~10a ~10a ~10a\n"
	  (name self)
	  (name (group self))
	  (name (owner self))))

(define-method render-report-line-item ((self <change-request>))
  (format #t "~-5d ~8a ~a ~10a ~a\n"
	  (id self)
	  (state self)
	  (time->string (open-time self) "%Y-%m-%d")
	  (if (null? (active-items self))
	      ""
	      (let ((ao (unionq (map owner (active-items self)) '())))
		(if (pair? (cdr ao))
		    (string-append (name (car ao)) " +")
		    (name (car ao)))))
	  (title self)))

(define-method default-sort-order ((self <change-request>))
  (lambda (a b) (< (id a) (id b))))

(define-method default-sort-order ((self <user>))
  (lambda (a b) (string<? (name a) (name b))))

(define-method default-sort-order ((self <snapshot>))
  (lambda (a b) 
    (if (eq? (versioned-object a) (versioned-object b))
	(string<? (name a) (name b))
	(string<? (name (versioned-object a)) (name (versioned-object b))))))

(define-method default-sort-order ((self <file-system>))
  (lambda (a b) (string<? (name a) (name b))))

(define-method default-sort-order ((self <work-item>))
  (lambda (a b) (< (id (base-request a)) (id (base-request b)))))

(define-method default-sort-order ((self <group>))
  (lambda (a b) (string<? (name a) (name b))))

(define-method default-sort-order ((self <checkout>))
  (lambda (a b) 
    (if (eq? (user a) (user b))
	(time<? (checkout-time a) (checkout-time b))
	(string<? (name (user a)) (name (user b))))))

(define-method render-report-line-item ((self <group>))
  (format #t "~10a ~10a ~a\n"
	  (name self)
	  (name (owner self))
	  (string-join #\, (map name (parent-groups self)))))

(define-method render-report-line-item ((self <integration-request>))
  (format #t "~-5d ~10a ~8a ~a\n"
	  (id (base-request self))
	  (name (file-system self))
	  (if (pair? (snapshots self))
	      (name (car (snapshots self)))
	      "")
	  (title (base-request self))))

(define-method render-report-line-item ((self <fs-change>))
  (format #t "~-5d ~10a ~a\n"
	  (id (base-request self))
	  (name (file-system self))
	  (title (base-request self))))

(define-method render-report-line-item ((self <checkout>))
  (format #t "~8a ~10a ~a ~-6a ~a\n"
	  (name (user self))
	  (name (file-system self))
	  (time->string (checkout-time self) "%Y-%m-%d %H:%M:%S")
	  (version-tag->string (version-tag (checked-out self)))
	  (let ((p (node->paths (file-system self) 
				(versioned-object (checked-out self)))))
	    (if (pair? p)
		(fs-path->string (car p))
		"[deleted file]"))))

(define-method render-report-line-item ((self <snapshot>))
  (format #t "~8a ~8a ~8a ~a\n"
	  (name (versioned-object self))
	  (name self)
	  (or (snapshot-state self) "")
	  (let ((c (or (assq 'committed (properties self))
		       (assq 'commit-time (properties self)))))
	    (if c
		(time->string (timestamp (cdr c)) "%Y-%m-%d %H:%M:%S")
		""))))

(define-method render-report-line-item ((self <user>))
  (format #t "~12a ~a <~a>\n"
	  (name self)
	  (full-name self)
	  (email-addr self)))

(define-class <file-record> (<object>)
  (filesystem type: <string>)
  (path type: <string>)
  (type type: <string>)
  (version type: <string>))

(define-method to-string ((self <file-record>))
  (path self))

(define-method default-sort-order ((self <file-record>))
  (lambda (a b)
    (let ((c (string-compare (filesystem a) (filesystem b))))
      (if (= c 0)
          (string<? (path a) (path b))
          (< c 0)))))

(define-method render-report-line-item ((self <file-record>))
  (format #t "~8a ~a ~-6a ~a\n"
          (filesystem self)
          (type self)
          (version self)
          (path self)))

