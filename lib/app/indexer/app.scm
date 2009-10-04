
;;;

(define-class <document-index> (<object>)
  (properties type: <vector> init-value: '#())
  (archive-index type: <string-table> init-function: make-string-table)
  (id-index type: <hash-integer-table> init-function: make-fixnum-table)
  (indices type: <vector> init-value: '#())
  (next-id type: <fixnum> init-value: 1))

(define-method lookup ((self <document-index>) index)
  (table-lookup (id-index self) index))

(define-method get-index-section ((self <document-index>) name)
  (let ((i (vassq name (indices self))))
    (if i
	(vector-ref (indices self) i)
	(error "get-index-section: no section: ~s" name))))

(define-class <open-index> (<object>)
  (underlying-store init-value: #f)
  (doc-index type: <document-index>)
  (alloc-area type: <allocation-area>)
  (read-only? type: <boolean> init-value: #f))

(define-class <keyword-index> (<object>)
  (name type: <symbol>)
  (constituents type: <table>))
			       
;;;

(define (access-document-index #key file)
  (let ((ps (read-persistent-store file)))
    (setup-indirects ps)
    (make <open-index>
      underlying-store: ps
      read-only?: #t
      alloc-area: (default-allocation-area ps)
      doc-index: (root-object ps))))

(define (open-document-index #key file)
  (let ((ps (open-persistent-store file)))
    (setup-indirects ps)
    (make <open-index>
      underlying-store: ps
      alloc-area: (default-allocation-area ps)
      doc-index: (root-object ps))))

(define (add-index-section (self <document-index>) name)
  (let ((ki (make <keyword-index>
	      name: name
	      constituents: (make-string-table))))
    (set-indices! self (vector-append (indices self)
				      (vector name ki)))
    ki))

(define (make-document-index #key (file default: #f))
  (let ((di (make <document-index>)))
    ;;
    (for-each (lambda (sect)
		(add-index-section di sect))
	      '(body message-id
		subject from to date domain
		in-reply-to references))
    ;;
    (if file
	(let ((ps (create-persistent-store file)))
	  (setup-indirects ps)
	  (commit ps di)
	  (make <open-index>
	    underlying-store: ps
	    alloc-area: (default-allocation-area ps)
	    doc-index: (root-object ps)))
	(make <open-index>
	  alloc-area: *default-allocation-area*
	  doc-index: di))))

;;;

(define $null-doc-index (make <document-index>
			  id-index: (make-fixnum-table)
			  indices: '#()))

(define (close-document-index (self <open-index>))
  (if (underlying-store self)
      (let ((h (if (read-only? self)
		   #f
		   (commit (underlying-store self)))))
	(close-persistent-store (underlying-store self))
	(set-underlying-store! self #f)
	(set-alloc-area! self *default-allocation-area*)
	(set-doc-index! self $null-doc-index)
	(if h h (values)))
      (error "~s: already closed" self)))

