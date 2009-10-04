
(define (resolve-font client data)
  (open-font (on-display client) data))

(define (resolve-color client data)
  (string->color data))

(define (resolve-pixel client data)
  (alloc-color (use-colormap client) data))

(define (resolve-pixels client data)
  (vector-map (lambda (name)
		(get-pixel-resource name client))
	      data))

;;;

(define-constant $font-resource 0)
(define-constant $color-resource 1)
(define-constant $pixel-resource 2)
(define-constant $color-changer-resource 3)
(define-constant $pixel-vec-resource 4)

(define (make-resource-tables)
  (vector (make-symbol-table)
	  (make-symbol-table)
	  (make-symbol-table)
	  (make-symbol-table)
	  (make-symbol-table)))

;;;

(define (get-resource client name resolver type)
  (let ((rtable (vector-ref (resource-tables client) type)))
    (or (table-lookup rtable name)
	(let ((data (table-lookup (resource-settings client) name)))
	  (if data
	      (let ((resolved (resolver client data)))
		(table-insert! rtable name resolved)
		   (dm 190 "resolved: ~s => ~s" name resolved)
		   resolved)
	      (em "~s: no resource `~s'" client name))))))

(define (get-color-resource name #optional (client default: (current-client)))
  (get-resource client name resolve-color $color-resource))

(define (get-pixel-resource name #optional (client default: (current-client)))
  (get-resource client name resolve-pixel $pixel-resource))

(define (get-font-resource name #optional (client default: (current-client)))
  (get-resource client name resolve-font $font-resource))

(define (get-pixels-resource name #optional (client default: (current-client)))
  (get-resource client name resolve-pixels $pixel-vec-resource))
