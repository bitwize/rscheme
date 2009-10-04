
(define (make-application db-name)
  (make <application>
	user-table: (make-table string=? string->hash)
	group-table: (make-table string=? string->hash)
	property-table: (make-table string=? string->hash)
	world-group: #f
	name: db-name
	structure-version: $structural-version
	application-version: $application-version
	creation-time: (time)
	domain-table: (make-symbol-table)
	change-request-table: (make-table eq? integer->hash)
	file-system-table: (make-table string=? string->hash)))
