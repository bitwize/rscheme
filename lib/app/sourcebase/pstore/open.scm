(define (open-database (path <string>))
  (setup-pstore (open-persistent-store path) 'read-write))

(define (read-database (path <string>))
  (setup-pstore (open-persistent-store path) 'read-only))

(define (revert-database (path <string>) generation)
  (let ((l (lss-open path generation)))
    (setup-pstore (open-pstore-on-lss l) 'read-write)))

(define (read-old-database (path <string>) v)
  (setup-pstore 
    (make <persistent-store>
	  path: path
	  mode: 'read
	  commit-record: v)
    'read-only))

#|
  (let ((ps ))
    (setup-indirect-page ps 1 $app-classes)
    (set! *access-mode* 'read-write)
    (set! *application* (root-object ps))
    *application*))

(define (read-database (path <string>))
  (let ((ps (open-persistent-store path)))
    (setup-indirect-page ps 1 $app-classes)
    (set! *access-mode* 'read-only)
    (set! *application* (root-object ps))
    *application*))

|#
