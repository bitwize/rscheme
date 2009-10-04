
(define (create-database (app <application>) (path <string>))
  (setup-pstore (create-persistent-store path) app))
