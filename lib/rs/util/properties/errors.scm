(define-message-table rs.util.properties 414)

(define-class <property-not-defined> (<condition>)
  missing-property
  in-object)

(define-method display-object ((self <property-not-defined>) port)
  (write-string
   port
   (fm type: error 
       401 "property `~s' not defined\nin object: ~#s"
       (missing-property self)
       (in-object self))))
