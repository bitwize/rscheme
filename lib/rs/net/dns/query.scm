
(define-class <query> (<object>) name type class)

(def (make-query name type class)
  (make <query>
    name:  name
    type:  type
    class: class))

(define-method write-object ((self <query>) port)
  (format port "#[<query> ~s ~s ~s]"
	  (name self)
	  (type self)
	  (class self)))

