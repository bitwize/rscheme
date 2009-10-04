;;
;;  <property> and <property-value> provide
;;  a useful interface for common extensions.  Most system
;;  properties do not use <property> objects, but instead
;;  have <symbol> keys and special code for value interpretation
;;
;;  instances of <property> can occur as the key in various
;;  sorts of object property lists
;;

(define-class <property> (<object>)
    (name type: <string>)
    (description type: <string>)
    property-value-type
    (property-index init-value: #f)
    (default-value init-value: #f))

(define-method write-object ((self <property>) port)
  (format port "#[<property> ~a]" (name self)))

(define-method display-object ((self <property>) port)
  (display (name self) port))

;;
;; valid property value types are:
;;
;;   <integer>
;;   <number>
;;   <string>
;;   <symbol>
;;   <user>
;;   <group>
;;   <change-request>
;;   <file-system>
;;
;;   (enum ?value...)
;;   (list ?type)
;;   (or ?type...)
;;

;;
;;  used for enumerated property values instead
;;  of symbols
;;

(define-class <property-value> (<object>)
    (owner type: <property>)
    (name type: <string>)
    (description type: <string>)
    (info init-value: #f))

(define-method write-object ((self <property-value>) port)
  (format port "#[<property-value> ~a=>~a]"
	  (name (owner self))
	  (name self)))

(define-method display-object ((self <property-value>) port)
  (display (name self) port))

;;
;;  used to convert other kinds of property values
;;  (like <symbol>'s and <number>'s) to strings
;;

(define-generic-function property-value-string-form)
