(define-module rs.io.textport (unquote)
  ,(use usual-inlines)
  ,(load "location.scm")
  ,(load "textin.scm")
  ,(export <text-input-port> make-text-input-port
	   location start-location previous-location
           location-line-number
           location-column-number
           location-port-owner
           <text-location> location+ location-))
