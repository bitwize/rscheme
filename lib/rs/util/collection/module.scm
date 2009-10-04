
(define-module rs.util.collection ()
  (&module
   (import usual-inlines tables)
   ;
   (load "sequences.scm")
   ;
   (export sequence-length 
	   sequence->vector
	   sequence->list
	   sequence-as)
   ;
   (load "iterate.scm")
   (export initial-state
	   next-state
	   current-element
	   previous-state)
   ;
   (load "map.scm")
   (export map)
   ;
   (load "foreach.scm")
   (export for-each)
   ;
   (load "every-and-any.scm")
   (export every? any?)))
