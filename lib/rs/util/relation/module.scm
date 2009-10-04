(define-module rs.util.relation ()
  (&module
   (import usual-inlines
           tables
           objsys
           sort
           rs.util.properties)
   ;;
   (load "world.scm")
   (export <universe>
           current-universe
           make-universe
           with-universe)
    ;;
   (load "relation.scm")
   (export define-relation
           constraint-check-establish
           establish
           retract
           <<relation>>
           <relation-row>
           <relation-participant>
           extent
           extent->list)
   (export ;; errors...
    <relation-already-established>
    <relation-no-extent>)
   ;;
   (load "index.scm")
   (export make-index 
           define-index
           initialize-indices)
   ;;
   (load "query.scm")
   ;;
   (export define-queries query)))
    
