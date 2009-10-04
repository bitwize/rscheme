(define-module util.xml ()
  ;; this is a higher-level wrapper around util.sxml
  (&module
   (implements rscheme-native-xml)
   (import usual-inlines
           rs.io.pushback
           regex))

  (&module
   (load "read.scm")
   (export string->sxml
           string->sxml-sequence
           port->sxml
           with-xml-application-entity-ref)

   (load "write.scm")
   (export sxml->string
           write-sxml)

   (load "dom.scm")
   (export sxml->dom            ; turn a SXML tree into a document node
           ;;
           ;;  The `dom-axis:' procedures return a node list
           ;;
           dom-axis:child
           dom-axis:parent
           dom-axis:parent1
           dom-axis:self
           dom-axis:attribute
           dom-axis:ancestor
           dom-axis:ancestor-or-self
           dom-axis:descendant
           dom-axis:descendant-or-self
           dom-axis:preceding-sibling
           dom-axis:following-sibling
           dom-axis:preceding
           dom-axis:following
           dom-axis:namespace
           ;;
           dom:attribute?
           dom:element?
           dom:comment?
           )
   
   (load "xpath.scm")
   (export xpath:eval
           xpath:node-set->string
           xpath:node->string

           sxml:document?
           sxml:comment?
           sxml:text?
           sxml:pi?
           sxml:entityref?
           sxml:element?
           sxml:children
           sxml:root-element    ; strip off top '*TOP* level
           sxml:attributes)
   ;;
   (load "pretty.scm")
   (export sxml:strip-whitespace
           pretty-printify-xml)))

