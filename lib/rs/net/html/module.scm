(define-module rs.net.html (unquote)
  (unquote
   (import usual-inlines)
   (import rs.util.properties)
   (load "htmlport.scm")
   (load "html.scm")
   (load "format.scm")
  ;
  (export with-output-to-html-string
          output-port-control
          ;; `(html foo)' executes foo with output to an HTML port
          html
          escape-html)

  ;; html structure
  (export bold
	  emph
	  code
	  html-font
	   header-1
	   header-2
	   header-3
	   header-4
	   horz-rule
	   html-body
	   html-header
	   hyperlink
	   input-field
	   input-area
	   input-submit
	   input-reset
	   input-form
	   input-select
	   input-text
	   input-hidden
	   list-item
	   nl
	   numbered-list
	   par
	   preformatted
	   select-option
	   title
	   unnumbered-list
	   html-table
	   html-table-data
	   html-table-row
	   with-tag)
  ;
  (load "fromparsed.scm")
  (export display-parsed-html)))

