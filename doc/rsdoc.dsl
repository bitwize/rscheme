<!DOCTYPE style-sheet PUBLIC "-//James Clark//DTD DSSSL Style Sheet//EN" [
<!ENTITY % html "IGNORE">
<![%html;[
<!ENTITY % print "IGNORE">
<!ENTITY docbook.dsl PUBLIC "-//Norman Walsh//DOCUMENT DocBook HTML Stylesheet//EN" CDATA dsssl>
]]>
<!ENTITY % print "INCLUDE">
<![%print;[
<!ENTITY docbook.dsl PUBLIC "-//Norman Walsh//DOCUMENT DocBook Print Stylesheet//EN" CDATA dsssl>
]]>
]>

<style-sheet>

<!--
******************************************************************************
		   Print (TeX) specific procedures
******************************************************************************
-->

<style-specification id="print" use="docbook">
<style-specification-body>

(define %left-margin% 1in)
(define %right-margin% 1in)

;;; Sedgewick Algorithms page dimen: 6.25 wide x  9.25 high
;;;
;;;  Chapter/Appendix Titling
;;;

(define ($comptitle$)
  (cond 
   ((have-ancestor? "CHAPTER")
    (gen-component-title 'chapter))
   ((have-ancestor? "APPENDIX")
    (gen-component-title 'appendix))
   (else
    "")))

(define (gen-component-title chap-type)
  (make sequence
	(make paragraph
	      font-family-name: %title-font-family%
	      font-weight: 'bold
	      font-size: (HSIZE 4)
	      line-spacing: (* (HSIZE 4) %line-spacing-factor%)
	      space-before: (* (HSIZE 4) %head-before-factor%)
	      space-after: (* (HSIZE 4) %head-after-factor%)
	      start-indent: 0pt
	      first-line-start-indent: 0pt
	      quadding: 'start
	      keep-with-next?: #t
	      (make line-field
		    field-width: %body-start-indent%
		    font-size: (HSIZE 6)
		    (literal (chap-app-id chap-type)))
	      ;(inline-space 1in) ; context needs a sosofos...?
	      (process-children-trim))
	(make rule ; p.239
	      length: 6in
	      orientation: 'horizontal
	      color: (gray-color 0.5)
	      line-thickness: 3pt)))



(define (singleton-node-list? nl)
  (and (node-list? nl)
       (not (node-list-empty? nl))
       (node-list-empty? (node-list-rest nl))))

(define (chap-app-id chap-type)
  (let* ((anc (ancestor (symbol->string chap-type)))
	 (label (attribute-string "label" anc)))
    (if (and label 
	     (not (equal? label "auto")))
	label
	(format-number (element-number anc)
		       (if (equal? chap-type 'chapter)
			   "1"
			   "A")))))

;;; looking at the sources, it seems that Jade implements
;;; only `::Device RGB'

(define *rgb*
  (color-space "ISO/IEC 10179:1996//Color-Space Family::Device RGB"))

(define (gray-color k)
  (color *rgb* k k k))

;;;
;;;  RefEntry ("man page") formatting
;;;

(element SYNOPSIS 
  (make sequence
	(process-children)
	(make paragraph-break)))

(element FUNCSYNOPSIS
  (if (equal? (attribute-string "role") "Scheme")
      (scheme-func-synopsis)
      (make sequence
	    (process-children))))

(define (scheme-func-synopsis)
  (make sequence
	font-family-name: %mono-font-family%
	(with-mode scheme (process-children))))

(mode scheme
  ; outer construct
  (element FUNCPROTOTYPE
    (make paragraph
	  font-family-name: %mono-font-family%
	  (literal "(")
	  (process-matching-children '(FuncDef))
	  (process-matching-children '(ParamDef (role arguments)))
	  (literal ")")
	  (literal " => ")
	  (process-matching-children '(ParamDef (role returns)))))
	  ;(make formatting-instruction data: "\\doublerightarrow")
  ; inside a FuncPrototype
  (element FUNCDEF
    (make sequence
	  font-family-name: %mono-font-family%
	  (process-children)))
  ; perhaps an argument within a FuncDef, or maybe ReturnValue
  (element REPLACEABLE
    ($italic-pro-seq$)))

(define ($italic-pro-seq$) ; pro=proportional spaced font
  (make sequence
	font-family-name: %body-font-family%
	font-posture: 'italic
	(process-children-trim)))

(element REPLACEABLE ($italic-pro-seq$))
(element PARAMETER ($italic-pro-seq$))

(element REFNAME
  (if (first-sibling?)
      (refname-seq)
      (make sequence
	    (literal ", ")
	    (refname-seq))))

(define (refname-seq)
  (make sequence
	font-weight: 'medium
	font-family-name: %mono-font-family%
	(process-children)))

(element REFNAMEDIV
  (make paragraph
	use: default-text-style
	space-before: %para-sep%
	start-indent: %body-start-indent%
	quadding: 'start
	(make sequence
	      (outdented-title1 "Name")
	      (make sequence
		    (process-matching-children 'refname)
		    (literal "")
		    (process-matching-children 'refpurpose)))))
	
(element REFSYNOPSISDIV
  (make paragraph
	use: default-text-style
	space-before: %para-sep%
	start-indent: %body-start-indent%
	quadding: 'start
	(make sequence
	      (outdented-title1 "Synopsis")
	      (make paragraph
		    use: default-text-style
		    space-before: %para-sep%
		    start-indent: %body-start-indent%
		    lines: 'asis
		    font-family-name: %mono-font-family%
		    (process-children)))))

(element (REFSECT1 TITLE)
  (outdented-title1))

(element (REFSECT2 TITLE)
  (outdented-title2))

(element (REFSECT2 VARIABLELIST)
  (make display-group
	(process-children)))

(element (REFSECT2 VARIABLELIST VARLISTENTRY)
  (make paragraph
	(process-children)))

(element (REFSECT2 VARIABLELIST VARLISTENTRY TERM)
  (let ((termlength
	 (attribute-string "termlength" 
			   (ancestor (normalize "variablelist")))))
    (make paragraph
	  use: default-text-style
	  end-indent: (if termlength
			  (- %text-width% (measurement-to-length termlength))
			  0pt)
	  (process-children-trim))))

(define (outdented-title2 #!optional str)
  (make paragraph
	font-family-name: %title-font-family%
	font-weight: 'bold
	font-size: 6pt
	start-indent: (- %body-start-indent% 12pt)
	quadding: 'start
	keep-with-next?: #t
	(if str
	    (literal str)
	    (process-children-trim))))

(define (outdented-title1 #!optional str)
  (make paragraph
	font-family-name: %title-font-family%
	font-weight: 'bold
	font-size: 9pt
	start-indent: (- %body-start-indent% 12pt)
	quadding: 'start
	keep-with-next?: #t
	(if str
	    (literal str)
	    (process-children-trim))))

;;;

(element EXAMPLE (gen-example))
(element INFORMALEXAMPLE (gen-example))

(define (gen-example)
  (with-mode example 
    (make display-group
	  space-before: 0pt
	  space-after: 0pt
	  start-indent: %body-start-indent%
	  (process-children))))

(define debug
  (external-procedure "UNREGISTERED::James Clark//Procedure::debug"))


(define (example-table-cell sh in-thunk)
  (make paragraph
	start-indent: sh
	end-indent: 0pt
	(in-thunk)))

(define (example-table-row side in-thunk)
  (case side
    ((right) (example-table-cell (+ %body-start-indent% 18pt) in-thunk))
    ((left) (example-table-cell %body-start-indent% in-thunk))))

(mode example
      ;; some additional cleverness is called for to make the first
      ;; right-hand-side cell be on the same row as the left-hand-side
      ;; cell.  Probably can use (first-sibling?) to do it...  Might
      ;; be able to use table-cell's starts-row?/ends-row? too...
  (element Para
    (make sequence
	  (process-matching-children 'UserInput)
	  (process-matching-children 'Phrase 'ComputerOutput)))
  (element ComputerOutput
    (example-table-row 
     'right
     (lambda ()
       (make sequence
	     (literal " => ")
	     ($mono-seq$)))))
  (element Phrase
    (example-table-row 
     'right
     (lambda ()
       (make sequence
	     (literal " => ")
	     ($italic-seq$)))))
  (element UserInput
    (example-table-row
     'left
     (lambda ()
       ($verbatim-display$ #f #f)))))



</style-specification-body>
</style-specification>

<!--
******************************************************************************
		   Online (HTML) specific procedures
******************************************************************************
-->

<style-specification id="html" use="docbook">
<style-specification-body> 

(define %html-ext% ".html")
(define %root-filename% "Welcome")
(define html-index #t)
(define html-index-filename "html.index")

</style-specification-body>
</style-specification>

<external-specification id="docbook" document="docbook.dsl">
</style-sheet>
