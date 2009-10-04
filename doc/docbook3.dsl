<!doctype style-sheet PUBLIC "-//James Clark//DTD DSSSL Style Sheet//EN">

;; Version 0.70	1997.06.19

;; Changes from db069.dsl by Norman Walsh (norm@berkshire.net)
;;   Added rudimentary support for book title pages
;;     BOOKINFO is used to produce the title page
;;     On the recto side:
;;        TITLE
;;        AUTHOR or AUTHORGROUP
;;          AUTHOR names and AFFILIATIONs
;;     On the verso side:
;;        All the children of BOOKINFO; the code here supports
;;        TITLE, AUTHORGROUP, COPYRIGHT, LEGALNOTICE and anything else
;;        in the stylesheet, of course.
;;   Added %generate-titlepage% to toggle title page creation
;;   Added %generate-toc% to toggle automatic TOC generation
;;   Added function author-string to return the contents of an AUTHOR
;;     element as a formatted string
;;   Added function author-list-string to return the contents of an AUTHOR
;;     element as a formatted string in a list of authors in an AUTHORGROUP
;;     (adds appropriate punctuation around (author-string))
;;   The following elements are no longer (empty-sosofo):
;;      BOOKINFO, AUTHOR, AUTHOGROUP, COPYRIGHT, YEAR, HOLDER, LEGALNOTICE
;;      ORGNAME, HONORIFIC, FIRSTNAME, OTHERNAME, SURNAME, LINEAGE
;;   Redefined handling of BOOK to better support title pages and TOCs
;;      (individually or in combination)
;;   Added the (C) copyright symbol to the list of accessible bullets.

;; Changes from db068b.dsl by Norman Walsh (norm@berkshire.net)
;;   made several of the link elements $charseq$ not empty-sosofo
;;      (even if we don't know what to do with the link, we keep the text)
;;   added $linespecific-display$, like $verbatim-display$ but doesn't switch
;;      to monospace font.  Made LITERALLAYOUT $linespecific-display.
;;      added ADDRESS as $linespecific-display$
;;   added support for simplelist type=inline
;;   added %refentry-generate-name% to control output of NAME in RefEntrys
;;   added %refentry-function% to control output of () in RefEntrys
;;   integrated automatic TOC generation (supplied by Jon) into Book

;; Changes from 0.63 (last public release) by Jon Bosak:
;;   all "start-indent: (inherited-start-indent)" removed
;;   n-rows spanned fixed for DocBook 3.0
;;   PART moved from component to division
;;   entityref handling fixed in graphics

;; ######################################################################
;;
;; DSSSL style sheet for DocBook 2.x  and 3.x print output
;;
;; Jon Bosak, Sun Microsystems
;; CALS table formatting by Anders Berglund, EBT
;; Recent additions by Norm Walsh (see notes above)
;;
;; Other contributors: Tony Graham, Terry Allen, and of course
;;   James Clark
;;
;; To do:
;;   platform-independent way to invoke bullet chars etc.
;;   footnotes
;;   refmiscinfo, refdescriptor
;;   toc, lot
;;   segmentedlist, segtitle, seglistitem, seg
;;   calloutlist, callout
;;   screenshot, screeninfo, areaspec, area, areaset
;;   programlistingco, screenco, graphicco
;;   equation, informalequation, inlineequation
;;   syntax definition stuff
;;   index stuff
;;   links and anchors
;;   divide style sheet into modules
;;
;; ######################################################################


;; =========================== PARAMETERS ==============================

;; Refentry generate name indicates whether or not the word "NAME "
;; should be generated at the start of a REFNAME.
(define %refentry-generate-name% #t)

;; Refentry function indicates whether or not REFENTRYs are for functions.
;; If true, "()" is automatically generated after the title.
(define %refentry-function% #t)

;; Generate titlepage indicates whether or not a title page should be
;; generated if a BOOKINFO exists
(define %generate-titlepage% #t)

;; Generate toc indicates whether or not an automatically generated TOC
;; should be included 
(define %generate-toc% #t)

;; Visual acuity levels are "normal", "presbyopic", and 
;;   "large-type"; set the line following to choose the level

(define %visual-acuity% "normal")
;; (define %visual-acuity% "presbyopic")
;; (define %visual-acuity% "large-type")

(define %bf-size%
  (case %visual-acuity%
	(("normal") 10pt)
	(("presbyopic") 12pt)
	(("large-type") 24pt)))
(define-unit em %bf-size%)

;; these font selections are for Windows 95

(define %title-font-family% "Arial")
(define %body-font-family% "Times New Roman")
(define %mono-font-family% "Courier New")
(define %admon-font-family% "Arial")
(define %dingbat-font-family% "WingDings")

(define %line-spacing-factor% 1.1)
(define %head-before-factor% 0.75)
(define %head-after-factor% 0.5)

(define %page-width% 8.5in)
(define %page-height% 11in)

(define %left-right-margin% 6pi)
(define %top-margin%
  (if (equal? %visual-acuity% "large-type") 7.5pi 6pi))
(define %bottom-margin%
  (if (equal? %visual-acuity% "large-type") 7.5pi 6pi))
(define %header-margin%
  (if (equal? %visual-acuity% "large-type") 4.5pi 3pi))
(define %footer-margin% 3pi)

(define %text-width% (- %page-width% (* %left-right-margin% 2)))
(define %body-start-indent% 4pi)
(define %pgwide-start-indent% %body-start-indent%)
(define %body-width% (- %text-width% %body-start-indent%))
(define %para-sep% (/ %bf-size% 2.0))
(define %block-sep% (* %para-sep% 2.0))
(define %hsize-bump-factor% 1.2)
(define %ss-size-factor% 0.6)
(define %ss-shift-factor% 0.4)
(define %smaller-size-factor% 0.9)

;; ====================== BASIC PARAGRAPH TEMPLATE ======================

(define ($paragraph$)
  (make paragraph
	use: para-style
	space-before: %para-sep%
	space-after: %para-sep%
	quadding: 'start
	(process-children-trim)))

;; ============================== FUNCTIONS =============================

(define outer-parent-list
  (list "TOC" "LOT" "APPENDIX" "CHAPTER" "PART" "PREFACE" "REFERENCE"
	"BIBLIOGRAPHY" "GLOSSARY" "INDEX" "SETINDEX"
	"SECT1" "SECT2" "SECT3" "SECT4" "SECT5" "SIMPLESECT"
	"PARTINTRO" "BIBLIODIV" "GLOSSDIV" "INDEXDIV"
	"REFENTRY" "REFSECT1" "REFSECT2" "REFSECT3"
	"MSGTEXT" "MSGEXPLAN"))

(define list-list
  (list "ORDEREDLIST" "ITEMIZEDLIST" "VARIABLELIST" "SEGMENTEDLIST"
        "SIMPLELIST" "CALLOUTLIST" "STEP"))

(define (SECTLEVEL)
  (cond
   ((have-ancestor? "SECT5") 5)
   ((have-ancestor? "SECT4") 4)
   ((have-ancestor? "SECT3") 3)
   ((have-ancestor? "SECT2") 2)
   ((have-ancestor? "SECT1") 1)
   ((have-ancestor? "REFERENCE") 1)
   (else 0))) ;; 0 is the component (chapter/appendix) level

(define (OLSTEP)
  (case
   (modulo (length (hierarchical-number-recursive "ORDEREDLIST")) 4)
	((1) 1.2em)
	((2) 1.2em)
	((3) 1.6em)
	((0) 1.4em)))

(define (ILSTEP) 1.0em)

(define (PROCSTEP ilvl)
  (if (> ilvl 1) 1.8em 1.4em))

(define (PROCWID ilvl)
  (if (> ilvl 1) 1.8em 1.4em))


;; ............................. Title Page .............................

(mode titlepage-recto-mode

  (element TITLE 
    (make paragraph
      font-family-name: %title-font-family%
      font-weight: 'bold
      font-size: (HSIZE 5)
      line-spacing: (* (HSIZE 5) %line-spacing-factor%)
      space-before: (* (HSIZE 5) %head-before-factor%)
      space-after: (* (HSIZE 5) %head-after-factor% 8)
      quadding: 'center
      keep-with-next?: #t
      (process-children-trim)))

  (element AUTHORGROUP
    (make sequence (process-children-trim)))

  (element AUTHOR
    (let ((author-name (author-string))
	  (author-affil (if (not (node-list-empty? 
				  (select-elements 
				   (descendants (current-node)) 
				   "AFFILIATION")))
			    (data (node-list-first
				   (select-elements 
				    (descendants (current-node)) 
				    "AFFILIATION")))
			    "")))

      (make sequence      
	(make paragraph
	  font-family-name: %title-font-family%
	  font-weight: 'bold
	  font-size: (HSIZE 3)
	  line-spacing: (* (HSIZE 1) %line-spacing-factor%)
	  space-before: (* (HSIZE 2) %head-before-factor%)
	  quadding: 'center
	  keep-with-next?: #t
	  (literal author-name))
	(make paragraph
	  font-family-name: %title-font-family%
	  font-weight: 'bold
	  font-size: (HSIZE 1)
	  line-spacing: (* (HSIZE 1) %line-spacing-factor%)
	  space-after: (* (HSIZE 2) %head-after-factor% 4)
	  quadding: 'center
	  keep-with-next?: #t
	  (literal author-affil)))))
)

(mode titlepage-verso-mode

  (element TITLE
    (make paragraph
      font-family-name: %title-font-family%
      font-weight: 'bold
      quadding: 'start
      keep-with-next?: #t
      (process-children-trim)))

  (element AUTHORGROUP
    (make paragraph
      use: para-style
      space-after: (* %bf-size% %line-spacing-factor%)
      (make sequence
	(literal "by ")
	(process-children-trim))))

(element AUTHOR
  ;; Print the author name.  Handle the case where there's no AUTHORGROUP
  (let ((in-group (have-ancestor? "AUTHORGROUP" (current-node))))
    (if (not in-group)
	(make paragraph
	  ;; Hack to get the spacing right below the author name line...
	  use: para-style
	  space-after: (* %bf-size% %line-spacing-factor%)
	  (make sequence
	    (literal "by ")
	    (literal (author-list-string))))
	(make sequence 
	  (literal (author-list-string))))))

)

;; ======================= NON-PRINTING ELEMENTS ========================

;; ................................ META ................................

(element SETINFO (empty-sosofo))
(element BOOKINFO 
  ;; If a titlepage is desired, produce it
  ;; If a TOC is desired, produce it after the title page
  ;; If a titlepage is not desired, but a TOC is, 
  (if %generate-titlepage%
      (make simple-page-sequence
	top-margin: %top-margin%
	bottom-margin: %bottom-margin%
	left-margin: %left-right-margin%
	right-margin: %left-right-margin%
	input-whitespace-treatment: 'collapse
	(make sequence
	  (with-mode titlepage-recto-mode 
	    (make sequence
	      (process-first-descendant "TITLE")
	      (if (node-list-empty? 
		   (select-elements 
		    (descendants (current-node)) "AUTHORGROUP"))
		  (process-first-descendant "AUTHOR")
		  (process-first-descendant "AUTHORGROUP"))
	      (make paragraph
		break-before: 'page
		(literal "")
		(empty-sosofo))))
	  (with-mode titlepage-verso-mode
	    (process-children-trim))
	  (if %generate-toc%
	      (make sequence
		(make paragraph
		  break-before: 'page
		  (literal "")
		  (empty-sosofo))
		(MAKETOC))
	      (empty-sosofo))))
      (if %generate-toc%
	  (make simple-page-sequence
	    top-margin: %top-margin%
	    bottom-margin: %bottom-margin%
	    left-margin: %left-right-margin%
	    right-margin: %left-right-margin%
	    input-whitespace-treatment: 'collapse
	    (MAKETOC))
	  (empty-sosofo))))

(element DOCINFO (empty-sosofo))
(element SECT1INFO (empty-sosofo))
(element SECT2INFO (empty-sosofo))
(element SECT3INFO (empty-sosofo))
(element SECT4INFO (empty-sosofo))
(element SECT5INFO (empty-sosofo))
(element REFSECT1INFO (empty-sosofo))
(element REFSECT2INFO (empty-sosofo))
(element REFSECT3INFO (empty-sosofo))
(element SERIESINFO (empty-sosofo))
(element ARTHEADER (empty-sosofo))

(element COMMENT (empty-sosofo))
(element TITLEABBREV (empty-sosofo))
(element SUBTITLE (empty-sosofo))

;; ......................... BIBLIOGRAPHIC DATA .........................

(define (author-string #!optional (author (current-node)))
  ;; Return a formatted string representation of the contents of AUTHOR
  ;; Handles *only* Honorific, FirstName, SurName, and Lineage.
  ;; Handles *only* the first of each.
  ;; Format is "Honorific. FirstName SurName, Lineage"
  (let* ((h_nl (select-elements (descendants author) "HONORIFIC"))
	 (f_nl (select-elements (descendants author) "FIRSTNAME"))
	 (s_nl (select-elements (descendants author) "SURNAME"))
	 (l_nl (select-elements (descendants author) "LINEAGE"))
	 (has_h (not (node-list-empty? h_nl)))
	 (has_f (not (node-list-empty? f_nl)))
	 (has_s (not (node-list-empty? s_nl)))
	 (has_l (not (node-list-empty? l_nl))))
    (string-append
     (if has_h (string-append (data (node-list-first h_nl)) ".") "")
     (if has_f (string-append 
		(if has_h " " "") 
		(data (node-list-first f_nl))) "")
     (if has_s (string-append 
		(if (or has_h has_f) " " "") 
		(data (node-list-first s_nl))) "")
     (if has_l (string-append ", " (data (node-list-first l_nl))) ""))))


(define (author-list-string #!optional (author (current-node)))
  ;; Return a formatted string representation of the contents of AUTHOR
  ;; *including appropriate punctuation* if the AUTHOR occurs in a list
  ;; of AUTHORs in an AUTHORGROUP:
  ;;
  ;;   John Doe
  ;; or
  ;;   John Doe and Jane Doe
  ;; or
  ;;   John Doe, Jane Doe, and A. Nonymous
  ;;

  (let ((author-count (if (have-ancestor? "AUTHORGROUP" author)
			  (node-list-length (select-elements
					     (descendants
					      (ancestor "AUTHORGROUP"))
					     "AUTHOR"))
			  1)))
    (string-append
     (if (and (> author-count 1)
	      (last-sibling? author))
	 "and "
	 "")
     (author-string)
     (if (and (> author-count 2)
	      (not (last-sibling? author)))
	 ", "
	 (if (> author-count 1)
	     " "
	     "")))))

(element BIBLIOENTRY (empty-sosofo))
(element BIBLIOMISC (empty-sosofo))
(element BOOKBIBLIO (empty-sosofo))

(element ACKNO (empty-sosofo))
(element STREET (empty-sosofo))
(element POB (empty-sosofo))
(element POSTCODE (empty-sosofo))
(element CITY (empty-sosofo))
(element STATE (empty-sosofo))
(element COUNTRY (empty-sosofo))
(element PHONE (empty-sosofo))
(element FAX (empty-sosofo))
(element OTHERADDR (empty-sosofo))
(element AFFILIATION (empty-sosofo))
(element SHORTAFFIL (empty-sosofo))
(element JOBTITLE (empty-sosofo))
(element ORGDIV (empty-sosofo))
(element ARTPAGENUMS (empty-sosofo))

(element AUTHOR
  (make sequence
    (literal (author-list-string))))

(element AUTHORGROUP (process-children))

(element COLLAB (empty-sosofo))
(element COLLABNAME (empty-sosofo))
(element AUTHORINITIALS (empty-sosofo))
(element CONFGROUP (empty-sosofo))
(element CONFDATES (empty-sosofo))
(element CONFTITLE (empty-sosofo))
(element CONFNUM (empty-sosofo))
(element CONFSPONSOR (empty-sosofo))
(element CONTRACTNUM (empty-sosofo))
(element CONTRACTSPONSOR (empty-sosofo))

(element COPYRIGHT
  (make paragraph
    use: para-style
    (make sequence
      (literal "Copyright ")
      (make line-field
	font-family-name:
	(BULLTREAT BULLFONT 1 "copyright" "")
	font-size:
	(BULLTREAT BULLSIZE 1 "copyright" "")
	position-point-shift:
	(BULLTREAT BULLSHIFT 1 "copyright" "")
	field-width: (ILSTEP)
	(literal
	 (BULLTREAT BULLSTR 1 "copyright" "")))
      (literal " ")
      (process-children-trim))))

(element YEAR
  (make sequence
    (process-children)
    (if (not (last-sibling? (current-node)))
	(literal ", ")
	(literal " by "))))

(element HOLDER ($charseq$))

(element CORPAUTHOR (empty-sosofo))
(element CORPNAME (empty-sosofo))
(element DATE (empty-sosofo))
(element EDITION (empty-sosofo))
(element EDITOR (empty-sosofo))
(element ISBN (empty-sosofo))
(element ISSN (empty-sosofo))
(element INVPARTNUMBER (empty-sosofo))
(element ISSUENUM (empty-sosofo))

(element LEGALNOTICE (process-children))

(element MODESPEC (empty-sosofo))

(element ORGNAME ($charseq$))

(element OTHERCREDIT (empty-sosofo))
(element PAGENUMS (empty-sosofo))
(element CONTRIB (empty-sosofo))

(element FIRSTNAME ($charseq$))
(element HONORIFIC ($charseq$))
(element LINEAGE ($charseq$))
(element OTHERNAME ($charseq$))
(element SURNAME ($charseq$))

(element PRINTHISTORY (empty-sosofo))
(element PRODUCTNAME (empty-sosofo))
(element PRODUCTNUMBER (empty-sosofo))
(element PUBDATE (empty-sosofo))
(element PUBLISHER (empty-sosofo))
(element PUBLISHERNAME (empty-sosofo))
(element PUBSNUMBER (empty-sosofo))
(element RELEASEINFO (empty-sosofo))
(element REVHISTORY (empty-sosofo))
(element REVISION (empty-sosofo))
(element REVNUMBER (empty-sosofo))
(element REVREMARK (empty-sosofo))
(element SERIESVOLNUMS (empty-sosofo))
(element VOLUMENUM (empty-sosofo))

;; ................... INDEX TERMS (EMBEDDED MARKERS) ...................

(element INDEXTERM (empty-sosofo))
(element PRIMARY (empty-sosofo))
(element SECONDARY (empty-sosofo))
(element TERTIARY (empty-sosofo))
(element SEE (empty-sosofo))
(element SEEALSO (empty-sosofo))

;; ============================= DIVISIONS ==============================

(define ($divtitlepage$)
  (make simple-page-sequence
	top-margin: %top-margin%
	bottom-margin: %bottom-margin%
	left-margin: %left-right-margin%
	right-margin: %left-right-margin%
	input-whitespace-treatment: 'collapse
	(make paragraph
	      font-family-name: %title-font-family%
	      font-weight: 'bold
	      font-size: (HSIZE 5)
	      line-spacing: (* (HSIZE 5) %line-spacing-factor%)
	      space-before: (* (HSIZE 5) %head-before-factor%)
	      space-after: (* (HSIZE 5) %head-after-factor% 4)
	      quadding: 'start
	      keep-with-next?: #t
	      (process-children-trim))))

(element (SET TITLE) ($divtitlepage$))

(element BOOK 
  ;; Handle two cases:
  ;;   If there's a BOOKINFO, just process children.  BOOKINFO will handle
  ;;      generation of a title page and/or a TOC
  ;;   If there's no BOOKINFO, generate a simple page sequence containing
  ;;      the TOC, if a TOC is desired
  (if (node-list-empty? 
       (select-elements (descendants (current-node)) "BOOKINFO"))
      (make sequence
	(if %generate-toc%
	    (make simple-page-sequence
	      top-margin: %top-margin%
	      bottom-margin: %bottom-margin%
	      left-margin: %left-right-margin%
	      right-margin: %left-right-margin%
	      input-whitespace-treatment: 'collapse
	      (MAKETOC))
	    (empty-sosofo))
	(process-children))
      (process-children)))

(element (ARTHEADER TITLE) ($divtitlepage$))
(element (PART TITLE) ($divtitlepage$))

;; ========================== TABLE OF CONTENTS =========================

(define (NUMLABEL fmt)
  (let ((div0n 
	 ;; Enumerate Chapters and Appendixes
	 (cond
	  ((have-ancestor? "CHAPTER") (ancestor-child-number "CHAPTER"))
	  ((have-ancestor? "APPENDIX") (ancestor-child-number "APPENDIX"))
	  (else #f)))
	(div1n (ancestor-child-number "SECT1"))
	(div2n (ancestor-child-number "SECT2"))
	(div3n (ancestor-child-number "SECT3"))
	(div4n (ancestor-child-number "SECT4"))
	(div5n (ancestor-child-number "SECT5")))
    (string-append
     (if div0n (string-append (NESTEDFNUM div0n fmt)) " ")
     (if div1n (string-append "." (NESTEDFNUM div1n "1"))
	 (if div0n ". " " "))
     (if div2n (string-append "." (NESTEDFNUM div2n "1")) " ")
     (if div3n (string-append "." (NESTEDFNUM div3n "1")) " ")
     (if div4n (string-append "." (NESTEDFNUM div4n "1")) " ")
     (if div5n (string-append "." (NESTEDFNUM div5n "1")) " "))))

(mode toc
      (element (PART TITLE) ($toc-entry$ 1))
      (element (PREFACE TITLE) ($toc-entry$ 1))
      (element (CHAPTER TITLE) ($toc-entry$ 1))
      (element (APPENDIX TITLE) ($toc-entry$ 1))
      (element (GLOSSARY TITLE) ($toc-entry$ 1))
      (element (BIBLIOGRAPHY TITLE) ($toc-entry$ 1))
      (element (INDEX TITLE) ($toc-entry$ 1))
      (element (SECT1 TITLE) ($toc-entry$ 2))
      (element (SECT2 TITLE) ($toc-entry$ 3))
      (element (SECT3 TITLE) ($toc-entry$ 4))
      (element (SECT4 TITLE) ($toc-entry$ 5))
      (element (SECT5 TITLE) ($toc-entry$ 6))
      (default (apply process-matching-children
		      (list "PART" "PREFACE" "CHAPTER" "APPENDIX" 
                            "GLOSSARY" "BIBLIOGRAPHY" "INDEX"
                            "SECT1" "SECT2" "SECT3" "SECT4" "SECT5" "TITLE"))))

(define %toc-indent% 2pi)
(define %toc-spacing-factor% 0.4)

(define ($toc-entry$ level)
  (sosofo-append
   (make paragraph
	 use: para-style
	 start-indent: (+ %body-start-indent%
			  (* %toc-indent% level))
	 first-line-start-indent: (* -1 %toc-indent%)
	 font-weight: (if (= level 1) 'bold 'medium)
	 space-before: (if (= level 1) (* %toc-spacing-factor% 6pt) 0pt)
	 space-after: (if (= level 1) (* %toc-spacing-factor% 6pt) 0pt)
	 quadding: 'start
	 (literal
	  (cond
	   ((have-ancestor? "CHAPTER") (NUMLABEL "1"))
	   ((have-ancestor? "APPENDIX") (NUMLABEL "A"))
	   (else "")))
	 (make link
	       destination: (current-node-address)
	       (with-mode #f (process-children-trim)))
	 (make leader (literal "."))
	 (current-node-page-number-sosofo))))

(define (MAKETOC)
  (sosofo-append
   (make paragraph
	 font-family-name: %title-font-family%
	 font-weight: 'bold
	 font-posture: 'upright
	 font-size: (HSIZE 3)
	 line-spacing: (* (HSIZE 3) %line-spacing-factor%)
	 space-after: (* (HSIZE 4) %head-after-factor%)
	 start-indent: 0pt
	 quadding: 'start
	 keep-with-next?: #t
;;	 break-before: 'page
	 (literal "Table of Contents"))
   (with-mode toc
     ;; Handle two case: books with and without parts.  Books with Parts
     ;; may begin with Preface(s) and are followed by Parts.  Books without
     ;; parts are just sequential collections of chapter-level components.
     ;; If you have something more complex, you'll have to tinker here...
     (let ((book (if (equal? (gi) "BOOK")
		     (current-node)
		     (ancestor "BOOK"))))
       (if (node-list-empty?
	    (select-elements (descendants book) "PART"))

	   (sosofo-append
	    (process-node-list
	     (select-elements (descendants book) "PREFACE"))
	    (process-node-list
	     (select-elements (descendants book) "CHAPTER"))
	    (process-node-list
	     (select-elements (descendants book) "APPENDIX"))
	    (process-node-list
	     (select-elements (descendants book) "GLOSSARY"))
	    (process-node-list
	     (select-elements (descendants book) "BIBLIOGRAPHY"))
	    (process-node-list
	     (select-elements (descendants book) "INDEX")))

	   (sosofo-append
	    (process-node-list
	     (select-elements (descendants book) "PREFACE"))
	    (process-node-list
	     (select-elements (descendants book) "PART")))
	   )))
   ;; force a page break
   (make paragraph
     break-before: 'page
     (literal "")
     (empty-sosofo))))

;; ============================= COMPONENTS =============================
;;
;; in docbook, components are containers at the chapter/appendix level

(define ($component$)
  (let ((page-header
         (make sequence
	       use: para-style
	       font-posture: 'italic
	       (with-mode hf-mode (process-first-descendant "TITLE"))))
	(page-footer
         (make sequence
	       use: para-style
	       font-posture: 'italic
	       (literal 
		(string-append
		 "Page "
		 (cond
		  ((equal? (gi) "CHAPTER")
		   (format-number (element-number) "1"))
		  ((equal? (gi) "APPENDIX")
		   (format-number (element-number) "A"))
		  ((equal? (gi) "GLOSSARY") "Glossary")
		  ((equal? (gi) "PREFACE") "Preface")
		  (else ""))
		 "-"))
	       (page-number-sosofo))))
    (make simple-page-sequence
	  page-number-restart?: #t
	  use: para-style
	  start-indent: %body-start-indent%
	  left-header:  (if-front-page (empty-sosofo) page-header)
	  right-header: (if-front-page page-header (empty-sosofo))
	  left-footer: 	(if-front-page (empty-sosofo) page-footer)
	  right-footer: (if-front-page page-footer (empty-sosofo))
	  top-margin: %top-margin%
	  bottom-margin: %bottom-margin%
	  left-margin: %left-right-margin%
	  right-margin: %left-right-margin%
	  header-margin: %header-margin%
	  footer-margin: %footer-margin%
	  page-width: %page-width%
	  page-height: %page-height%
	  input-whitespace-treatment: 'collapse
	  quadding: 'start
	  (process-children-trim))))

;; this is how we prevent the title in the header from acquiring the
;;   display treatment that it receives in the body of the document
;;
(mode hf-mode
  (element TITLE
	   (make sequence
		 (literal
		  (cond ((have-ancestor? "CHAPTER")
			 (CHAP-APP-HEAD-LABEL "Chapter"))
			((have-ancestor? "APPENDIX")
			 (CHAP-APP-HEAD-LABEL "Appendix"))
			(else "")))
		 (process-children-trim))))

(define ($comptitle$)
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
	(literal
	  (cond ((have-ancestor? "CHAPTER")
		 (CHAP-APP-HEAD-LABEL "Chapter"))
		((have-ancestor? "APPENDIX")
		 (CHAP-APP-HEAD-LABEL "Appendix"))
		(else "")))
	(process-children-trim)))

(define (CHAP-APP-HEAD-LABEL chap-or-app)
  (let ((label
	 (attribute-string "label" (ancestor chap-or-app))))
    (string-append 
     chap-or-app
     " "
     (if label
	 (if (equal? label "auto")
	     (format-number
	      (element-number (ancestor chap-or-app))
	      (if (equal? chap-or-app "Chapter") "1" "A"))
	   label)
       (format-number
	(element-number (ancestor chap-or-app))
	(if (equal? chap-or-app "Chapter") "1" "A")))
     ". ")))

(element APPENDIX ($component$))
(element (APPENDIX TITLE) ($comptitle$))
(element CHAPTER ($component$))
(element (CHAPTER TITLE) ($comptitle$))
(element PREFACE ($component$))
(element (PREFACE TITLE) ($comptitle$))
(element REFERENCE ($component$))
(element (REFERENCE TITLE) ($comptitle$))
(element BIBLIOGRAPHY ($component$))
(element (BIBLIOGRAPHY TITLE) ($comptitle$))
(element GLOSSARY ($component$))
(element (GLOSSARY TITLE) ($comptitle$))
(element INDEX ($component$))
(element (INDEX TITLE) ($comptitle$))
(element SETINDEX ($component$))
(element (SETINDEX TITLE) ($comptitle$))

;; need test cases to do toc/lot; do these later

(element TOC ($component$))
(element (TOC TITLE) ($comptitle$))
(element TOCFRONT ($paragraph$))
(element TOCENTRY ($paragraph$))
(element TOCPART (process-children))
(element TOCCHAP (process-children))
(element TOCLEVEL1 (process-children))
(element TOCLEVEL2 (process-children))
(element TOCLEVEL3 (process-children))
(element TOCLEVEL4 (process-children))
(element TOCLEVEL5 (process-children))
(element TOCBACK ($paragraph$))
(element LOT ($component$))
(element (LOT TITLE) ($comptitle$))
(element LOTENTRY ($paragraph$))


;; ============================== SECTIONS ==============================

(define ($section$) ($block-container$))
(define ($sectitle$)
  (let* ((renderas (inherited-attribute-string "renderas"))
	 (hlevel                         ;; the apparent section level;
	   (if renderas                  ;; if not real section level,
	     (string->number             ;;   then get the apparent level
	       (substring renderas 4 5)) ;;   from "renderas",
	     (SECTLEVEL)))               ;; else use the real level
	(hs (HSIZE (- 4 hlevel)))
	(label (attribute-string "label")))
  (make paragraph
	font-family-name: %title-font-family%
	font-weight:  (if (< hlevel 5) 'bold 'medium)
	font-posture: (if (< hlevel 5) 'upright 'italic)
	font-size: hs
	line-spacing: (* hs %line-spacing-factor%)
	space-before: (* hs %head-before-factor%)
	space-after: (* hs %head-after-factor%)
	start-indent:
	  (if (< hlevel 3)
	      0pt
	      %body-start-indent%)
	first-line-start-indent: 0pt
	quadding: 'start
	keep-with-next?: #t
	(literal
	 (if label
	     (if (equal? label "AUTO")
		 (let ((chn (ancestor-child-number "CHAPTER"))
		       (apn (ancestor-child-number "APPENDIX"))
		       (s1n (ancestor-child-number "SECT1"))
		       (s2n (ancestor-child-number "SECT2"))
		       (s3n (ancestor-child-number "SECT3"))
		       (s4n (ancestor-child-number "SECT4"))
		       (s5n (ancestor-child-number "SECT5")))
		   (string-append
		    (cond
		     (chn (FNUM chn))
		     (apn (FNUM apn))
		     (else "X"))
		    (if s1n (string-append "." (FNUM s1n)) " ")
		    (if s2n (string-append "." (FNUM s2n)) " ")
		    (if s3n (string-append "." (FNUM s3n)) " ")
		    (if s4n (string-append "." (FNUM s4n)) " ")
		    (if s5n (string-append "." (FNUM s5n)) " ")))
	     (string-append label " "))
	  (string-append "")))
	(process-children-trim))))

(element SECT1 ($section$))
(element (SECT1 TITLE) ($sectitle$))
(element SECT2 ($section$))
(element (SECT2 TITLE) ($sectitle$))
(element SECT3 ($section$))
(element (SECT3 TITLE) ($sectitle$))
(element SECT4 ($section$))
(element (SECT4 TITLE) ($sectitle$))
(element SECT5 ($section$))
(element (SECT5 TITLE) ($sectitle$))

(element SIMPLESECT ($section$))
(element (SIMPLESECT TITLE) ($sectitle$))

(element PARTINTRO ($section$))
(element (PARTINTRO TITLE) ($sectitle$))

(element BIBLIODIV ($section$))
(element (BIBLIODIV TITLE) ($sectitle$))
(element GLOSSDIV ($section$))
(element (GLOSSDIV TITLE) ($sectitle$))
(element INDEXDIV ($section$))
(element (INDEXDIV TITLE) ($sectitle$))


;; =========================== REFERENCE PAGES ==========================

(element REFENTRY ($block-container$))

(element REFMETA
  (let* ((slevel (SECTLEVEL)) ;; the true level in the section hierarchy
	 (hlevel (if (> slevel 2) 2 slevel)) ;; limit to sect2 equiv.
	 (hs (HSIZE (- 4 hlevel))))
    (make paragraph
	  font-family-name: %title-font-family%
	  font-weight: 'bold
	  font-size: hs
	  line-spacing: (* hs %line-spacing-factor%)
	  space-before: (* hs %head-before-factor%)
	  space-after: (* hs %head-after-factor%)
	  start-indent: %body-start-indent%
	  first-line-start-indent: (- %body-start-indent%)
	  quadding: 'start
	  keep-with-next?: #t
	  (process-first-descendant "REFENTRYTITLE")
	  (if %refentry-function%
	      (sosofo-append
	       (literal " (")
	       (process-first-descendant "MANVOLNUM")
	       (literal ")"))
	      (sosofo-append (empty-sosofo)))
)))

(element REFMISCINFO (empty-sosofo)) ;; *** TO DO: finish this

(element REFNAMEDIV
  (make paragraph
	use: para-style
	space-before: %para-sep%
	start-indent: %body-start-indent%
	quadding: 'start
	(process-children)))

(element REFNAME
  (make sequence
    (if %refentry-generate-name%
	(make sequence
	  font-weight: 'bold
	  (literal "NAME "))
	(make sequence 
	  (literal "")))
    (make sequence
	  font-weight: 'medium
	  font-family-name: %mono-font-family%
	  (process-children)
	  (literal " "))))

(element REFPURPOSE
  (make sequence
	font-family-name: %body-font-family%
    (make sequence
	  (literal "-- ")
	  (process-children))
    (make paragraph-break)))
	
(element REFDESCRIPTOR (empty-sosofo)) ;; TO DO: finish this

(element REFCLASS
  (let ((role (attribute-string "role")))
    (make paragraph
	  use: para-style
	  space-before: %para-sep%
	  start-indent: %body-start-indent%
	  quadding: 'start
	  (make sequence
		font-weight: 'bold
		(literal
		  (if role
		      (string-append role ": ")
		      "")))
	  (process-children-trim))))

(element REFSYNOPSISDIV
  (make paragraph
	use: para-style
	space-before: %para-sep%
	start-indent: %body-start-indent%
	lines: 'asis
	font-family-name: %mono-font-family%
	(process-children)))

(element (REFSYNOPSISDIV TITLE) ($lowtitle$ 1))
(element REFSECT1 ($block-container$))
(element (REFSECT1 TITLE) ($lowtitle$ 1))
(element REFSECT2 ($block-container$))
(element (REFSECT2 TITLE) ($lowtitle$ 2))
(element REFSECT3 ($block-container$))
(element (REFSECT3 TITLE) ($lowtitle$ 3))


;; ======================== ERROR MESSAGES (ETC.) =======================

(element MSGSET (process-children))

(element MSGENTRY ($block-container$))

(element MSG
  (make display-group
	font-weight: 'bold
	font-family-name: %mono-font-family%
	(process-children)))

(element MSGMAIN (process-children))

(element MSGSUB
  (make display-group
	start-indent: (+ (inherited-start-indent) (ILSTEP))
	(process-children)))

(element MSGREL
  (make display-group
	font-weight: 'bold
	(process-children)))

(element MSGTEXT (process-children))

(element MSGINFO ($indent-para-container$))

(define ($genhead-para$ headtext)
  (make paragraph
	space-before: %para-sep%
	space-after: %para-sep%
	(make sequence
	      font-weight: 'bold
	      (literal
	        (string-append headtext ": ")))
	(process-children)))

(element MSGLEVEL ($genhead-para$ "Level"))
(element MSGORIG ($genhead-para$ "Origin"))
(element MSGAUD ($genhead-para$ "Audience"))

(element MSGEXPLAN ($indent-para-container$))
(element (MSGEXPLAN TITLE) ($runinhead$))
(element (MSGEXPLAN PARA) (make sequence (process-children)))


;; ================= UNCLASSIFIED BLOCK-LEVEL ELEMENTS ==================

(element TITLE ($lowtitle$ 2))         ;; the default TITLE format
(element BRIDGEHEAD ($lowtitle$ 2))

(element SIDEBAR ($block-container$))
(element ABSTRACT ($block-container$))
(element AUTHORBLURB ($block-container$))

(element BLOCKQUOTE
  (make paragraph
	font-size: (* %bf-size% %smaller-size-factor%)
	line-spacing: (* %bf-size% %line-spacing-factor%
			 %smaller-size-factor%)
	space-before: %para-sep%
	start-indent: (+ %body-start-indent% 1em)
	end-indent: 1em
	(process-children-trim)))

(element ATTRIBUTION ($paragraph$))
(element EPIGRAPH ($block-container$))
(element FOOTNOTE (empty-sosofo)) ;; can't deal with this yet -- revisit
(element HIGHLIGHTS ($block-container$))

(element FORMALPARA ($para-container$))
(element (FORMALPARA TITLE) ($runinhead$))
(element (FORMALPARA PARA) (make sequence (process-children)))

(element PARA ($paragraph$))
(element SIMPARA ($paragraph$))

(element (ROW ENTRY PARA)
  (make paragraph
	use: para-style
	(process-children-trim)))

(element (THEAD ROW ENTRY PARA)
  (make paragraph
	font-size: %bf-size%
	font-family-name: %title-font-family%
	font-weight: 'bold
	line-spacing: (* %bf-size% %line-spacing-factor%)
	quadding: 'start
	(process-children-trim)))

;; ============================ ADMONITIONS =============================

(define ($admonition$)
  (make display-group
	space-before: %block-sep%
	space-after: %block-sep%
	(process-children)))

(define ($admonpara$)
  (make paragraph
	space-before: %para-sep%
	space-after: %para-sep%
	font-size: (- %bf-size% 1pt)
	font-weight: 'medium
	font-posture: 'upright
	font-family-name: %admon-font-family%
	line-spacing: (* (- %bf-size% 1pt) %line-spacing-factor%)
	start-indent: (+ (inherited-start-indent) (* (ILSTEP) 2))
	(make sequence
	      font-family-name: %title-font-family%
	      font-weight: 'bold
	      (literal
	       (if (= (child-number) 1)
		   (cond
		     ((have-ancestor? "IMPORTANT") "IMPORTANT: ")
		     ((have-ancestor? "NOTE") "NOTE: ")
		     ((have-ancestor? "TIP") "TIP: ")
		     (else ""))
		   "")))
	(process-children)))

(element IMPORTANT ($admonition$))
(element (IMPORTANT PARA) ($admonpara$))
(element NOTE ($admonition$))
(element (NOTE PARA) ($admonpara$))
(element TIP ($admonition$))
(element (TIP PARA) ($admonpara$))

;; perils are given special treatment by generating a centered title
;;   and throwing a box around them
;; note that the paragraph indents are set by the box characteristics
;;
(define ($peril$)
  (let ((hs (HSIZE 2)))
    (make display-group
	  space-before: %block-sep%
	  space-after: %block-sep%
	  (make box
		display?: #t
		box-type: 'border
		line-thickness: 2pt
		start-indent: (+ (inherited-start-indent) (* 2 (ILSTEP)) 2pt)
		end-indent: (inherited-end-indent)
		(make paragraph
		      space-before: %para-sep%
		      space-after: %para-sep%
		      start-indent: 1em
		      end-indent: 1em
		      font-family-name: %title-font-family%
		      font-weight: 'bold
		      font-size: hs
		      line-spacing: (* hs %line-spacing-factor%)
		      quadding: 'center
		      keep-with-next?: #t
		      (literal
		        (cond
			  ((equal? (gi) "CAUTION") "CAUTION")
			  ((equal? (gi) "WARNING") "WARNING")
			  (else ""))))
		(process-children)))))

(element CAUTION ($peril$))
(element WARNING ($peril$))

;; ========================= GLOSSARY ELEMENTS ==========================

(element GLOSSLIST ($block-container$))
(element GLOSSENTRY ($para-container$))

(element GLOSSTERM ($lowtitle$ 3))
(element GLOSSDEF ($indent-para-container$))

(element GLOSSSEE ($italic-seq$))
(element GLOSSSEEALSO ($italic-seq$))


;; =============================== LISTS ================================


(define ($list$)
 (make display-group
       space-before: (if (INLIST?) %para-sep% %block-sep%)
       space-after:  (if (INLIST?) %para-sep% %block-sep%)))
 
(element ITEMIZEDLIST ($list$))

(element (ITEMIZEDLIST LISTITEM)
  (make paragraph
	start-indent: (+ (inherited-start-indent) (ILSTEP))
	(process-children)))

(element (ITEMIZEDLIST LISTITEM PARA)
  (let ((spacing (inherited-attribute-string "spacing")))
    (if (= (child-number) 1)
      (let ((ilevel 
	      (length (hierarchical-number-recursive "ITEMIZEDLIST")))
	    (override
	      (inherited-attribute-string "override"))
	    (spacing
	      (inherited-attribute-string "spacing"))
	    (mark
	      (inherited-attribute-string "mark")))
	(make paragraph
	      use: para-style
	      space-before: (if (equal? "COMPACT" spacing)
				0pt
			      %para-sep%)
	      first-line-start-indent: (- (ILSTEP))
	      (make line-field
		    font-family-name:
		      (BULLTREAT BULLFONT ilevel override mark)
		    font-size:
		      (BULLTREAT BULLSIZE ilevel override mark)
		    position-point-shift:
		      (BULLTREAT BULLSHIFT ilevel override mark)
		    field-width: (ILSTEP)
		    (literal
		      (BULLTREAT BULLSTR ilevel override mark)))
	      (make sequence
		    first-line-start-indent: 0pt
		    (process-children-trim))))
    (make paragraph
	  use: para-style
	  space-before: (if (equal? "COMPACT" spacing)
			    0pt
			  %para-sep%)
	  (process-children-trim)))))

(element ORDEREDLIST ($list$))

(element (ORDEREDLIST LISTITEM)
  (make paragraph
	start-indent: (+ (inherited-start-indent) (OLSTEP))
	(process-children)))

(element (ORDEREDLIST LISTITEM PARA)
  (let ((spacing (inherited-attribute-string "spacing")))
    (if (= (child-number) 1)
      (make paragraph
	    use: para-style
	    space-before: (if (equal? "COMPACT" spacing)
			      0pt
			    %para-sep%)
       first-line-start-indent: (- (OLSTEP))
       (make line-field
	     field-width: (OLSTEP)
	     (literal
	      (case (modulo (length
		(hierarchical-number-recursive "ORDEREDLIST")) 4)
		    ((1) (string-append
			  (format-number (PARNUM) "1") "."))
		    ((2) (string-append 
			  (format-number (PARNUM) "a") "."))
		    ((3) (string-append
			  "(" (format-number (PARNUM) "i") ")"))
		    ((0) (string-append
			  "(" (format-number (PARNUM) "a") ")")))))
       (make sequence
	     first-line-start-indent: 0pt
       (process-children-trim)))
    (make paragraph
	  use: para-style
	  space-before: (if (equal? "COMPACT" spacing)
			    0pt
			  %para-sep%)
	  (process-children-trim)))))

(element VARIABLELIST ($list$))
(element VARLISTENTRY ($para-container$))
(element (VARLISTENTRY TERM)
  (let ((termlength
	  (attribute-string "termlength" (ancestor "VARIABLELIST"))))
    (make paragraph
	  use: para-style
	  space-before: %para-sep%
	  end-indent: (if termlength
			  (- %text-width% (PARSEDUNIT termlength))
			  0pt)
	  (process-children-trim))))
(element (VARLISTENTRY LISTITEM PARA)
  (make paragraph
	use: para-style
	space-before: %para-sep%
	space-after: %para-sep%
	start-indent: (+ (inherited-start-indent) 2em)
	(process-children-trim)))

(element SIMPLELIST
  (let ((type (attribute-string "type")))
    (if (equal? type "INLINE")
	(make sequence
	  (process-children-trim))
	(make display-group
	  space-before: (if (INLIST?) %para-sep% %block-sep%)
	  space-after:  (if (INLIST?) %para-sep% %block-sep%)
	  (process-children-trim)))))

(element MEMBER
  (let ((type (inherited-attribute-string "type")))
    (if (equal? type "INLINE")
	(make sequence
	  (process-children-trim)
	  (if (not (last-sibling?))
	      (literal ", ")
	      (literal "")))
	(make paragraph
	  start-indent: (+ (inherited-start-indent) (ILSTEP))
	  (process-children)))))

;; TO DO: deal with these

(element SEGMENTEDLIST (process-children))
(element (SEGMENTEDLIST TITLE) ($lowtitle$ 2))
(element SEGTITLE ($paragraph$))
(element SEGLISTITEM ($paragraph$))
(element SEG ($paragraph$))
(element CALLOUTLIST (process-children))
(element (CALLOUTLIST TITLE) ($lowtitle$ 2))
(element CALLOUT ($paragraph$))


;; ============================= PROCEDURES =============================

(element PROCEDURE ($list$))
(element (PROCEDURE TITLE) ($lowtitle$ 2))

(element SUBSTEPS
  (make display-group
	space-before: %para-sep%
	space-after: %para-sep%
	start-indent: (+ (inherited-start-indent) (PROCSTEP 2))))

(element (STEP PARA)
  (let ((ilevel 
	 (length (hierarchical-number-recursive "STEP"))))
    (if (= (child-number) 1)
	(make paragraph
	      use: para-style
	      space-before: %para-sep%
	      start-indent: (+ (inherited-start-indent)
			       (PROCSTEP ilevel))
	      first-line-start-indent: (- (PROCSTEP ilevel))
	      font-weight: 'bold
	      (make line-field
		    field-width: (PROCSTEP ilevel)
		    (literal
		     (if (= 1 ilevel)
			 (string-append 
			  (format-number
			   (ancestor-child-number "STEP") "1") ".")
		         (string-append
			  (format-number
			   (list-ref
			    (hierarchical-number-recursive "STEP")
			    (- ilevel 2)) "1")
			  (format-number
			   (ancestor-child-number "STEP") "a") "."))))
	      (make sequence
		    first-line-start-indent: 0pt
		    (process-children-trim)))
    (make paragraph
	  use: para-style
	  space-before: %para-sep%
	  start-indent: (+ (inherited-start-indent) (PROCSTEP ilevel))
	  (process-children-trim)))))

;; ======================= EXAMPLES AND LISTINGS ========================

(element EXAMPLE ($block-container$))
(element (EXAMPLE TITLE) ($lowtitle$ 2))
(element INFORMALEXAMPLE ($block-container$))

(define ($verbatim-display$)
  (let* ((width-in-chars
	  (if (attribute-string "width")
	      (string->number (attribute-string "width"))
	    60)) ;; the default is a maximum line length of 60 chars
	 (fsize (lambda () (/ (/ (- %text-width% (inherited-start-indent))
	     width-in-chars) 0.7))))
    (make paragraph
	  space-before: (if (INLIST?) %para-sep% %block-sep%)
	  space-after:  (if (INLIST?) %para-sep% %block-sep%)
	  font-family-name: %mono-font-family%
	  font-size: (fsize)
	  font-weight: 'medium
	  font-posture: 'upright
	  line-spacing: (* (fsize) %line-spacing-factor%)
	  lines: 'asis
          input-whitespace-treatment: 'preserve
	  quadding: 'start
	  (process-children))))

(define ($linespecific-display$)
  (let* ((width-in-chars
	  (if (attribute-string "width")
	      (string->number (attribute-string "width"))
	    60)) ;; the default is a maximum line length of 60 chars
	 (fsize (lambda () (/ (/ (- %text-width% (inherited-start-indent))
	     width-in-chars) 0.7))))
    (make paragraph
	  space-before: (if (INLIST?) %para-sep% %block-sep%)
	  space-after:  (if (INLIST?) %para-sep% %block-sep%)
	  lines: 'asis
          input-whitespace-treatment: 'preserve
	  quadding: 'start
	  (process-children))))

(element LITERALLAYOUT ($linespecific-display$))
(element ADDRESS ($linespecific-display$))
(element PROGRAMLISTING ($verbatim-display$))
(element SCREEN ($verbatim-display$))

;; screenshot is a graphic with possible screeninfo
;; *** TO DO: deal with this
(element SCREENSHOT (process-children))

;; screenco can have children areaspec, screen, calloutlist
;; programlistingco can have children areaspec, programlisting, calloutlist
;; don't know how to deal with these yet
;; *** TO DO: deal with these
(element SCREENINFO ($paragraph$))
(element AREASPEC ($paragraph$))
(element AREA ($paragraph$))
(element AREASET ($paragraph$))

(element PROGRAMLISTINGCO (process-children))
(element SCREENCO (process-children))

;; ==================== FIGURES, GRAPHICS, EQUATIONS ====================

(element FIGURE 
  (make display-group
	(with-mode figure-caption-mode
		   (process-first-descendant "TITLE"))
	(process-children)))

(element GRAPHIC ($image$))
(element INLINEGRAPHIC ($image$))

(define ($image$)
  (let ((fileref (attribute-string "fileref"))
	(entityref (attribute-string "entityref")))
    (make paragraph
	  space-before: %block-sep%
	  space-after: %block-sep%
	  (make external-graphic ; p.243
		entity-system-id: (if fileref fileref
				    (if entityref 
					(entity-generated-system-id entityref)
				      ""))
		display?: #t
		display-alignment: 'start))))

(mode figure-caption-mode
  (element TITLE
    (let ((label (attribute-string "label" (ancestor "figure"))))
      (make paragraph
	    use: para-style
	    font-weight: 'bold
	    space-before: %block-sep%
	    space-after: %para-sep%
	    keep-with-next?: #t
	    (literal
	     (string-append
	      "Figure "
	      (if label
		  label
		(format-number (element-number (parent (current-node))) "1"))
	      ". "))
	    (process-children-trim)))))

(element (FIGURE TITLE) (empty-sosofo)) ; don't show caption below figure

;; *** TO DO: deal with these
(element GRAPHICCO (process-children))
(element EQUATION (process-children))
(element (EQUATION TITLE) ($lowtitle$ 2))
(element INFORMALEQUATION (process-children))
(element INLINEEQUATION (process-children))


;; ========================= SYNTAX DEFINITIONS =========================
;; *** TO DO: this is just a placeholder; need examples to define styles

(element SYNOPSIS (process-children))
(element CMDSYNOPSIS (process-children))
(element ARG ($paragraph$))
(element GROUP ($paragraph$))
(element SBR ($paragraph$))
(element SYNOPFRAGMENTREF ($paragraph$))
(element SYNOPFRAGMENT (process-children))
(element FUNCSYNOPSIS (process-children))
(element FUNCSYNOPSISINFO (process-children))
(element FUNCPROTOTYPE (process-children))
(element FUNCDEF ($paragraph$))
(element VOID ($paragraph$))
(element VARARGS (process-children))
(element PARAMDEF (process-children))
(element FUNCPARAMS (process-children))


;; ============================== INLINES ===============================

(element ACCEL ($charseq$))
(element ACTION ($charseq$))
(element APPLICATION ($charseq$))
(element CLASSNAME ($charseq$))
(element COMMAND ($bold-seq$))
(element COMPUTEROUTPUT ($mono-seq$))
(element DATABASE ($charseq$))
(element EMAIL ($charseq$))
(element ERRORNAME ($charseq$))
(element ERRORTYPE ($charseq$))
(element FILENAME ($charseq$))
(element FUNCTION ($bold-seq$))
(element GUIBUTTON ($charseq$))
(element GUIICON ($charseq$))
(element GUILABEL ($charseq$))
(element GUIMENU ($charseq$))
(element GUIMENUITEM ($charseq$))
(element GUISUBMENU ($charseq$))
(element HARDWARE ($charseq$))
(element INTERFACE ($charseq$))
(element INTERFACEDEFINITION ($charseq$))
(element KEYCAP ($bold-seq$))
(element KEYCODE ($charseq$))
(element KEYCOMBO ($charseq$))
(element KEYSYM ($charseq$))
(element LITERAL ($mono-seq$))
(element MEDIALABEL ($italic-seq$))
(element MENUCHOICE ($charseq$))
(element SHORTCUT ($bold-seq$))
(element MOUSEBUTTON ($charseq$))
(element OPTION ($charseq$))
(element OPTIONAL ($charseq$))
(element PARAMETER ($italic-mono-seq$))
(element PROPERTY ($charseq$))
(element REPLACEABLE ($italic-mono-seq$))
(element RETURNVALUE ($charseq$))
(element STRUCTFIELD ($italic-mono-seq$))
(element STRUCTNAME ($charseq$))
(element SYMBOL ($charseq$))
(element SYSTEMITEM ($charseq$))
(element TOKEN ($charseq$))
(element TYPE ($charseq$))
(element USERINPUT ($bold-mono-seq$))
(element ABBREV ($charseq$))
(element ACRONYM ($charseq$))
(element CITATION ($charseq$))
(element CITEREFENTRY ($charseq$))
(element CITETITLE ($charseq$))
(element CO ($charseq$))
(element EMPHASIS ($bold-seq$))
(element FIRSTTERM ($bold-seq$))
(element FOREIGNPHRASE ($italic-seq$))
(element MARKUP ($charseq$))
(element PHRASE ($charseq$))
(element QUOTE ($charseq$))
(element SGMLTAG ($charseq$))
(element TRADEMARK ($charseq$))
(element WORDASWORD ($italic-seq$))

(element LINEANNOTATION
  (make sequence
	use: para-style
	(process-children-trim)))

(define ($ss-seq$ plus-or-minus)
  (make sequence
	font-size:
	  (* (inherited-font-size) %ss-size-factor%)
	position-point-shift:
	  (plus-or-minus (* (inherited-font-size) %ss-shift-factor%))
	(process-children-trim)))

(element SUPERSCRIPT ($ss-seq$ +))
(element SUBSCRIPT ($ss-seq$ -))


;; ========================= LINKS AND ANCHORS ==========================

(element LINK ($charseq$))
(element OLINK ($charseq$))
(element ULINK ($charseq$))
(element FOOTNOTEREF (empty-sosofo))
(element XREF (empty-sosofo))
(element ANCHOR (empty-sosofo))
(element BEGINPAGE (empty-sosofo))

;; =========================== INDEX ELEMENTS ===========================

(element INDEXENTRY (process-children))
(element PRIMARYIE ($paragraph$))
(element SECONDARYIE ($paragraph$))
(element TERTIARYIE ($paragraph$))
(element SEEIE ($paragraph$))
(element SEEALSOIE ($paragraph$))


;; 961028 -- bosak
;;   TABLE element spec changed
;;   p-style changed to para-style
;; 961123 -- bosak
;;   TABLE and TITLE elements modified
;;   margins changed
;;   added INFORMALTABLE (docbook-specific)
;; 961124 -- bosak
;;   mods to THEAD
;; 970116 -- bosak
;;   method for assigning frame-attribute on TGROUP changed from
;;     attribute-string to inherited-attribute string to cope
;;     with FRAME attribute set on either TABLE or INFORMALTABLE
;;     (docbook-specific)
;;   mods to TITLE, cell indents & margins
;; 970215 -- berglund, communicated 970128
;;   n-rows-spanned fixed to accommodate #IMPLIED value for morerows
;;     rather than default value of 0 (DocBook 2.x -> 3.0 change to
;;     align with SGML Open table model)
;; 970215 -- graham, communicated 970202
;;   pgwide accommodated
;; Notes by bosak:
;;   For (informal)tables and table titles to align correctly with
;;     paragraphs immediately preceding them, the (informal)table
;;     must be a child of the preceding paragraph.
;;   To get a table frame and rules around each cell, you must set
;;     frame="all" on every TABLE and INFORMALTABLE and
;;     colsep=1 rowsep=1 on every TGROUP.
;;   All further notes by Anders Berglund except where indicated.
;;
;; ============================ TABLES ===============================
;
; *** DRAFT VERSION ****
;
; Copyright (C), Berglund Consulting & Type Foundry 1996.
; Permission to copy in any form is granted for use in  
; DSSSL applications, provided this notice is included in
; all copies.
;
; This supports the specifications in the "Exchange model" in the
; SGML Open Technical Resolution TR 9503:1995
; with the modification that SPANSPECs are supported.
; Thus the following is NOT supported:
; - mixed measure - e.g. 2*+3pt - for colspecs

; Caution
; - NOTE that vertical column spans are not supported by Microsoft Word
; - NOTE that for RTF the table foot is placed at the end of the table;
;   table heads are correctly repeated at page breaks in a table
;
; Tailor these values to go with the rest of the DSSSL application
; and for the desired default values
;
(define %cals-rule-default% 0)
(define %cals-valign-default% "TOP")
; cell margins - a 4pt value assumes that the paragraphs in the
;                cells have a 0 start and end indent so that a  
;                margin needs to be specified on the cells
;; bosak has tweaked all these
(define %cals-cell-before-row-margin% 3pt)
(define %cals-cell-after-row-margin% 3pt)
(define %cals-cell-before-column-margin% 3pt)
(define %cals-cell-after-column-margin% 3pt)
; value for start and end indent; initial value for inheritance in the
;                                 cells
(define %cals-cell-content-start-indent% 3pt)
(define %cals-cell-content-end-indent% 2pt)
;
; These may need changing for the desired style
;
(element INFORMALTABLE
  (make display-group
	space-before: %block-sep%
	space-after: %block-sep%
	(process-children)))

(element TABLE
  (make display-group
	space-before: %block-sep%
	space-after: %block-sep%
	;; pgwide handling by Tony Graham
	start-indent: (let ((pgwide (attribute-string "pgwide")))
			(if
			 (string? pgwide)
			 (if
			  (not
			   (= (string->number pgwide) 0))
			  %pgwide-start-indent%
			  (inherited-start-indent))
			 (inherited-start-indent)))
	(with-mode table-caption-mode
		   (process-first-descendant "TITLE"))
	(process-children)))

(mode table-caption-mode
  (element TITLE
    (let ((label (attribute-string "label" (ancestor "table"))))
      (make paragraph
	    use: para-style
	    font-weight: 'bold
	    space-before: %block-sep%
	    space-after: %block-sep%
	    keep-with-next?: #t
	    (literal
	     (string-append
	      "Table "
	      (if label
		  label
		(format-number (element-number (parent (current-node))) "1"))
	      ". "))
	    (process-children-trim)))))

;---------------------------------------------------------------------
;   
; There should be no need to change the specification below 
;
;---------------------------------------------------------------------

(define (CALS-COLSPEC-UNIT u)
 (if (string? u)
    (let ((strlen (string-length u)))
    (if (string=? "*" (substring u (- strlen 1) strlen)) 
        (let* ((pnum (substring u 0 (- strlen 1))))
             (if (number? (string->number pnum))
                  (table-unit (string->number pnum))
                  (table-unit 1)))
        (if (> strlen 2)
             (let ((u-s-i (UNAME-START-INDEX u (- strlen 1))))
             (if (= u-s-i 0) ;; there's no number here
                  1pi         ;; so return something that might work
                  (if (= u-s-i strlen)           ;; there's no unit name here
                      (* (string->number u) 1pt) ;; so default to points
                      (let* ((unum (string->number
                                    (substring u 0 u-s-i)))
                              (uname (STRING-DOWNCASE
                                       (substring u u-s-i strlen))))
                        (case uname
                              (("mm") (* unum 1mm))
                              (("cm") (* unum 1cm))
                              (("in") (* unum 1in))
                              (("pi") (* unum 1pi))
                              (("pt") (* unum 1pt))
                              (else
                               (cond 
                                ((number? unum)
                                 (* unum 1pt))
                                ((number? (string->number u))
                                 (* (string->number u) 1pt))
                                      (else u))))))))
             (if (number? (string->number u))
                  (* (string->number u) 1pt)
                  (table-unit 1)))))
    (table-unit 1)))


; given a node list "nodes" find the snl that has a gi matching "giname"
; and an attribute "attname" that has the value "attval"
; if no such node return #f
(define (GI-ATTVAL-NODE-IN-NODELIST giname attname attval nodes)
  (let* ((n (node-list-first nodes)) ;; has to be let* bosak 961123
	 (attnamestr (attribute-string attname n))) ;; added by bosak 961123
    (if (and (string=? (gi n) giname) attnamestr (string=? attnamestr attval)) ;; check for attnamestr added by bosak 961123
;;    (if (and (string=? (gi n) giname) (string=? (attribute-string attname n) attval)) ;; old version dies if attribute-string returns #f
        n
        (if (node-list-empty? (node-list-rest nodes))
            #f
            (GI-ATTVAL-NODE-IN-NODELIST giname attname attval (node-list-rest nodes))))))

; find the child number of the "colspec" that has a "colname" attribute
; whose value matches the "namest" attribute value of the "spanspec"
; whose "spannane" attribute value matches the "spanname" value of the "entry"
(define (CALS-ENTRY-SPANSPEC-START)
  (child-number
    (GI-ATTVAL-NODE-IN-NODELIST "COLSPEC"  "colname"
      (attribute-string "namest" 
        (GI-ATTVAL-NODE-IN-NODELIST "SPANSPEC" "spanname"
          (attribute-string "spanname") (children (ancestor "tgroup"))))
      (children (ancestor "tgroup")))))

; find the child number of the "colspec" that has a "colname" attribute
; value matching the "namest" attribute value of the "entry"
(define (CALS-ENTRY-COLSPEC-NAMEST-NODE)
  (GI-ATTVAL-NODE-IN-NODELIST "COLSPEC" "colname"              
    (attribute-string "namest") (children (ancestor "tgroup"))))
(define (CALS-ENTRY-COLSPEC-NAMEST)
  (child-number (CALS-ENTRY-COLSPEC-NAMEST-NODE)))

; calculate the spane information from "namest" and "nameend" on
; the "entry"   
(define (CALS-ENTRY-COLSPEC-NAMEEND-NODE)
  (GI-ATTVAL-NODE-IN-NODELIST "COLSPEC" "colname"              
    (attribute-string "nameend") (children (ancestor "tgroup"))))
(define (CALS-ENTRY-NAMEST-NAMEEND-NUMCOLS)
  (if (CALS-ENTRY-COLSPEC-NAMEEND-NODE)
     (+ 1
        (- (child-number (CALS-ENTRY-COLSPEC-NAMEEND-NODE))
           (child-number (CALS-ENTRY-COLSPEC-NAMEST-NODE))
     ))
     1))

; find the child number of the "colspec" that has a "colname" attribute
; value matching the "colname" attribute value of the "entry"
(define (CALS-ENTRY-COLSPEC-NAME-NODE)
  (GI-ATTVAL-NODE-IN-NODELIST "COLSPEC" "colname"               
    (attribute-string "colname") (children (ancestor "tgroup"))))
(define (CALS-ENTRY-COLSPEC-NAME)
  (child-number (CALS-ENTRY-COLSPEC-NAME-NODE)))

; find the number of columns spanned by calculating the difference in child
; number of the "colspec"s that have a "colname" attribute whose value
; matches the "nameend" and "namest" attribute values of the "spanspec"
; whose "spannane" attribute value matches the "spanname" value of the "entry"
(define (CALS-ENTRY-SPANSPEC-NUMCOLS)
  (let ((spanspec-node 
          (GI-ATTVAL-NODE-IN-NODELIST "SPANSPEC" "spanname"
            (attribute-string "spanname") (children (ancestor "tgroup")))))
    (+ 1
       (- (child-number
            (GI-ATTVAL-NODE-IN-NODELIST "COLSPEC"  "colname"
              (attribute-string "nameend" spanspec-node)
              (children (ancestor "tgroup"))))
          (child-number
            (GI-ATTVAL-NODE-IN-NODELIST "COLSPEC"  "colname"
              (attribute-string "namest" spanspec-node)
              (children (ancestor "tgroup"))))))))

; find a "colspec" for a separator
(define (CALS-ENTRY-FIND-SEP-COLSPEC)
  (cond ((attribute-string "spanname") 
         (let ((spanspec-node 
               (GI-ATTVAL-NODE-IN-NODELIST "SPANSPEC" "spanname"
               (attribute-string "spanname") (children (ancestor "tgroup")))))
              (GI-ATTVAL-NODE-IN-NODELIST "COLSPEC"  "colname"
                (attribute-string "nameend" spanspec-node)
                (children (ancestor "tgroup")))))
        ((attribute-string "namest")   
         (CALS-ENTRY-COLSPEC-NAMEST-NODE))
        ((attribute-string "colname")  
         (CALS-ENTRY-COLSPEC-NAME-NODE))    
        (else #f)))

; find a "rowsep" specification by looking - in order - at
; "entry", "row", "colspec", "tgroup", "table"
(define (CALS-ENTRY-FIND-ROWSEP)
  (if (attribute-string "rowsep")
    (string->number (attribute-string "rowsep"))
    (if (attribute-string "rowsep" (ancestor "row"))
      (string->number (attribute-string "rowsep" (ancestor "row")))
      (let ((col-spec-node (CALS-ENTRY-FIND-SEP-COLSPEC)))
        (if (and col-spec-node (attribute-string "rowsep" col-spec-node))
          (string->number (attribute-string "rowsep" col-spec-node))
          (if (attribute-string "rowsep" (ancestor "tgroup"))
            (string->number (attribute-string "rowsep" (ancestor "tgroup")))
            (if (attribute-string "rowsep" (ancestor "table"))
              (string->number (attribute-string "rowsep" (ancestor "table")))
              %cals-rule-default%)))))))
; set up a value for the row separator - no distinction for values > 0
; if no rowsep found then use default separator
(define (CALS-ENTRY-ROWSEP)
  (let ((rowsep-value (CALS-ENTRY-FIND-ROWSEP)))
    (if (> rowsep-value 0)
       #t
       #f)))

; find a "rowsep" specification by looking - in order - at
; current "colspec", "tgroup", "table"
(define (CALS-COLSPEC-FIND-ROWSEP)
  (if (attribute-string "rowsep")
    (string->number (attribute-string "rowsep"))
    (if (attribute-string "rowsep" (ancestor "tgroup"))
      (string->number (attribute-string "rowsep" (ancestor "tgroup")))
      (if (attribute-string "rowsep" (ancestor "table"))
        (string->number (attribute-string "rowsep" (ancestor "table")))
        %cals-rule-default%))))
; set up a value for the row separator - no distinction for values > 0
; if no rowsep found then use default separator
(define (CALS-COLSPEC-ROWSEP)
  (let ((rowsep-value (CALS-COLSPEC-FIND-ROWSEP)))
    (if (> rowsep-value 0)
       #t
       #f)))

; find a "colsep" specification by looking - in order - at
; "entry", "colspec", "tgroup", "table"
(define (CALS-ENTRY-FIND-COLSEP)
  (if (attribute-string "colsep")
    (string->number (attribute-string "colsep"))
    (let ((col-spec-node (CALS-ENTRY-FIND-SEP-COLSPEC)))
      (if (and col-spec-node (attribute-string "colsep" col-spec-node))
        (string->number (attribute-string "colsep" col-spec-node))
        (if (attribute-string "colsep" (ancestor "tgroup"))
          (string->number (attribute-string "colsep" (ancestor "tgroup")))
          (if (attribute-string "colsep" (ancestor "table"))
            (string->number (attribute-string "colsep" (ancestor "table")))
            %cals-rule-default%))))))
; set up a value for the column separator - no distinction for values > 0
; if no colsep found then use default separator
(define (CALS-ENTRY-COLSEP)
  (let ((colsep-value (CALS-ENTRY-FIND-COLSEP)))
    (if (> colsep-value 0)
       #t
       #f)))

; find a "colsep" specification by looking - in order - at
; current "colspec", "tgroup", "table"
(define (CALS-COLSPEC-FIND-COLSEP)
  (if (attribute-string "colsep")
    (string->number (attribute-string "colsep"))
    (if (attribute-string "colsep" (ancestor "tgroup"))
      (string->number (attribute-string "colsep" (ancestor "tgroup")))
      (if (attribute-string "colsep" (ancestor "table"))
        (string->number (attribute-string "colsep" (ancestor "table")))
        %cals-rule-default%))))
; set up a value for the row separator - no distinction for values > 0
; if no colsep found then use default separator
(define (CALS-COLSPEC-COLSEP)
  (let ((colsep-value (CALS-COLSPEC-FIND-COLSEP)))
    (if (> colsep-value 0)
       #t
       #f)))

; find a "valign" specification by looking - in order - at
; "entry", "tbody", "thead", "tfoot"
(define (CALS-ENTRY-FIND-VALIGN)
  (if (attribute-string "valign")
    (string->number (attribute-string "valign"))
      (if (attribute-string "valign" (ancestor "tbody"))
        (string->number (attribute-string "valign" (ancestor "tbody")))
        (if (attribute-string "valign" (ancestor "thead"))
          (string->number (attribute-string "valign" (ancestor "thead")))
          (if (attribute-string "valign" (ancestor "tfoot"))
            (string->number (attribute-string "valign" (ancestor "tfoot")))
            %cals-valign-default%)))))
; set up a value for the row alignment
(define (CALS-ENTRY-VALIGN)
  (let ((valign-value (CALS-ENTRY-FIND-VALIGN)))
    (case valign-value
       (("TOP") 'start)
       (("MIDDLE") 'center)
       (("BOTTOM") 'end)
       (else 'start))))

(element TGROUP
  (let ((frame-attribute (inherited-attribute-string "frame")))
    (make table
	  before-row-border:  (if frame-attribute
				  (case frame-attribute
					(("ALL") #t)
					(("SIDES") #f)
					(("TOP") #t)
					(("BOTTOM") #f)
					(("TOPBOT") #t)
					(("NONE") #f)
					(else #f))
				(if (> %cals-rule-default% 0)
				    #t
				  #f)) 
	  after-row-border:   (if frame-attribute
				  (case frame-attribute
					(("ALL") #t)
					(("SIDES") #f)
					(("TOP") #f)
					(("BOTTOM") #t)
					(("TOPBOT") #t)
					(("NONE") #f)
					(else #f))
				(if (> %cals-rule-default% 0)
				    #t
				  #f)) 
	  before-column-border: (if frame-attribute
				    (case frame-attribute
					  (("ALL") #t)
					  (("SIDES") #t)
					  (("TOP") #f)
					  (("BOTTOM") #f)
					  (("TOPBOT") #f)
					  (("NONE") #f)
					  (else #f))
				  (if (> %cals-rule-default% 0)
				      #t
				    #f)) 
	  after-column-border:  (if frame-attribute
				    (case frame-attribute
					  (("ALL") #t)
					  (("SIDES") #t)
					  (("TOP") #f)
					  (("BOTTOM") #f)
					  (("TOPBOT") #f)
					  (("NONE") #f)
					  (else #f))
				  (if (> %cals-rule-default% 0)
				      #t
				    #f)) 
	  (make table-part
		content-map: '((thead header)
			       (tbody #f)
			       (tfoot footer))
		(process-children)))))

(element COLSPEC 
  (make table-column
        cell-after-column-border: (CALS-COLSPEC-COLSEP)
        cell-after-row-border: (CALS-COLSPEC-ROWSEP)
        width: (CALS-COLSPEC-UNIT (attribute-string "colwidth"))))

(element THEAD
  (make sequence
        label: 'thead))

(element TFOOT
  (make sequence
        label: 'tfoot))

(element TBODY
  (make sequence
        label: 'tbody))

(element ROW
  (if (attribute-string "rowsep")
    (make table-row
          cell-after-row-border: (let ((rowsep-value (string->number (attribute-string "rowsep"))))
                                   (if (> rowsep-value 0)
                                     #t
                                     #f))
          (process-children-trim))
    (make table-row
          (process-children-trim))))

(element ENTRY
  (if (attribute-string "spanname")
    (make table-cell
          column-number: (CALS-ENTRY-SPANSPEC-START)
          n-columns-spanned: (CALS-ENTRY-SPANSPEC-NUMCOLS)
	  n-rows-spanned: (let ((morerows-value (attribute-string "morerows")))
			    (if morerows-value
				(+ 1 (string->number morerows-value))
			      1))
          cell-row-alignment: (CALS-ENTRY-VALIGN)
          cell-after-column-border: (CALS-ENTRY-COLSEP)
          cell-after-row-border: (CALS-ENTRY-ROWSEP)
          cell-before-row-margin: %cals-cell-before-row-margin%
          cell-after-row-margin: %cals-cell-after-row-margin%
          cell-before-column-margin: %cals-cell-before-column-margin%
          cell-after-column-margin: %cals-cell-after-column-margin%
          start-indent: %cals-cell-content-start-indent%
          end-indent: %cals-cell-content-end-indent%
          (process-children-trim))
    (if (attribute-string "namest")
      (make table-cell
            column-number: (CALS-ENTRY-COLSPEC-NAMEST)
            n-columns-spanned: (CALS-ENTRY-NAMEST-NAMEEND-NUMCOLS)
	    n-rows-spanned: (let ((morerows-value (attribute-string "morerows")))
			      (if morerows-value
				  (+ 1 (string->number morerows-value))
				1))
            cell-row-alignment: (CALS-ENTRY-VALIGN)
            cell-after-column-border: (CALS-ENTRY-COLSEP)
            cell-after-row-border: (CALS-ENTRY-ROWSEP)
            cell-before-row-margin: %cals-cell-before-row-margin%
            cell-after-row-margin: %cals-cell-after-row-margin%
            cell-before-column-margin: %cals-cell-before-column-margin%
            cell-after-column-margin: %cals-cell-after-column-margin%
            start-indent: %cals-cell-content-start-indent%
            end-indent: %cals-cell-content-end-indent%
            (process-children-trim))
      (if (attribute-string "colname")
        (make table-cell
              column-number: (CALS-ENTRY-COLSPEC-NAME)
	      n-rows-spanned: (let ((morerows-value (attribute-string "morerows")))
				(if morerows-value
				    (+ 1 (string->number morerows-value))
				  1))
              cell-row-alignment: (CALS-ENTRY-VALIGN)
              cell-after-column-border: (CALS-ENTRY-COLSEP)
              cell-after-row-border: (CALS-ENTRY-ROWSEP)
              cell-before-row-margin: %cals-cell-before-row-margin%
              cell-after-row-margin: %cals-cell-after-row-margin%
              cell-before-column-margin: %cals-cell-before-column-margin%
              cell-after-column-margin: %cals-cell-after-column-margin%
              start-indent: %cals-cell-content-start-indent%
              end-indent: %cals-cell-content-end-indent%
              (process-children-trim))

        (make table-cell
	      n-rows-spanned: (let ((morerows-value (attribute-string "morerows")))
				(if morerows-value
				    (+ 1 (string->number morerows-value))
				  1))
              cell-row-alignment: (CALS-ENTRY-VALIGN)
              cell-after-column-border: (CALS-ENTRY-COLSEP)
              cell-after-row-border: (CALS-ENTRY-ROWSEP)
              cell-before-row-margin: %cals-cell-before-row-margin%
              cell-after-row-margin: %cals-cell-after-row-margin%
              cell-before-column-margin: %cals-cell-before-column-margin%
              cell-after-column-margin: %cals-cell-after-column-margin%
              start-indent: %cals-cell-content-start-indent%
              end-indent: %cals-cell-content-end-indent%
              (process-children-trim))))))

;; bosak 1996.11.23
(element (TABLE TITLE) (empty-sosofo)) ; don't show caption below table
(element (CHART TITLE) (empty-sosofo)) ; don't show caption below chart

(element SPANSPEC (empty-sosofo))

;; ===================== END OF TABLES ===============================

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STOCK STYLESHEET PIECES ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ============================== UNITS ================================

(define-unit pi (/ 1in 6))
(define-unit pt (/ 1in 72))
(define-unit px (/ 1in 96))

;; "em" is defined in the parameters section of the stylesheet

;; ========================= COMMON FUNCTIONS ===========================

(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

;; per ISO/IEC 10179
(define (node-list-reduce nl proc init)
  (if (node-list-empty? nl)
      init
      (node-list-reduce (node-list-rest nl)
                        proc
                        (proc init (node-list-first nl)))))

;; per ISO/IEC 10179
(define (node-list-length nl)
  (node-list-reduce nl
                    (lambda (result snl)
                      (+ result 1))
                    0))

(define if-front-page
  (external-procedure "UNREGISTERED::James Clark//Procedure::if-front-page"))

(define if-first-page
  (external-procedure "UNREGISTERED::James Clark//Procedure::if-first-page"))

(declare-characteristic page-number-format
   "UNREGISTERED::James Clark//Characteristic::page-number-format" "1")

(declare-characteristic page-number-restart?
   "UNREGISTERED::James Clark//Characteristic::page-number-restart?" #f)

(define upperalpha
  '(#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M
    #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z))

(define loweralpha
  '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m
    #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z))

(define (char-downcase ch)
  (case ch
	((#\A) #\a) ((#\B) #\b) ((#\C) #\c) ((#\D) #\d) ((#\E) #\e)
	((#\F) #\f) ((#\G) #\g) ((#\H) #\h) ((#\I) #\i) ((#\J) #\j)
	((#\K) #\k) ((#\L) #\l) ((#\M) #\m) ((#\N) #\n) ((#\O) #\o)
	((#\P) #\p) ((#\Q) #\q) ((#\R) #\r) ((#\S) #\s) ((#\T) #\t)
	((#\U) #\u) ((#\V) #\v) ((#\W) #\w) ((#\X) #\x) ((#\Y) #\y)
	((#\Z) #\z) (else ch)))

(define (LOWCASE slist)
  (if (null? slist)
      '()
      (cons (char-downcase (car slist)) (LOWCASE (cdr slist)))))

(define (STR2LIST s)
  (let ((len (string-length s)))
    (let loop ((i 0) (ln len))
	 (if (= i len)
	     '()
	     (cons (string-ref s i) (loop (+ i 1) ln))))))

(define (STRING-DOWNCASE s)
  (apply string (LOWCASE (STR2LIST s))))

(define (UNAME-START-INDEX u last)
  (let ((c (string-ref u last)))
    (if (or (member c upperalpha) (member c loweralpha))
	(if (= last 0)
	    0
	    (UNAME-START-INDEX u (- last 1)))
        (+ last 1))))

(define (PARSEDUNIT u) ;; this doesn't deal with "%" yet
 (if (string? u)
  (let ((strlen (string-length u)))
    (if (> strlen 2)
	(let ((u-s-i (UNAME-START-INDEX u (- strlen 1))))
	  (if (= u-s-i 0) ;; there's no number here
	      1pi         ;; so return something that might work
	      (if (= u-s-i strlen)           ;; there's no unit name here
		  (* (string->number u) 1pt) ;; so default to points
		  (let* ((unum (string->number
			       (substring u 0 u-s-i)))
			 (uname (STRING-DOWNCASE
				 (substring u u-s-i strlen))))
		    (case uname
			  (("mm") (* unum 1mm))
			  (("cm") (* unum 1cm))
			  (("in") (* unum 1in))
			  (("pi") (* unum 1pi))
			  (("pc") (* unum 1pi))
			  (("pt") (* unum 1pt))
			  (("px") (* unum 1px))
			  (("barleycorn") (* unum 2pi)) ;; extensible!
			  (else
			   (cond 
			    ((number? unum)
			     (* unum 1px))
			    ((number? (string->number u))
			     (* (string->number u) 1px))
				 (else u))))))))
        (if (number? (string->number u))
	    (* (string->number u) 1px)
	    1pi)))
    1pi))

(define (INLIST?)
  (let loop ((rest-of-list list-list))
       (if (null? rest-of-list)
	   #f
	   (if (have-ancestor? (car rest-of-list))
	       #t
	       (loop (cdr rest-of-list))))))

(define (HSIZE n)
  (let ((m (if (< n 0) 0 n)))
    (* %bf-size%
       (expt %hsize-bump-factor% m))))

(define (PARNUM)
  (child-number (parent (current-node))))

(define (NESTEDFNUM n fmt)
  (if (number? n)
      (format-number n fmt)
      #f))

(define (FNUM n) (NESTEDFNUM n "1"))

(define (BULLTREAT bullfcn ilevel override mark)
  (cond
   (override (bullfcn override ilevel))
   (mark (bullfcn mark ilevel))
   (else (bullfcn "bullet" ilevel))))

(define (BULLFONT m lvl)
  (let ((md (STRING-DOWNCASE m)))
    (case md
	  (("bullet") "WingDings")
	  (("box") "WingDings")
	  (("checkbox") "WingDings")
	  (("check") "WingDings")
	  (("checkedbox") "WingDings")
	  (("dash") %body-font-family%)
	  (("copyright") "Symbol")
	  (else %body-font-family%))))

(define (BULLSTR m lvl)
  (let ((md (STRING-DOWNCASE m)))
    (case md
	  (("bullet") "l")
	  (("box") "o")
	  (("checkbox") "o")
	  (("check") "ü")
	  (("checkedbox") "þ")
	  (("dash") "–")
	  (("copyright") "ã")
	  (("none") "")
	  (else "l"))))

(define (MSIZE m lvl f1 f2)
  (if (= lvl 1)
      (* %bf-size% f1)
      (* %bf-size% f2)))

(define (BULLSIZE m lvl)
  (let ((md (STRING-DOWNCASE m)))
    (case md
	  (("bullet") (MSIZE m lvl 0.8 0.72))
	  (("box") (MSIZE m lvl 0.9 0.72))
	  (("checkbox") (MSIZE m lvl 0.9 0.72))
	  (("check") (MSIZE m lvl 1.0 1.0))
	  (("checkedbox") (MSIZE m lvl 1.0 1.0))
	  (("dash") (MSIZE m lvl 1.0 1.0))
	  (("none") (MSIZE m lvl 1.0 1.0))
	  (else (MSIZE m lvl 1.0 1.0)))))

(define (BULLSHIFT m lvl)
  (let ((md (STRING-DOWNCASE m)))
    (case md
	  (("bullet") 0.0em)
	  (("box") (if (= lvl 1) 0.0em 0.1em))
	  (("checkbox") (if (= lvl 1) 0.0em 0.1em))
	  (("check") 0.0em)
	  (("checkedbox") 0.0em)
	  (("dash") 0.0em)
	  (("none") 0.0em)
	  (else 0.0em))))

;; ====================== COMMON STYLE TEMPLATES =======================

(define ($block-container$)
  (make display-group
	space-before: %block-sep%
	space-after: %block-sep%
	start-indent: %body-start-indent%
	(process-children)))

(define ($para-container$)
  (make paragraph
	space-before: %para-sep%
	space-after: %para-sep%
	start-indent: (if (member (current-node) outer-parent-list)
			  %body-start-indent%
			  (inherited-start-indent))
	(process-children)))

(define ($indent-para-container$)
  (make paragraph
	space-before: %para-sep%
	space-after: %para-sep%
	start-indent: (+ (inherited-start-indent) (* (ILSTEP) 2))
	quadding: 'start
	(process-children-trim)))

(define para-style
  (style
   font-size: %bf-size%
   font-weight: 'medium
   font-posture: 'upright
   font-family-name: %body-font-family%
   line-spacing: (* %bf-size% %line-spacing-factor%)))

(define title-style
  (style
   font-family-name: %title-font-family%
   font-weight: 'bold
   quadding: 'start))

(define ($lowtitle$ tlevel)
  (let ((hs (HSIZE (- 3 tlevel))))
    (make paragraph
	  font-family-name: %title-font-family%
	  font-weight: 'bold
	  font-size: hs
	  line-spacing: (* hs %line-spacing-factor%)
	  space-before: (* hs %head-before-factor%)
	  space-after: (* hs %head-after-factor%)
	  start-indent: %body-start-indent%
	  quadding: 'start
	  keep-with-next?: #t
	  (process-children))))

(define ($runinhead$)
  (make sequence
	font-weight: 'bold
	(process-children)
	(literal "  ")))

(define ($bold-seq$)
  (make sequence
    font-weight: 'bold
    (process-children-trim)))

(define ($italic-seq$)
  (make sequence
    font-posture: 'italic
    (process-children-trim)))

(define ($bold-italic-seq$)
  (make sequence
    font-weight: 'bold
    font-posture: 'italic
    (process-children-trim)))

(define ($mono-seq$)
  (make sequence
	font-family-name: %mono-font-family%
	(process-children-trim)))

(define ($italic-mono-seq$)
  (make sequence
	font-family-name: %mono-font-family%
	font-posture: 'italic
	(process-children-trim)))

(define ($bold-mono-seq$)
  (make sequence
	font-family-name: %mono-font-family%
	font-weight: 'bold
	(process-children-trim)))

(define ($score-seq$ stype)
  (make score
	type: stype
	(process-children-trim)))

(define ($charseq$) (process-children))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; END OF STOCK STYLESHEET PIECES ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

