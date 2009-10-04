
(define-solid-paint-style black ()
  color: 'black)

(define-solid-paint-style white ()
  color: 'white)

(define-solid-paint-style light-blue ()
  color: '(cmyk 0.333 0.333 0 0))

(define-stroke-style default-stroke ()
  paint: 'black
  linewidth: 0.5
  linejoin: 'miter
  linecap: 'butt
  miterlimit: 2)

(define-stroke-style screen-stroke (default-stroke))

(define-font-style  $default-font ()
  family: "Times" 
  angle: 'normal
  weight: 'normal
  width: 'normal
  variation: 'none
  size: 12
  stretch: 0)

(define-font-style symbol-font ($default-font)
  family: "Symbol"
  size: 10)

(define-font-style dingbat-font ($default-font)
  family: "ZapfDingbats"
  size: 8)

(define-font-style body-font ($default-font)
  family: "Minion"
  width: 'condensed
  size: 10)

(define-font-style emphasis-font (body-font)
  angle: 'italic)

(define-font-style bold-font (body-font)
  weight: 'bold)

(define-font-style tt-font ($default-font)
  family: "BriemMono"
  width: 'condensed
  size: 7)
(define-font-style proto-font (tt-font )
  weight: 'bold)
(define-font-style arg-font (body-font )
  angle: 'italic)

(define-font-style comment-font (body-font)
  family: "Helvetica"
  weight: 'normal
  width: 'normal)

(define-font-style title-font (body-font)
  family: "Univers"
  weight: 'normal
  width: 'ultra-condensed)

(define-font-style chapter-title-font (title-font) 
  size: 18)

(define-font-style title1-font (title-font)
  size: 14)

(define-font-style title2-font (title-font)
  size: 12)

(define-font-style title3-font (title-font)
 size: 10)

(define-font-style paratitle-font (title-font )
  size: 8
  weight: 'normal)


(define-character-style $default-char-style  ()
  font: '$default-font
  color: 'black
  spread: 0
  underline: 'none
  overline: 'none
  strikethrough: 'none
  baseline-shift: 0
  smallcaps: #f
  kerning?: #t
  annotation: #f)

(define-character-style chapter-title-char-style   ($default-char-style)
  font: 'chapter-title-font)
(define-character-style title1-char-style ($default-char-style)
  font: 'title1-font)
(define-character-style title2-char-style ($default-char-style)
  font: 'title2-font)
(define-character-style title3-char-style ($default-char-style)
  font: 'title3-font)
(define-character-style paratitle-char-style ($default-char-style)
  font: 'paratitle-font)
                                            

(define-character-style comment-char-style ($default-char-style)
  font: 'comment-font
)

(define-paragraph-style $default-para-style  ()
  ;;
  lines: 'wrap
  default-char-style: 'body-char-style
  ;;
  left-margin-first: 0
  left-margin: 0
  right-margin-last: 0                ; relative to right frame edge
  right-margin: 0                     ; relative to right frame edge
  tab-stops: '((position: (l 24) align: left)
               (position: (l 60) align: left)
               (position: (l 96) align: #\.)
               (position: (l 108) align: right)
               (position: (l 111) align: left)
               (position: (r 12) align: right leading: "."))
  ;;
  space-before: 0
  space-after: 0
  line-spacing: 'using-font-size
  line-spacing-fixed?: #f
  align: 'left                         ; (left right center justified)
  ;;
  ;;  pagination control
  ;;
  start: 'anywhere                     ; (column page left-page right-page)
  keep-with-next?: #f
  keep-with-previous?: #f
  widow-control: 2                     ; column have at least this many lines
  placement: 'in-column                ; (in-column
                                        ;  run-in
                                        ;  sidebar/first-baseline
                                        ;  sidebar/first-baseline*
                                        ;  sidebar/top-edge
                                        ;  sidebar/last-baseline
                                        ;  across-columns
                                        ;  across-frame)
  )

(define-paragraph-style body-para-style  ($default-para-style)
  line-spacing: 14
  space-after: 6)

(define-paragraph-style bookinfo-para  (body-para-style)
)

(define-paragraph-style screen-style  ($default-para-style)
  lines: 'asis-wrap         ; c.f. DSSSL p.218
  default-char-style: 'literal-char-style
  line-spacing: 8
  left-margin-first: 18
  left-margin: 18
  right-margin: 18
  right-margin-last: 18)

(define-paragraph-style blockquote-para-style  ($default-para-style)
  left-margin-first: 36
  align: 'left                 ; prefer: 'justified
  left-margin: 36
  right-margin: 36
  right-margin-last: 36)

(define-paragraph-style attribution-para-style  ($default-para-style)
  left-margin-first: 2in
  left-margin: 2in
  align: 'right
  right-margin: 36
  right-margin-last: 36
  default-char-style: 'emphasis-char-style)

(define-paragraph-style chapter-title-para-style  ($default-para-style)
  space-after: 0
  tab-stops: '((position: (r 0) align: right))
  default-char-style: 'chapter-title-char-style)

(define-paragraph-style title1-para-style  ($default-para-style)
  space-before: 0
  right-margin: 1in
  right-margin-last: 1in
  tab-stops: '((position: (l 1.5in) align: right)
               (position: (l 1.75in) align: left))
  placement: 'sidebar/first-baseline
  default-char-style: 'title1-char-style)

(define-paragraph-style conformance-para-style  ($default-para-style)
  space-before: 7
  right-margin: 0.25in
  right-margin-last: 0.25in
  tab-stops: '((position: (l 1.5in) align: right))
  placement: 'sidebar/first-baseline*
  default-char-style: 'prototype-char-style)
  

(define-paragraph-style title2-para-style  ($default-para-style)
  space-before: 6
  default-char-style: 'title2-char-style)

(define-paragraph-style title3-para-style  ($default-para-style)
  space-before: 6
  default-char-style: 'title3-char-style)

(define-paragraph-style paratitle-para-style  (body-para-style)
  default-char-style: 'paratitle-char-style)


(define-character-style body-char-style ($default-char-style)
  font: 'body-font)

(define-character-style symbol-char-style ($default-char-style)
  font: 'symbol-font
  kerning?: #f)

(define-character-style emphasis-char-style ($default-char-style)
  font: 'emphasis-font)

(define-character-style bold-char-style ($default-char-style)
  font: 'bold-font)

(define-character-style literal-char-style ($default-char-style)
  font: 'tt-font
  kerning?: #f
  spread: -10)

(define-character-style dingbat-char-style ($default-char-style)
  font: 'dingbat-font
  kerning?: #f)

(define-paragraph-style chapter-releaseinfo-para-style  ($default-para-style)
  tab-stops: '((position: (r 0) align: right))
  default-char-style: (override-style
                       'literal-char-style
                       font: (override-style 
                              'tt-font
                              size: 7)))
                   

;;; XXX need ability to override font characteristics at char-style level!
(define-character-style prompt-char-style (literal-char-style)
  font: 'proto-font
  kerning?: #f
  spread: -10)

(define-character-style prototype-char-style ($default-char-style)
  font: 'proto-font
  kerning?: #f
  spread: -10)
(define-character-style prototype-symbol-style ($default-char-style)
  kerning?: #f
  font: 'symbol-font)
(define-character-style argument-char-style ($default-char-style)
  font: 'arg-font)
(define-character-style identifier-char-style ($default-char-style)
  kerning?: #f
  font: 'proto-font)
(define-character-style body-super-char-style (body-char-style)
  baseline-shift: 75)

;;;

;;;



(define-paragraph-style table-header-style  (body-para-style)
  default-char-style: (override-style
                       'body-char-style
                       font: (override-style
                              'body-font
                              weight: 'bold)))

(define-paragraph-style table-title-style  ($default-para-style)
  default-char-style: (override-style
                       'body-char-style
                       font: (override-style
                              'body-font
                              size: 9))
  left-margin-first: 0
  left-margin: 48
  right-margin: 48                 ; XXX these aren't working
  right-margin-last: 48            ; XXX these aren't working
  space-after: 4
  tab-stops: '((position: (l 44) align: right)
               (position: (l 48) align: left)))


;;;


;;;

(define (title-front-page)
  (list
   (make <text-frame>
         flow: 't
         frame: (make-rect 1in 4in 6.5in 6in))
   (make <text-frame>
         flow: 't
         frame: (make-rect 1.25in 0in 6in 0.8in))
   #|
   (make <text-box>
         frame: (make-rect 1.25in 1in 6in 0.8in)
         style: 'body-char-style
         align: 'left
         content: '("This document is being submitted on a confidential basis,"))
   (make <text-box>
         frame: (make-rect 1.25in (- 72 8) 6in 0.8in)
         style: 'body-char-style
         align: 'left
         content: '("and the contents are the trade secrets of Xynthesis LLC.  It may not be reproduced, stored or copied in any form without express permission."))
   |#

   #|
   (make <script-graphic>
         frame: (make-rect 1.25in 0.5in 6in 0.3in)
         script: 'rectangle
         argv: '(stroke-color: black
                 line-width: 0.5
                 fill-color: #f))
   |#
   ))

;;;
(define $footer-rule-y 0.75in)
(define $header-rule-y 10.25in)

(define (chapter-start-page)
  (list
   ;; this frame is where the title will go (with a "\t" prefix)
   (make <text-frame>
         flow: 'a
         frame: (make-rect 1in 8.75in 6.75in 1.25in))
   ;; this frame is where the release info will go
   (make <text-frame>
         flow: 'a
         frame: (make-rect 1in 575 6.75in 0.4in))
   ;; this is the body frame
   (make <text-frame>
         flow: 'a
         frame: (make-rect 1in 1in 6.75in 7in)
         num-columns: 1
         primary-sidebar: 'left
         primary-sidebar-width: 1.5in
         sidebar-gap: 0.25in)
   (make <line-graphic>
         line-start: (make-point 1in 8.25in)
         line-end: (make-point 7.75in 8.25in))
   (make <line-graphic>
         line-start: (make-point 1in $footer-rule-y)
         line-end: (make-point 7.75in $footer-rule-y))
   (make <text-box>
         frame: (make-rect 7in 0.5in 0.75in 0.25in)
         content: '((page-number))
         style: 'emphasis-char-style
         align: 'right)))



   
(define-page-style landscape-table-page  (letter-landscape)
  content: (let ((b (trim-rect (media-box 'letter-landscape)
                               left: 0.75in
                               top: 0.75in
                               right: 0.75in
                               bottom: 1in)))
             (list
              ;; main text frame
              (make <text-frame>
                    flow: 'a
                    frame: b)
              ;;
              (make <line-graphic>
                    line-start: (point+ (upper-left b) 
                                        (make-size 0 0.25in))
                    line-end: (point+ (upper-right b)
                                      (make-size 0 0.25in)))
              ;;
              (make <line-graphic>
                    line-start: (point+ (lower-left b)
                                        (make-size 0 -0.25in))
                    line-end: (point+ (lower-right b)
                                      (make-size 0 -0.25in))))))
               
                                      
                                      
     
(define (chapter-right-page)
  (list
   ;; main text frame
   (make <text-frame>
         flow: 'a
         frame: (make-rect 1in 1in 6.75in 9in)
         num-columns: 1
         primary-sidebar: 'left
         primary-sidebar-width: 1.5in
         sidebar-gap: 0.25in)
   ;; R header
   (make <line-graphic>
         line-start: (make-point 1in $header-rule-y)
         line-end: (make-point 7.75in $header-rule-y))
   (make <text-box>
         frame: (make-rect 4.75in 10.4in 3in 0.25in)
         align: 'right
         style: 'emphasis-char-style
         content: '((section-page-header)))
   ;; R footer
   (make <line-graphic>
         line-start: (make-point 1in $footer-rule-y)
         line-end: (make-point 7.75in $footer-rule-y))
   (make <text-box>
         frame: (make-rect 7in 0.5in 0.75in 0.25in)
         content: '((page-number))
         style: 'emphasis-char-style
         align: 'right)
   (make <text-box>
         frame: (make-rect 1in 0.5in 3in 0.25in)
         align: 'left
         style: 'emphasis-char-style
         content: '((ref date)))))

(define (chapter-left-page)
  (list
   ;; main text frame
   (make <text-frame>
         flow: 'a
         frame: (make-rect 0.75in 1in 6.75in 9in)
         num-columns: 1
         primary-sidebar: 'left
         primary-sidebar-width: 1.5in
         sidebar-gap: 0.25in)
   ;; L header
   (make <line-graphic>
         line-start: (make-point 0.75in $header-rule-y)
         line-end: (make-point 7.5in $header-rule-y))
   (make <text-box>
         frame: (make-rect 0.75in 10.4in 3in 0.25in)
         align: 'left
         style: 'emphasis-char-style
         content: '((section-page-header)))
   ;; L footer
   (make <line-graphic>
         line-start: (make-point 0.75in $footer-rule-y)
         line-end: (make-point 7.5in $footer-rule-y))
   (make <text-box>
         frame: (make-rect 0.75in 0.5in 0.75in 0.25in)
         content: '((page-number))
         style: 'emphasis-char-style
         align: 'left)
   (make <text-box>
         frame: (make-rect 4.5in 0.5in 3in 0.25in)
         content: '((ref version))
         style: 'emphasis-char-style
         align: 'right)))

(define-page-style title-front-page (letter)
  content: (title-front-page)
  numbering-format: 'roman
  numbering-group: 'preface
  page-start: 'recto)

(define-page-style title-back-page   (letter)
  numbering-format: 'roman
  numbering-group: 'preface
  content: (list
            (make <text-frame>
                  flow: 't
                  frame: (make-rect 0.75in 1in 6.75in 9in))
            ;; L footer
            (make <line-graphic>
                  line-start: (make-point 0.75in $footer-rule-y)
                  line-end: (make-point 7.5in $footer-rule-y))
            (make <text-box>
                  frame: (make-rect 0.75in 0.5in 0.75in 0.25in)
                  content: '((page-number))
                  style: 'emphasis-char-style
                  align: 'left)
            (make <text-box>
                  frame: (make-rect 4.5in 0.5in 3in 0.25in)
                  content: '((ref version))
                  style: 'emphasis-char-style
                  align: 'right)))

(define-page-style chapter-start-page (letter)
  content: (chapter-start-page)
  page-start: 'recto)

(define-page-style chapter-left-page (letter)
  content: (chapter-left-page))

(define-page-style chapter-right-page (letter)
  content: (chapter-right-page))

(define-page-style intentionally-blank-page-plain  (letter)
  content: (list
            (make <text-box>
                  frame: (make-rect 0 5.4in 8.5in 0.5in)
                  content: '("(This Page Intentionally Left Blank)")
                  align: 'center
                  style: 'emphasis-char-style)
            ;; footer
            (make <line-graphic>
                  line-start: (make-point 0.75in $footer-rule-y)
                  line-end: (make-point 7.5in $footer-rule-y))
            (make <text-box>
                  frame: (make-rect 0.75in 0.5in 0.75in 0.25in)
                  content: '((page-number))
                  style: 'emphasis-char-style
                  align: 'left)
            (make <text-box>
                  frame: (make-rect 4.5in 0.5in 3in 0.25in)
                  content: '((ref version))
                  style: 'emphasis-char-style
                  align: 'right)))

(define-page-style intentionally-blank-page-body  (intentionally-blank-page-plain)
  numbering-group: 'body
  numbering-format: 'arabic)

(define-page-style intentionally-blank-page-preface  (intentionally-blank-page-plain)
  numbering-group: 'preface
  numbering-format: 'roman)


(define (make-chapter-label)
  (make <flow-vbox>
        height: 33
        render-proc: (lambda (self width dev)
                       (render-chapter-number-block dev width))))

(define (render-chapter-number-block dev width)
  (with-gstate-saved
   dev
   (lambda ()
     (let* ((f (get-text-font "Palatino" "Bold" 36))
            (ch (page->chapter (current-page dev)))
            (cn (name ch))
            (w (string-width f cn)))
       (translate dev (make-point (- width w 6) 0))
       (setfont dev (get-text-font "Helvetica" "Bold" 9))
       (with-gstate-saved
        dev
        (lambda ()
          (rotate dev 90)
          (moveto dev (make-point 1 2))
          (case (type ch) 
            ((chapter) 
             (scale dev 0.95 1)
             (show dev "Chapter"))
            ((appendix) 
             (scale dev 0.78 1)
             (show dev "Appendix")))))
       ;;
       (setcolor dev (device-color dev 'black))
       (rectfill dev (make-rect 1 0 (+ 5 w) 33))
       ;;
       (setcolor dev (device-color dev 'white))
       ;;
       (setfont dev f)
       (moveto dev (make-point 3 5))
       (show dev cn)))))

;;;


;;;
;;;  Style support for refentrys
;;;

(define-paragraph-style refnamediv-name-style (body-para-style)
  placement: 'sidebar/first-baseline
  line-spacing: 12
  space-after: 0
  tab-stops: '((position: (l 1.5in) align: right)
               (position: (r 0) align: right)))

(define-paragraph-style refnamesect-title-style  (body-para-style)
  placement: 'sidebar/first-baseline
  default-char-style: (override-style
                       'title3-char-style
                       font: (override-style 'title3-font
                                             size: 8))
  line-spacing: 8
  space-after: 0
  tab-stops: '((position: (l 1.5in) align: left)
               (position: (r 0) align: right)))

(define-paragraph-style refnamesect-title2-style  (body-para-style)
  default-char-style: (override-style
                       'title3-char-style
                       font: (override-style 'title3-font
                                             size: 8))
  line-spacing: 8
  space-after: 0)

(define-paragraph-style refnamediv-style (body-para-style)
  line-spacing: 12
  space-after: 0
  tab-stops: '((position: (r 0) align: right)))

(define-paragraph-style synopsis-style (body-para-style)
  line-spacing: 12
  space-before: 0
  space-after: 2
  left-margin-first: 0
  left-margin: 36
  tab-stops: '((position: (l 3.5in)
                          align: right)
               (position: (l 3.6in)
                          align: left)))
                                            

(define-character-style synopsis-char-style  (literal-char-style)
)

(define-character-style synopsis-head-char-style (synopsis-char-style)
  ;weight: 'bold
  font: 'proto-font)

(define-character-style synopsis-param-char-style  (argument-char-style)
)

(define-character-style synopsis-symbol-char-style  (synopsis-char-style)
  font: 'symbol-font)
                   
(define-character-style listterm-char-style  (emphasis-char-style)
)

;;;
;;;  style support for <EXAMPLE>

(define-paragraph-style example-para-style  (body-para-style)
  line-spacing: 12
  left-margin-first: 18
  left-margin: 18
  space-after: 0
  tab-stops: '((position: (l 3in) align: right)
               (position: (l 3.1in) align: left)))

;;;
;;;  style support for a <PART>'s title page
;;;


(define-stroke-style heavy-stroke (default-stroke)
  line-width: 3)

(define-font-style part-title-font   (title-font )
  ;family: "Times"
  ;variation: 'none
  ;angle: 'italic
  size: 72)

(define-character-style part-title-char-style   ($default-char-style)
  font: 'part-title-font)

(define-character-style part-number-char-style   ($default-char-style)
  color: 'white
  font: (override-style 'part-title-font
                        angle: 'normal))

(define-paragraph-style part-title-para-style   ($default-para-style)
  default-char-style: 'part-title-char-style
  align: 'right
  line-spacing: 1in
  tab-stops: '((position: (r 0) align: right)))
                   

(define-page-style part-first-page  (letter)
  content: (list
            (make <text-frame>
                  flow: 'a
                  frame: (make-rect 2.25in 3.75in 5.5in 6.5in))
            (make <script-graphic>
                  frame: (make-rect 1in 9in 1in 1in)
                  script: 'part-label-graphic
                  argv: '(char-style: part-number-char-style)))
  page-start: 'recto)

;;;
;;;  Style support for title page
;;;

(define-character-style revision-char-style  (title3-char-style)
  font: (override-style 'title3-font
                        size: 12))

(define-character-style book-title-char-style  (body-char-style)
  font: (override-style 'body-font size: 36))

(define-character-style book-subtitle-char-style  (body-char-style)
  font: (override-style 'body-font size: 24))

(define-character-style book-author-char-style  (body-char-style)
  font: (override-style 'body-font size: 14))

(define-paragraph-style book-title-para  ($default-para-style)
  default-char-style: 'book-title-char-style
  line-spacing: 'using-font-size
  tab-stops: '((position: (l 3.25in) align: center)))

(define-paragraph-style book-subtitle-para  (book-title-para)
  default-char-style: 'book-subtitle-char-style)

(define-paragraph-style book-author-para  (book-title-para)
  default-char-style: 'book-author-char-style)

(define-paragraph-style revision-para  (book-title-para)
  default-char-style: 'revision-char-style
  left-margin-first: 6
  right-margin: 6
  line-spacing: 14
  right-margin-last: 6
  tab-stops: '((position: (l 3in) align: center)
               (position: (r 6) align: right)))

(define-paragraph-style revtable-header-para  (body-para-style)
  tab-stops: '((position: (c 0) align: center))
  default-char-style: 'bold-char-style)

(define-paragraph-style revtable-entry-para  (body-para-style)
)

;;;
;;;  Style support for table of contents
;;;

(define-page-style toc-right-page  (letter)
  numbering-format: 'roman
  numbering-group: 'preface
  page-start: 'recto
  content: (list
            (make <text-frame>
                  flow: 'toc
                  frame: (make-rect 1in 1in 6.75in 9in)
                  num-columns: 1
                  primary-sidebar: 'left
                  primary-sidebar-width: 1.5in
                  sidebar-gap: 0.25in)
            (make <line-graphic>
                  line-start: (make-point 1in 0.75in)
                  line-end: (make-point 7.75in 0.75in))
            (make <text-box>
                  frame: (make-rect 7in 0.5in 0.75in 0.25in)
                  content: '((page-number))
                  style: 'emphasis-char-style
                  align: 'right)
            (make <text-box>
                  frame: (make-rect 1in 0.5in 3in 0.25in)
                  align: 'left
                  style: 'emphasis-char-style
                  content: '((ref date)))))

(define-page-style toc-left-page  (letter)
  numbering-format: 'roman
  numbering-group: 'preface
  page-start: 'verso
  content: (list
            (make <text-frame>
                  flow: 'toc
                  frame: (make-rect 0.75in 1in 6.75in 9in)
                  num-columns: 1
                  primary-sidebar: 'left
                  primary-sidebar-width: 1.5in
                  sidebar-gap: 0.25in)
            (make <line-graphic>
                  line-start: (make-point 0.75in 0.75in)
                  line-end: (make-point 7.5in 0.75in))
            (make <text-box>
                  frame: (make-rect 0.75in 0.5in 0.75in 0.25in)
                  content: '((page-number))
                  style: 'emphasis-char-style
                  align: 'left)
            (make <text-box>
                  frame: (make-rect 4.5in 0.5in 3in 0.25in)
                  content: '((ref version))
                  style: 'emphasis-char-style
                  align: 'right)))

(define-character-style toc-metatitle-char-style  (title1-char-style)
)

(define-character-style toc-level1-char-style  (body-char-style)
  font: (override-style 'body-font 
                        weight: 'bold
                        size: 14))

(define-character-style toc-level2-char-style  (body-char-style)
  font: (override-style 'body-font size: 12))
                   
(define-character-style toc-level3-char-style  (body-char-style)
  font: (override-style 'body-font size: 10))
                   

(define-paragraph-style toc-metatitle-para  ($default-para-style)
  line-spacing: 'using-font-size
  default-char-style: 'toc-metatitle-char-style
  space-before: 6
  tab-stops: '())

(define-paragraph-style toc-level1-para  ($default-para-style)
  default-char-style: 'toc-level1-char-style
  line-spacing: 'using-font-size
  space-before: 6
  tab-stops: '((position: (l 18) align: left)
               (position: (r 0) align: right)))

(define-paragraph-style toc-level2-para  ($default-para-style)
  default-char-style: 'toc-level2-char-style
  line-spacing: 'using-font-size
  left-margin-first: 18
  tab-stops: '((position: (l 36) align: left)
               (position: (r 0) align: right)))

(define-paragraph-style toc-level3-para  ($default-para-style)
  default-char-style: 'toc-level3-char-style
  line-spacing: 'using-font-size
  left-margin-first: 36
  tab-stops: '((position: (l 54) align: left)
               (position: (r 0) align: right)))

;;;
;;;  Style support for indices (index)
;;;

(define-paragraph-style index-section-para  ($default-para-style)
  space-before: 18)

(define-paragraph-style index-level1-para  ($default-para-style)
  default-char-style: 'index-content-char-style
  line-spacing: 'using-font-size
  left-margin-first: 0
  left-margin: 0.25in)

(define-paragraph-style index-level2-para  ($default-para-style)
  default-char-style: 'index-content-char-style
  line-spacing: 'using-font-size
  left-margin-first: 0.125in
  left-margin: 0.375in)

(define-character-style index-section-char-style  (title1-char-style)
)

(define-character-style index-content-char-style  (body-char-style)
)

(define-character-style index-primary-ref-char-style  (bold-char-style)
)

(define-character-style index-seealso-char-style  (emphasis-char-style)
)

(define-page-style index-right-page  (index-first-page)
)

(define-page-style index-left-page  (index-first-page)
)

(define-page-style index-first-page  (letter)
  content: (list
            (make <text-box>
                  frame: (make-rect 1in 8.75in 6.75in 1.25in)
                  content: '("Index")
                  style: 'chapter-title-char-style
                  align: 'right)
            (make <text-frame>
                  flow: 'index
                  frame: (make-rect 1in 1in 6.75in 7in)
                  num-columns: 2
                  column-gap: 0.5in)
            (make <line-graphic>
                  line-start: (make-point 1in 8.25in)
                  line-end: (make-point 7.75in 8.25in))
            (make <line-graphic>
                  line-start: (make-point 1in $footer-rule-y)
                  line-end: (make-point 7.75in $footer-rule-y))
            (make <text-box>
                  frame: (make-rect 7in 0.5in 0.75in 0.25in)
                  content: '((page-number))
                  style: 'emphasis-char-style
                  align: 'right)))

