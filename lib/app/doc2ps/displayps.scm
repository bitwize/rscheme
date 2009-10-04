
(define *use-ghostscript* #t)
(define *dps* #f)

(define (with-output-to-ps thunk)
  (with-output-to-port *dps* thunk)
  (flush-output-port *dps*))
  
(define (display-postscript ps)
  (if (not *dps*)
      (if *use-ghostscript*
          (set! *dps* (open-output-process "gs >/tmp/gs.log"))
          (set! *dps* (open-output-file "/tmp/dps.ps"))))
  (write-string *dps* ps)
  (flush-output-port *dps*))

(define (init-dps)
  (display-postscript (text *dps-preamble*)))

(define *dps-preamble* {
erasepage initgraphics
/f0 /Courier-Bold findfont 10 scalefont def
/f1 /Times-Roman findfont 48 scalefont def
/f2 /Times-Bold findfont 48 scalefont def

%     label x y width   linestart   - 
%     x                 tabtic      -

/linetic 20 def

/linestart {
  /width exch def
  gsave
    translate
    0.25 setlinewidth
    gsave
      [0.5 2] 0 setdash
      0 linetic moveto
      0 0 lineto
      width 0 lineto
      width linetic lineto stroke
      f0 setfont
      width 0 moveto 30 5 rmoveto show
    grestore
} def

/lineend {
  0 25 rmoveto 0 -50 rlineto 0.25 setlinewidth stroke
  grestore
} def

/ticmark {
  gsave
    0 translate
    0 0 moveto 3 -4 lineto -3 -4 lineto closepath fill
  grestore
} def

/tabtic {
  gsave
    [0.25 1] 0 setdash
    -10 moveto 0 linetic rlineto stroke
  grestore
} def

})

#|
20 20 200 linestart

50 tabtic
75 tabtic

/Times-Roman findfont 48 scalefont setfont
0 0 moveto
(KooPooAy) show

lineend
|#