(define-module graphics.device ()
  (&module
   (import rs.lang
           iolib                        ; for `~'
           tables
           paths regex          ; for eps support
           rs.util.properties
           rs.sys.multimethod
           rs.util.pack
           graphics.geometry
           graphics.color
           graphics.fontmgr
           graphics.afm
           graphics.styles
           graphics.image
           syscalls
           util.pdf
           rs.sys.compression.lzw)
   ;;
   (load "abstract.scm")
   ;;
   (export <graphics-device>
           ;;
           close-graphics-device
           ;;
           areastroke
           areafill
           areapath
           areashow             ; fill: and stroke: keyword args
           ;;
           ;;  transformation
           ;;
           scale
           rotate
           translate
           concat
           ;;
           ;;  path construction
           ;;
           moveto
           lineto
           curveto
           arc
           arcn
           closepath
           newpath
           ;;
           ;;  graphics state
           ;;
           with-gstate-saved
           setdash
           clip
           rectclip
           setfont
           currentfont
           currentpoint
           currentmatrix
           setcolor
           device-color
           setlinewidth
           setlinecap
           setlinejoin
           setmiterlimit
           ;;
           ;;  painting
           ;;
           stroke
           fill
           rectstroke
           rectfill
           show
           xshow
           dxshow                       ; Implements PDF 'TJ' operator
           composite-image
           include-eps
           get-eps-bbox
           ;;
           ;;  document structure
           ;;
           starttext
           endtext
           startpage                    ; (startpage DEVICE [LABEL])
           endpage                      ; (endpage DEVICE)
           ;;
           ;;  PDF-in-PS support
           pdfmark-annotation
           to-pdfmark
           )
   ;;
   (load "postscript.scm")
   (export open-ps-device
           include-font
	   set-binary-encoding-ok?!
           open-eps-device)
   ;;
   (load "pdf.scm")
   (export open-pdf-device)
   ;;
   (load "bboxdev.scm")
   (export open-bbox-device)
   ;;
   (load "stdstyles.scm")
   (export <font-style>
           <stroke-style>
           <fill-style>
           <solid-paint-style>
           <pattern-paint-style>
           ;;
           define-stroke-style
           define-fill-style
           define-font-style
           define-solid-paint-style
           define-pattern-paint-style)
   ;;
   (load "gaudy.scm")
   (export roundrectstroke
           draw-long-brace
           arrowstroke)
   ;;
   (load "drawdimens.scm")      ; super gaudy
   (export draw-dimen-labels)
   ;;
   (load "current.scm")
   (export with-graphics-device
           ;;
           ;;  WITH-GRAPHICS-FILE file thunk #key bbox type
           ;;
           ;;  output to a file; explicit type keyword, or implicit
           ;;  in extension.  If bbox needed (eps), calls thunk twice
           ;;
           with-graphics-file
           compute-graphics-bbox
           current-graphics-device)
   ;;
   ))
