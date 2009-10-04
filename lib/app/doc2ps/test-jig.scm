
;;;
;;; Given a flow, lay it out and display the result
;;;

(define (test-flow (flow <flow>) page-style)
  (reset-pages)
  ;;
  (layout-flow-in-page-sequence
   flow
   (make <simple-page-sequence>
         initial-page-styles: '()
         blank-page-style: #f
         repeat-page-styles: (list page-style))
   "i")
  ;;
  (tgsp 0))
