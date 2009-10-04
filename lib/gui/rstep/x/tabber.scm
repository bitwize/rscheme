
(define-class <tabber> (<control>)
  (tab-cells type: <vector>))

(define-class <tab-cell> (<cell>)
  (parent type: <tab-control>)
  (tab-width init-value: #f) ; not counting tab xover graphics
  (label-x init-value: #f)
  (tab-label type: <string>))

