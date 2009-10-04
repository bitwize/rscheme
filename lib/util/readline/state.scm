
;;;  a <readline-state> keeps track of all the information we
;;;  need to proceed with a single attempt to read a line
;;;
;;;  an instance is created by `provide-more-input'

(define-class <readline-state> (<object>)
  (ever-read-from? type: <boolean> init-value: #f)
  (history type: <list> init-value: '())
  (completions type: <list> init-value: '())
  (word-break type: <string> init-value: " \t\n\"\\()[]{},'"))
