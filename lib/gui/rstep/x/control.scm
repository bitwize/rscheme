
(define-class <control> (<view>)
  (target init-value: #f)
  (action init-value: #f))

(define-method perform-action ((self <control>) #optional from)
  (if (action self)
      ((action self) (target self) (or from self))))

(define-class <cell> (<object>) :abstract
  (value init-value: #f))

(define-class <action-cell> (<cell>) :abstract
  (target init-value: #f)
  (action init-value: #f))

(define-method perform-action ((self <cell>) #optional from)
  (if (action self)
      ((action self) (target self) (or from self))))

