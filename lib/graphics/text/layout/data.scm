(define-class <data-node> (<object>) :abstract)

(define-class <char-node> (<data-node>) 
  (width type: <fixnum>)
  (height type: <fixnum>)
  (depth type: <fixnum>)
  (content type: <char>)
  (font type: <text-font>))

(define-method write-object ((self <char-node>) port)
  (format port "#[~s @~a ~s]"
          (name (object-class self))
          (machine-bits->string self)
          (content self)))

(define-class <glue-node> (<data-node>)
  (content type: <glue>))

(define-class <hlist-node> (<data-node>))
(define-class <vlist-node> (<data-node>) :abstract)
(define-class <whatsit-node> (<data-node>))
(define-class <rule-node> (<data-node>))
(define-class <kern-node> (<data-node>) width)
(define-class <accent-kern-node> (<kern-node>))
(define-class <explicit-kern-node> (<kern-node>))

;; the ligature symbol itself is the content; the subclass describes
;; the "expanded" text
(define-class <ligature-node> (<char-node>)
  (expanded type: <string>))

(define-class <disc-node> (<data-node>) pre-break post-break replace-count)
(define-class <math-node> (<data-node>) width)
(define-class <penalty-node> (<data-node>) penalty)
(define-class <mark-node> (<data-node>))
(define-class <ins-node> (<data-node>))
(define-class <adjust-node> (<data-node>))

(define-method write-object ((self <glue-node>) port)
  (format port "#[~s @~a ~a]"
          (name (object-class self))
          (machine-bits->string self)
          (to-string (content self))))

(define-method write-object ((self <ligature-node>) port)
  (format port "#[~s @~a ~s]"
          (name (object-class self))
          (machine-bits->string self)
          (expanded self)))

(define-method write-object ((self <kern-node>) port)
  (format port "#[~s @~a ~s]"
          (name (object-class self))
          (machine-bits->string self)
          (width self)))

