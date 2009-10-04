
(define-class <file-content> (<object>) :abstract)

(define-class <text-file-content> (<file-content>)
    (line-tree type: <vector>))

(define-class <binary-file-content> (<file-content>)
    (data type: <byte-vector>))
