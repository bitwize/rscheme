(define-class <not-a-tif-file> (<condition>)
  (file type: <string>))

(define-class <no-such-tag> (<condition>)
  (tiff-file type: <string>)
  tiff-subimage
  tiff-tag)

;;;
