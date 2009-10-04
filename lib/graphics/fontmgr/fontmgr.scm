
(define-class <font-database> (<object>)
  (properties type: <vector> init-value: '#())
  family-index)

(define-class <font-entry> (<object>)
  (properties type: <vector> init-value: '#())
  (family-name type: <string>)
  (member-name type: <string>)
  (content init-value: #f))

(define-method write-object ((self <font-entry>) port)
  (format port "#[<font-entry> ~s ~s]" (family-name self) (member-name self)))

(define-method to-string ((self <font-entry>))
  (string-append (family-name self) "-" (member-name self)))


(define-class <type-1-font> (<object>)
  (properties type: <vector> init-value: '#())
  (postscript-name type: <string>)
  (outlines init-value: #f)
  (encoding type: <vector> init-value: '#())
  (afm type: <afm>)
  (font-program init-value: #f))
  
(define-constant $indirects (vector <font-database>
                                    <afm>
                                    <font-entry>
                                    <char-table>
                                    <char-metrics>
                                    <rect>
                                    <type-1-font>
                                    <segment-path>
                                    <line>
                                    <bezier-curve>
                                    <point>))


;;;

(define-class <text-font> (<object>)
  (font-shape type: <font-entry>)
  (font-size type: <real>))

(define-method to-string ((self <text-font>))
  (string-append (to-string (font-shape self))
                 "-"
                 (to-string (font-size self))))

(define-method write-object ((self <text-font>) port)
  (format port "#[<text-font> ~a]" (to-string self)))

(define-method postscript-name ((self <text-font>))
  (postscript-name (content (font-shape self))))

(define-method postscript-name ((self <font-entry>))
  (postscript-name (content self)))

;;;

(define-method font-metrics ((self <font-entry>))
  (afm (content self)))

;;;

(define-method font-metrics ((self <text-font>))
  (make-scaled-metrics (afm (content (font-shape self))) (font-size self)))

(define-method font-characters ((self <text-font>))
  (font-characters (font-shape self)))

;;;

(define-method string-width ((self <text-font>) str)
  (string-width (font-metrics self) str))
