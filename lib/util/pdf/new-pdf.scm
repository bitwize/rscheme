
(define (call-with-pdf-port (pdf <pdf>) proc)
  (let ((p (make <std-output-port>
                 name: (filename pdf)
                 file-stream: (fstream pdf))))
    (proc p)
    (flush-output-port p)
    (values)))

(define (create-pdf filename)
  (let ((f (fopen filename "w+b")))
    ;;
    (if (not f)
        (error "create-pdf: creation of ~s failed" filename))
    ;;
    (let* ((tb (make-symbol-table))
           (pdf (make <pdf>
                      filename: filename
                      fstream: f
                      object-table: (make-fixnum-table)
                      trailer-block: tb))
           (root (alloc-object pdf (make-symbol-table))))
      ;;
      (table-insert! (object-table pdf) 0 (make <pdf-object>
                                                id: 0
                                                generation: 65535
                                                offset: '(deleted . 0)
                                                owner: pdf
                                                %value-cache: #f))
      ;;
      (call-with-pdf-port pdf
                          (lambda (port)
                            (write-string port "%PDF-1.4\n")
                            (write-string port "% RScheme util.pdf\n")))
      ;;
      (table-insert! tb 'Root root)
      ;;
      (dict-insert! root 'Type 'Catalog)
      (dict-insert! root 'Version (string->symbol "1.4"))
      (let ((pages (alloc-dict pdf)))
        ;;
        (dict-insert! pages 'Type 'Pages)
        (dict-insert! pages 'Kids '#())
        (dict-insert! pages 'Count 0)
        ;;
        (dict-insert! root 'Pages pages))
      ;;
      pdf)))

(define (pdf-insert-page (self <pdf>)
                         #key
                         (before-page default: #f)
                         (after-page default: #f))
  (let ((pages (dict-lookup (catalog self) 'Pages))
        (new-page (alloc-dict self)))
    ;;
    (dict-insert! new-page 'Type 'Page)
    (dict-insert! new-page 'Parent pages)
    (dict-insert! new-page 'Resources (make-symbol-table))
    (dict-insert! new-page 'MediaBox (vector 0 0 (* 8+1/2 72) (* 11 72)))
     ;;
    (dict-insert! pages 'Kids
                  (vector-append (dict-lookup pages 'Kids)
                                 (vector new-page)))
    (let loop ((pp pages)) 
      (dict-insert! pages 'Count (+ 1 (dict-lookup pages 'Count)))
      (let ((parent (dict-lookup pp 'Parent)))
        (if parent
            (loop parent)
            new-page)))))

(define (page-append-contents (page <pdf-object>) stream)
  (assert (eq? (dict-lookup page 'Type) 'Page))
  (let ((c (dict-lookup page 'Contents)))
    (if (vector? c)
        (dict-insert! page 'Contents (vector-append c (vector stream)))
        (if c
            (dict-insert! page 'Contents (vector c stream))
            (dict-insert! page 'Contents stream)))))

(define (page-assign-resource-id (self <pdf-object>))
  (assert (eq? (dict-lookup self 'Type) 'Page))
  (let ((id (get-property self 'next-id 0)))
    (set-property! self 'next-id (+ id 1))
    id))

(define (page-add-resource (self <pdf-object>) name item type)
  (assert (eq? (dict-lookup self 'Type) 'Page))
  (let* ((rsrc (dict-lookup self 'Resources))
         (rtab (or (table-lookup rsrc type)
                   (let ((f (make-symbol-table)))
                     (table-insert! rsrc type f)
                     f))))
    (table-insert! rtab name item)))
  
(define (page-add-colorspace-resource (self <pdf-object>) name item)
  (page-add-resource self name item 'ColorSpace))
  
(define (page-add-font-resource (self <pdf-object>) name font)
  (page-add-resource self name font 'Font))

(define (page-add-pattern-resource (self <pdf-object>) name font)
  (page-add-resource self name font 'Pattern))
 
#|
(define *eexec-pattern* (reg-expr->proc '(seq "eexec" (+ (or #\cr #\lf)))))

(define (type-1-skip-trailing-zeros str)
  (let loop ((k (- (string-length str) 1))
             (n 0))
    (if (= n 512)
        (+ k 1)
        (if (char=? (string-ref str k) #\0)
            (loop (- k 1) (+ n 1))
            (loop (- k 1) n)))))
        
(define (scan-pfa-font pfa)
  (bind ((str (file->string pfa))
         (s e (*eexec-pattern* str)))
    (if s
        (let* ((t (substring str e))
               (z (type-1-skip-trailing-zeros t)))
          (values str e (+ e z)))
        (error "~a: Could not parse PFA font file" pfa))))
|#

#|
  (bind ((data brk0 brk1 (scan-pfa-font (get-font-definition font)))
         (code-min code-max (char-code-range font))
         (widths (list->vector
                  (map (lambda (w)
                         (if w
                             (* w 1000)
                             0))
                       (char-widths
                        font
                        (list->string
                         (map 
                          integer->char
                          (list-tail (range (+ code-max 1)) code-min)))))))
         (bbox (get-property font 'FontBBox)))
    ;;
    (make <loaded-type1-font>
          base-font: (get-property font 'FontName)
          first-char: code-min
          last-char: code-max
          char-widths: widths
          length1: brk0
          length2: (- brk1 brk0)
          length3: (- (string-length data) brk1)
          content: data
          properties: (vector 'Flags 0          ; XXX improve this...
                              'FontBBox (vector (origin-x bbox)
                                                (origin-y bbox)
                                                (limit-x bbox)
                                                (limit-y bbox))
                              'ItalicAngle (get-property font 'ItalicAngle 0)
                              'Ascent (get-property font 'Ascender)
                              'Descent (get-property font 'Descender)
                              'CapHeight (get-property font 'CapHeight)
                              'StemV (get-property font 'UnderlineThickness)
                              'XHeight (get-property font 'XHeight)))))
|#

(define-class <loaded-type1-font> (<object>)
  (family-name type: <string>)
  (style-name type: <string>)
  (base-font type: <string>)
  (first-char type: <fixnum>)
  (last-char type: <fixnum>)
  (char-widths type: <vector>)
  (length1 type: <fixnum>)
  (length2 type: <fixnum>)
  (length3 type: <fixnum>)
  (content type: <string>)
  (properties type: <vector> init-value: '#()))
  
(define (import-font-for-pdf (font <font-entry>))
  (let ((d (get-font-pdf-data font)))
    (assert (and (pair? d) (eq? (car d) 'pdf)))
    (apply make-instance <loaded-type1-font> (cdr d))))
  

(define (pdf-import-font-descriptor pdf (info <loaded-type1-font>))
  (let ((d (alloc-dict pdf))
        (stream (make <pdf-stream>
                      owner: pdf
                      dict: (make-symbol-table)
                      stream-contents: (content info))))
    ;;
    (table-insert! (dict stream) 'Length (string-length (content info)))
    (table-insert! (dict stream) 'Length1 (length1 info))
    (table-insert! (dict stream) 'Length2 (length2 info))
    (table-insert! (dict stream) 'Length3 (length3 info))
    ;;
    (dict-insert! d 'Type 'FontDescriptor)
    (dict-insert! d 'FontName (string->symbol (base-font info)))
    ;;
    (for-each
     (lambda (p)
       (dict-insert! d p (get-property info p)))
     '(Flags FontBBox ItalicAngle Ascent Descent CapHeight StemV XHeight))
    ;;
    (dict-insert! d 'FontFile (alloc-object pdf stream))
    d))
  

(define (pdf-import-font pdf (info <loaded-type1-font>))
  (let ((font (alloc-dict pdf))
        (fd (pdf-import-font-descriptor pdf info)))
    (dict-insert! font 'Type 'Font)
    (dict-insert! font 'Subtype 'Type1)
    (dict-insert! font 'BaseFont (dict-lookup fd 'FontName))
    ;;
    (dict-insert! font 'FirstChar (first-char info))
    (dict-insert! font 'LastChar (last-char info))
    (dict-insert! font 'Widths (char-widths info))
    (dict-insert! font 'FontDescriptor fd)
    ;;
    font))
