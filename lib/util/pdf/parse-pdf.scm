
(define eol-pattern (reg-expr->proc '(or "\r\n" "\r" "\n")))
(define xref-block-header (unformat->proc "~d ~d~a"))

(define-class <pdf> (<object>)
  filename
  fstream
  (object-table init-value: #f)
  (trailer-block init-value: #f)
  (last-xref-section-at init-value: #f)
  (updated-objects-map init-value: #f))

(define-class <pdf-object> (<object>)
  (properties type: <vector> init-value: '#())
  id
  generation
  offset                        ; file-offset or (deleted . [LINK|()])
  owner
  (%value-cache init-value: '#uninit))

(define-method value ((self <pdf-object>))
  (if (eq? (%value-cache self) '#uninit)
      (begin
        (set-%value-cache! self #f)
        (let ((v (pdf-read-indirect-obj (port-at (owner self) (offset self))
                                        (owner self))))
          (set-%value-cache! self v))))
  (%value-cache self))

(define-method port-at ((self <pdf>) #optional offset)
  (if offset
      (pdf-seek self offset))
  (make <std-input-port>
        name: (filename self)
        file-stream: (fstream self)))

(define-method write-object ((self <pdf-object>) port)
  (if (pair? (offset self))
      (format port "#[<pdf-object> ~d DEL=>~d]"
              (id self)
              (cdr (offset self)))
      (format port "#[<pdf-object> ~d +~d]"
              (id self)
              (offset self))))

(define (open-pdf filename)
  (let ((f (fopen filename "r+b")))
    (if (not f)
        (error "open-pdf: open of ~s failed" filename))
    (let ((pdf (make <pdf>
                     filename: filename
                     fstream: f)))
      (load-xref pdf)
      pdf)))

(define (load-trailer (self <pdf>))
  (let ((b (make-string 256)))
    (fseek (fstream self) -256 2)
    (fread-fill (fstream self) b 0 256)
    (let ((l (reverse! (string-split b eol-pattern))))
      (if (not (string=? (list-ref l 3) "startxref"))
          (error "Didn't find 'startxref' on 3rd line from end"))
      (if (not (string=? (list-ref l 1) "%%EOF"))
          (error "Didn't find '%%EOF' on last line"))
      (or (string->number (list-ref l 2))
          (error "Couldn't parse byte offset of last xref section: ~s"
                 (list-ref l 2))))))

(define-method dict-lookup ((self <hash-table>) key)
  (table-lookup self key))

(define-method catalog ((self <pdf>))
  (table-lookup (trailer-block self) 'Root))

(define-method dict-lookup ((self <pdf-object>) key)
  (dict-lookup (value self) key))

(define (read-trailer-block (self <pdf>))
  (bind ((p (port-at self))
         (t v (pdf-scan p)))            ; scan past the "<<"
    (if (not (eq? t 'dict-start))
        (error "Expected dict start"))
    (pdf-read-eval p 'dict-end self)))

(define (load-xref (self <pdf>))
  (set-object-table! self (make-fixnum-table))
  ;; Read the offset of the "last" (most recent data) xref section from
  ;; the very end of the file
  (let ((x (load-trailer self)))
    (set-last-xref-section-at! self x)
    ;; read the xref section itself
    (load-xref-section self x)
    ;; which is immediately followed by the `trailer << ... >>' block
    (set-trailer-block! self (read-trailer-block self))
    ;; now go read the other xref sections, following the Prev links
    (let loop ((tb (trailer-block self)))
      (if (table-lookup tb 'Prev)
          (begin
            (load-xref-section self (table-lookup tb 'Prev))
            (loop (read-trailer-block self)))))))

(define (load-xref-section (self <pdf>) offset)
  ;(format #t "Loading XREF section at ~d\n" offset)
  ;;
  (pdf-seek self offset)
  ;;
  (if (not (string=? (read-line self) "xref"))
      (error "Didn't see 'xref' line"))
  ;;
  (let ((tbl (object-table self)))
    (let loop ()
      ;; read the first object number and number of objects
      (let ((xbh-line (read-line self)))
        (if (string=? xbh-line "trailer")
            tbl
            (bind ((first-obj num-obj (xref-block-header xbh-line))
                   (data (make-string (* 20 num-obj))))
              (fread-fill (fstream self) data 0 (* 20 num-obj))
              (load-xref-entries! self tbl data first-obj num-obj)
              (loop)))))))

(define (load-xref-entries! self tbl data i n)
  (let loop ((i i)
             (j 0)
             (k 0))
    (if (< j n)
        (let ((offset (string->number (substring data k (+ k 10))))
              (gen (string->number (substring data (+ k 11) (+ k 16))))
              (type (string-ref data (+ k 17))))
          (if (not (table-key-present? tbl i))
              (case type
                ((#\f)
                 (table-insert! tbl i (make <pdf-object>
                                            id: i
                                            generation: gen
                                            offset: (cons 'deleted offset)
                                            owner: self)))
                ((#\n)
                 (table-insert! tbl i (make <pdf-object>
                                            id: i
                                            generation: gen
                                            offset: offset
                                            owner: self)))
                (else
                 (error "Unknown xref entry type: ~s" type))))
          (loop (+ i 1)
                (+ j 1)
                (+ k 20))))))

(define (pdf-seek (self <pdf>) (offset <fixnum>))
  (fseek (fstream self) offset 0))

(define-method input-port-read-line ((self <pdf>))
  (let (((first <pair>) (cons 0 '()))
        (stream (fstream self)))
    (let loop (((prev <pair>) first))
      (let ((ch (fgetc stream)))
	(if (not ch)
            ;; EOF
	    (if (eq? prev first)
                #f
		(list->string (cdr first)))
            ;; not EOF
            (if (or (eq? ch #\newline)
                    (eq? ch #\cr))
                (begin
                  (if (eq? ch #\cr)
                      (if (not (eq? (fgetc stream) #\newline))
                          (fseek stream -1 1)))
                  (list->string (cdr first)))
		(let (((cell <pair>) (cons ch '())))
		  (set-cdr! prev cell)
		  (loop cell))))))))


(define (make-dict . lst)
  (keywords->table lst))

(define (keywords->table lst)
  (let ((tbl (make-symbol-table)))
    (let loop ((i lst))
      (if (null? i)
          tbl
          (begin
            (table-insert! tbl (keyword->symbol (car i)) (cadr i))
            (loop (cddr i)))))))
