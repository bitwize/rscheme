(define (get-updated-objects-map (self <pdf>))
  (or (updated-objects-map self)
      (let ((tbl (make-object-table)))
        (set-updated-objects-map! self tbl)
        tbl)))

(define-method dict-insert! ((self <pdf-object>) key obj)
  (let ((t (value self)))
    (table-insert! t key obj)
    (set-value! self t)))

(define-method alloc-object ((self <pdf>) value)
  (let ((obj (make <pdf-object>
                   id: (+ 1 (reduce max 0 (key-sequence (object-table self))))
                   generation: 0
                   offset: 0
                   owner: self
                   %value-cache: value)))
    (table-insert! (object-table self) (id obj) obj)
    (table-insert! (get-updated-objects-map self) obj obj)
    obj))

(define (alloc-dict (self <pdf>))
  (alloc-object self (make-symbol-table)))

(define-method delete-object! ((self <pdf-object>))
  (let* ((pdf (owner self))
         (free-list-head (table-lookup (object-table pdf) 0)))
    (set-offset! self (cons 'deleted (cdr (offset free-list-head))))
    (set-cdr! (offset free-list-head) (id self))
    (table-insert! (get-updated-objects-map pdf) self self)
    (values)))

(define-method set-value! ((self <pdf-object>) value)
  (let ((pdf (owner self)))
    (table-insert! (get-updated-objects-map pdf) self self)
    (set-%value-cache! self value)
    (values)))

(define-method pdf-render ((self <pdf-object>) port)
  (format port "~d ~d obj\n" (id self) (generation self))
  (pdf-render* (%value-cache self) port)
  (format port "endobj\n"))

(define-method pdf-render* ((self <pdf-stream>) port)
  (format port "% ...a stream...\n")
  (pdf-render* (dict self) port)
  (format port " stream\n")
  (write-string port (contents self))
  (format port "\nendstream\n"))

(define-method pdf-render* ((self <symbol>) port)
  (format port "/~a" self))

(define-method pdf-render* ((self <string>) port)
  (format port "(~a)" self))

(define-method pdf-render* ((self <hash-table>) port)
  (format port "<< ")
  (table-for-each
   self
   (lambda (h k v)
     (pdf-render* k port)
     (write-char #\space port)
     (pdf-render* v port)
     (newline port)))
  (format port " >>\n"))

(define-method pdf-render* ((self <pdf-object>) port)
  (format port "~d ~d R" (id self) (generation self)))

(define-method pdf-render* ((self <vector>) port)
  (write-char #\[ port)
  (let loop ((i 0))
    (if (< i (vector-length self))
        (begin
          (if (> i 0)
              (write-char #\space port))
          (pdf-render* (vector-ref self i) port)
          (loop (+ i 1)))
        (write-char #\] port))))

(define-method pdf-render* ((self <number>) port)
  (format port "~d" self))

;;;


(define-method flush-pdf ((self <pdf>) #optional port)
  (let ((u (updated-objects-map self)))
    (if u
        (if port
            (pdf-write-new-trailer self port)
            (begin
              (fseek (fstream self) 0 2)
              (pdf-write-new-trailer self (make <std-output-port>
                                                name: (filename self)
                                                file-stream: (fstream self)))
              (fflush (fstream self)))))))

(define-method file-offset ((self <std-output-port>))
  (ftell (file-stream self)))

(define (pdf-write-new-trailer (self <pdf>) port)
  ;;
  (let ((free-list-head (table-lookup (object-table self) 0)))
    (table-insert! (updated-objects-map self) free-list-head free-list-head))
  ;;
  (let ((lst (sort (key-sequence (updated-objects-map self))
                   (lambda (a b)
                     (< (id a) (id b))))))
    ;(format #t "~a: ~d objects updated\n" (filename self) (length lst))
    (set-updated-objects-map! self #f)
    ;;
    (for-each
     (lambda ((obj <pdf-object>))
       (if (not (pair? (offset obj)))
           (begin
             (set-offset! obj (file-offset port))
             (pdf-render obj port)
             (newline port)
             (newline port))))
     lst)
    ;;
    (if (last-xref-section-at self)
        (format port "% Now for the updated cross-reference...\n")
        (format port "% Now for the cross-reference...\n"))
    ;;
    (let ((xrefat (file-offset port)))
      (if (last-xref-section-at self)
          (table-insert! (trailer-block self) 
                         'Prev
                         (last-xref-section-at self)))
      (table-insert! (trailer-block self) 'Size 
                     (table-size (object-table self)))
      (set-last-xref-section-at! self xrefat)
      ;;
      (format port "xref\n")
      (emit-pdf-xrefs port lst)
      (format port "trailer\n")
      (pdf-render* (trailer-block self) port)
      (format port "startxref\n")
      (format port "~d\n" xrefat)
      (format port "%%EOF\n"))))

(define (emit-pdf-xrefs port lst)
  (if (pair? lst)
      (bind ((i num (xref-compute-run lst)))
        (format port "~d ~d\n" i num)
        (let loop ((n num)
                   (lst lst))
          (if (> n 0)
              (let ((l (car lst)))
                (if (pair? (offset l))
                    (format port "~010d ~05d f \n" (cdr (offset l)) (generation l))
                    (format port "~010d ~05d n \n" (offset l) (generation l)))
                (loop (- n 1) (cdr lst)))
              (emit-pdf-xrefs port lst))))))

                
(define (xref-compute-run lst)
  (let ((i (id (car lst))))
    (let loop ((n 1)
               (t (cdr lst)))
      (if (and (pair? t)
               (eq? (id (car t)) (+ i n)))
          (loop (+ n 1) (cdr t))
          (values i n)))))
    
  
;;;


(define (make-pdf-stream pdf (str <string>))
  (make <pdf-stream>
        dict: (make-dict Length: (string-length str))
        owner: pdf
        stream-contents: str))
  
(define (output-to-pdf-stream pdf proc)
  (make-pdf-stream pdf (call-with-output-string proc)))

;;;

(define-class <pdf-output-port> (<output-port>)
  (file-offset init-value: 0)
  underlying-output-port)

(define (open-pdf-output-port underlying)
  (make <pdf-output-port>
        underlying-output-port: underlying))

(define-method write-string ((self <pdf-output-port>) str)
  (write-string (underlying-output-port self) str)
  (set-file-offset! self (+ (file-offset self) (string-length str))))

(define-method output-port-write-char ((self <pdf-output-port>) ch)
  (output-port-write-char (underlying-output-port self) ch)
  (set-file-offset! self (+ (file-offset self) 1)))
