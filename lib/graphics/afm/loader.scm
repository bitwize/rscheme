
(define-class <char-metrics> (<object>)
  (name type: <symbol>)
  (code type: <fixnum>)
  (x-width type: <fixnum>)
  (bbox type: <rect>)
  (kern-follows type: <vector> init-value: '#()))

(define-class <afm> (<object>)
  (filename type: <string>)
  (properties type: <vector>)
  (char-metrics type: <symbol-table>) ;; mapping names to metrics
  (code-metrics type: <char-table>))  ;; mapping chars to metrics

(define-method get-font-definition ((self <afm>))
  (or (get-property self 'font-file #f)
      (let ((f (get-font-definition* self)))
        (if f
            (begin
              (set-property! self 'font-file f)
              f)
            (error "~a: Could not find font file" (filename self))))))

(define (get-font-definition* (self <afm>))
  (let ((afm-f (string->file (filename self))))
    (let loop ((xs '("pfa" "pfb")))
      (if (pair? xs)
          (let ((fd-f (pathname->os-path (extension-related-path 
                                          afm-f 
                                          (car xs)))))
            (if (os-file-exists? fd-f)
                fd-f
                (loop (cdr xs))))
          #f))))

(define-method write-object ((self <char-metrics>) port)
  (format port "#[<char-metric> ~a]" (name self)))

(define-method name ((self <afm>))
  (get-property self 'FontName))

(define-method write-object ((self <afm>) port)
  (format port "#[<afm> ~a]" (name self)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;    Scan an AFM file
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

,(use regex)

(define StartFoo (reg-expr->proc '(prefix (seq "Start"
                                               (save (+ alpha))
                                               (? (seq (+ space)
                                                       (save (* any))))))))
(define EndFoo (reg-expr->proc '(prefix (seq "End" (save (+ alpha))))))

;;;
;;;  scan an AFM file, breaking it into a structure which reflects
;;;  the StartFoo ... EndFoo pairings
;;;
;;;  For example:
;;;
;;;    ((FontMetrics "2.0")
;;;     "FontName Foo-Italic"
;;;     "header-line-2"
;;;     "header-line-3"
;;;     ...
;;;     ((CharMetrics "228")
;;;      "C 32 ; ..."
;;;      "C 33 ; ..."
;;;      ...)
;;;     ((KernData #f)
;;;      ((KernPairs "93")
;;;       "KPX ..."
;;;       ...))
;;;     ((Composites "58")
;;;      ...))
;;;

(define (scan-afm (filename <string>))
  (let ((lines (string-split (file->string filename) 
                             (reg-expr->proc '(+ (or #\cr #\newline))))))
    (bind ((s e f x (StartFoo (car lines))))
      (if (and s (string=? f "FontMetrics"))
          (bind ((c f2 l (scan-afm* (cdr lines))))
            (if (string=? f f2)
                (cons (list (string->symbol f) x) c)
                (error "mismatch: Start~a != End~a\n" f f2)))
          (error "~a: doesn't start with StartFontMetrics")))))

(define (scan-afm* lines)
  (let loop ((l lines)
             (r '()))
    (bind ((s e f (EndFoo (car l))))
      (if s
          (values (reverse r) f l)
          (bind ((s e f x (StartFoo (car l))))
            (if s
                (bind ((c f2 l (scan-afm* (cdr l))))
                  (if (string=? f f2)
                      (loop (cdr l) (cons (cons (list (string->symbol f) 
                                                      x) 
                                                c) 
                                          r))
                      (error "mismatch: Start~a != End~a\n" f f2)))
                (loop (cdr l) (cons (car l) r))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;    Parse a scanned file
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO:  support composite chars

(define (parse-afm (afm-scan <list>))
  (parse-afm-tree* 
   afm-scan
   (list
    (list 'FontMetrics
          parse-header-line
          (list 'CharMetrics 
                parse-char-metrics)
          (list 'KernData 
                #f 
                (list 'KernPairs 
                      parse-kern-pair))
          (list 'Composites
                parse-composite)))))

(define (parse-afm-tree* afm-tree structure)
  (let ((t (caar afm-tree))
        (info (cadar afm-tree)))
    (let ((z (assq t structure)))
      (if z
          ;; we found the appropriate (sub)structure
          (begin
            (format #t "parsing substructure: ~s\n" t)
            (let loop ((e (cdr afm-tree))
                       (r '()))
              (if (null? e)
                  (cons t (reverse r))
                  (begin
                    ;(format #t "~s: ~#*@50s\n" t (car e))
                    (if (pair? (car e))
                        (let ((sub (parse-afm-tree* (car e) (cddr z))))
                          (if sub
                              (loop (cdr e) (cons sub r))
                              (loop (cdr e) r)))
                        (let ((elem ((cadr z) (car e))))
                          (if elem
                              (loop (cdr e) (cons elem r))
                              (loop (cdr e) r))))))))
          (error "~s: unknown substructure" t)))))

;;;  ignore composite data for now...

(define (parse-composite s)
  #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;    Parse Kerning Data
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define kpx (reg-expr->proc '(entire (seq "KPX"
                                          (+ space)
                                          (save (+ (not space)))
                                          (+ space)
                                          (save (+ (not space)))
                                          (+ space)
                                          (save (seq (? #\-)
                                                     (+ (or digit #\.))))))))

(define (parse-kern-pair s)
  (bind ((s e ch1 ch2 dx (kpx s)))
    (if s
        (list 'kern-pair 
              (string->symbol ch1)
              (string->symbol ch2) 
              (string->number dx))
        #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;    Parse Header lines
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *afm-header-types* (make-symbol-table))

(for-each
 (lambda (a)
   (table-insert! *afm-header-types* (car a) (cadr a)))
 '((FontBBox <bbox>)
   (CapHeight <real>)
   (XHeight <real>)
   (Descender <real>)
   (Ascender <real>)
   (UnderlinePosition <real>)
   (UnderlineThickness <real>)
   (ItalicAngle <real>)))

(define (parse-header-line ln)
  (let ((x (string-search ln #\space)))
    (if x
        (let* ((k (string->symbol (substring ln 0 x)))
               (v (substring ln (+ x 1)))
               (t (table-lookup *afm-header-types* k)))
          (cons 
           k
           (case t
             ((<bbox>)
              (parse-bbox v))
             ((<real>)
              (string->number v))
             ((#f)
              v))))
        #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;    Parse character metrics
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define cm-item (reg-expr->proc '(seq (* space)
                                      (save (+ alpha))
                                      (+ space)
                                      (save (* any)))))


(define (parse-char-metrics ln)
  (let ((f (select pair? (map (lambda (sub)
                                (bind ((s e p q (cm-item sub)))
                                  (if s
                                      (cons (string->symbol p) 
                                            (trim-trailing-space q))
                                      #f)))
                              (string-split ln #\;)))))
    (make <char-metrics>
          name: (string->symbol (cdr (assq 'N f)))
          code: (string->number (cdr (assq 'C f)))
          x-width: (string->number (cdr (assq 'WX f)))
          bbox: (parse-bbox (cdr (assq 'B f))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;    Build an AFM object from the parsed AFM data
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (assqr key list)
  (let ((a (assq key list)))
    (if a
        (cdr a)
        #f)))

(define (build-afm filename info)
  (let ((char-m (make-symbol-table))
        (code-m (make-char-table))
        (cm (assqr 'CharMetrics (cdr info)))
        (kp (assqr 'KernPairs (or (assqr 'KernData (cdr info)) '()))))
    ;; install the char metrics into the code and char-name tables
    (for-each 
     (lambda ((cm <char-metrics>))
       (table-insert! char-m (name cm) cm)
       (if (>= (code cm) 0)
           (table-insert! code-m (integer->char (code cm)) cm)))
     cm)
    ;; install the kerning data
    (for-each (lambda (kp)
                (let ((m1 (table-lookup char-m (cadr kp)))
                      (m2 (table-lookup char-m (caddr kp)))
                      (dx (cadddr kp)))
                  (set-kern-follows! m1
                                     (vector-append (kern-follows m1)
                                                    (vector m2 dx)))))
              (or kp '()))
    ;;
    (make <afm>
          filename: filename
          char-metrics: char-m
          code-metrics: code-m
          properties: (list->vector
                       (apply append
                              (map (lambda (i)
                                     (if (pair? (cdr i))
                                         '()
                                         (list (car i) (cdr i))))
                                   (cdr info)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;    Utilities
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (parse-bbox (str <string>))
  (bind ((llx lly urx ury (scan-bbox str)))
    (bbox-rect llx lly urx ury)))

(define scan-bbox (unformat->proc "~d ~d ~d ~d"))

(define (trim-trailing-space str)
  (let loop ((i (string-length str)))
    (if (eq? i 0)
        ""
        (if (char-whitespace? (string-ref str (- i 1)))
            (loop (- i 1))
            (if (= i (string-length str))
                str
                (substring str 0 i))))))

(define (load-afm file)
  (build-afm file (parse-afm (scan-afm file))))

;;;
;;;  take a quick peek at a file and return a
;;;  (<fontname> . <filename>) pair if it looks like
;;;  an AFM file containing font info
;;;

(define (peek-afm-header (f <file-name>))
  (call-with-input-file
      (pathname->os-path f)
    (lambda (port)
      (let ((s (read-string port 8000)))
        (if (and s
                 (> (string-length s) 20)
                 (string=? (substring s 0 16) "StartFontMetrics"))
            (let loop ((lines (string-split s #\newline)))
              (if (null? lines)
                  #f
                  (if (and (> (string-length (car lines)) 8)
                           (string=? (substring (car lines) 0 8) "FontName"))
                      (cons (cadr (string-split (car lines) #\space))
                            (filename f))
                      (loop (cdr lines))))))))))
