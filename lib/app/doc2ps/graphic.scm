,(use paths)

(define eps-orientation (reg-expr->proc 
                         '(prefix (seq "%%Orientation:"
                                       (* space)
                                       (save (or "Landscape" "Portrait"))))))

(define eps-comment (reg-expr->proc '(prefix "%")))

(define eps-bbox (reg-expr->proc 
                  '(prefix
                    (seq "%%BoundingBox:"
                         (* space)
                         (save (seq (? #\-) (+ (or digit #\.))))
                         (+ space)
                         (save (seq (? #\-) (+ (or digit #\.))))
                         (+ space)
                         (save (seq (? #\-) (+ (or digit #\.))))
                         (+ space)
                         (save (seq (? #\-) (+ (or digit #\.))))))))


(define (parse-eps file)
  (let ((bbox #f)
        (orient #f))
    ;;
    (call-with-input-file
        file
      (lambda (port)
        ;; note that we don't read the entire file; we only
        ;; scan the header section
        (let loop ()
          (let ((l (read-line port)))   ; XXX needs to work with \r and \r\n line terms, too
            (format #t "==> ~s\n" l)
            (cond
             ((eof-object? l)
              (values))
             ((eps-comment l)
              (bind ((s e llx lly urx ury (eps-bbox l))
                     (os oe o (eps-orientation l)))
                (if s
                    (let ((llx (string->number llx))
                          (lly (string->number lly))
                          (urx (string->number urx))
                          (ury (string->number ury)))
                      (set! bbox (make-rect llx lly (- urx llx) (- ury lly)))))
                (if os
                    (set! orient o)))
              (loop)))))))
    ;;
    (values bbox orient)))

(define-method get-eps-file ((self <GRAPHIC>))
  (cond
   ;; it's referenced using an entity
   ((get-attribute-value self "entityref")
      (bind ((ent (get-attribute-value self "entityref")))
        (entity-os-path ent)))
   ;; it's referenced using a fileref
   ((get-attribute-value self "fileref")
    (format #t "ORIGIN-SYSTEM-FILE ~s\n" (origin-system-file self))
    (let ((origin-dir (or (file-directory 
                           (string->file (origin-system-file self)))
                          $dot-dir)))
      (format #t "ORIGIN-DIR ~s\n" origin-dir)
      (format #t "FILEREF ~s\n" (string->file
                                 (get-attribute-value self "fileref")))
      (let* ((f (append-path origin-dir
                             (string->file
                              (get-attribute-value self "fileref"))))
             (fx (if (extension f)
                     f
                     (extension-related-path f "eps"))))
        (format #t "F ~s\n" f)
        (format #t "FX ~s\n" fx)
        ;; add a ".eps" if it doesn't already have an extension
        (pathname->os-path fx))))
   ;;
   (else
    (error "~s: can't find content" self))))

(define (unlandscape-bbox (f <rect>))
  (make-rect (origin-y f)
             (- (origin-x f))
             (size-height f)
             (size-width f)))
  
(define (scale-bbox (f <rect>) s)
  (make-rect (* s (origin-x f))
             (* s (origin-y f))
             (* s (size-width f))
             (* s (size-height f))))

(define-method bridge-figure-content ((self <GRAPHIC>) emit)
  (bind ((f (get-eps-file self))
         (s (/ (string->number (or (get-attribute-value self "scale") 
                                   "100")) 
               100))
         (bb o (parse-eps f))
         (bb rot? (if (and (string? o) (string-ci=? o "Landscape"))
                      (values (unlandscape-bbox bb) #t)
                      (values bb #f)))
         (bb (scale-bbox bb s)))
    (format #t "EPS ~s => ~s (scale ~s)\n" f bb s)
    (emit (make <flow-vbox>
                align: 'center
                height: (height bb)
                render-proc: (lambda (self width dev)
                               (render-eps dev width bb f s rot?))))
    f))

(define (render-eps dev width bb file s rot?)
  (let ((f (offset-rect bb
                        (- (origin-x bb))
                        (- (origin-y bb))))
        (dx (if width
                (/ (- width (size-width bb)) 2)
                0)))
    (format #t "EPS ~a : BB ~s width ~s ; dx ~s\n" file bb width dx)
    (with-gstate-saved
     dev
     (lambda ()
       (if *show-outlines*
           (let ((f (offset-rect f dx 0)))
             (setcolor dev (device-color dev '(gray 0.5)))
             (rectstroke dev f)
             (moveto dev (origin f))
             (lineto dev (upper-right f))
             (stroke dev)

             (moveto dev (upper-left f))
             (lineto dev (lower-right f))
             (stroke dev)))
       (translate dev (make-point (+ (- (origin-x bb)) dx)
                                  (- (origin-y bb))))
       (if rot?
           (begin
             (rotate dev -90)
             (translate dev (make-point (- (size-height bb)) 0))))
       (scale dev s s)
       (include-eps dev file)))))

