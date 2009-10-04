;;;

(define-thread-var *current-font-database* #f)

;;;

(define *font-databases* (make-string-table))

;;;

(define (expand-path p)
  (pathname->os-path (string->file p)))

(define (open-font-database-for-update #optional 
                                       (file default: "~/lib/fontbase.sto"))
  (let ((file (expand-path file)))
    (if (eq? (car (or (table-lookup *font-databases* file) '(#f))) 'update)
        (root-object (cadr (table-lookup *font-databases* file)))
        (let ((c (open-persistent-store file)))
          ;;
          (let ((old (table-lookup *font-databases* file)))
            (if old
                (begin
                  (table-remove! *font-databases* file)
                  (close-persistent-store (cadr old)))))
          ;;
          (register-indirect-page c 10 $indirects)
          (table-insert! *font-databases* file (list 'update c))
          (root-object c)))))
  

(define (open-font-database #optional (file default: "~/lib/fontbase.sto"))
  (let ((file (expand-path file)))
    (if (table-lookup *font-databases* file)
        (root-object (cadr (table-lookup *font-databases* file)))
        (let ((c (read-persistent-store file)))
          (register-indirect-page c 10 $indirects)
          (table-insert! *font-databases* file (list 'read c))
          (root-object c)))))

(define (init-font-database #optional (file default: "~/lib/fontbase.sto"))
  (let ((file (expand-path file)))
    (let ((c (create-persistent-store file)))
      (register-indirect-page c 10 $indirects)
      (let ((db (make <font-database>
                  %alloc-area: (default-allocation-area c)
                  family-index: (make-string-table))))
        ;;
        (for-each
         (lambda (std-entry)
           (bind ((family fmember psname (list->values std-entry))
                  (f (load-font* db family fmember psname)))
             (define (q n t f) (if (memq n std-entry) t f))
             ;;
             (if (list-ref std-entry 3)
                 (set-property! f 'style-weight (list-ref std-entry 3))
                 (set-property! f 'style-weight 'normal))
             (if (list-ref std-entry 4)
                 (set-property! f 'style-angle (list-ref std-entry 4))
                 (set-property! f 'style-angle 'normal))
             ;;
             (set-property! f 'style-pitch (q 'fixed 'fixed 'proportional))
             (set-property! f 'style-serif (q 'sans 'sans 'serif))
             (set-property! f 'style-symbolic (q 'symbolic 'symbolic 'normal))
             (set-property! f 'style-script (q 'script 'script 'normal))
             (set-property! f 'style-caps (q 'caps 'caps 'normal))
             ;; these are all normal-width fonts
             (set-property! f 'style-width 'normal)
             ;;
             (set-property! f 'standard-pdf #t)))
         ;;
         '(("Times" "Roman" "Times-Roman" #f #f)
           ("Times" "Bold" "Times-Bold" bold #f)
           ("Times" "Italic" "Times-Italic" #f italic)
           ("Times" "Bold Italic" "Times-BoldItalic" bold italic)
           ;;
           ("Helvetica" "Regular" "Helvetica" #f #f sans)
           ("Helvetica" "Bold" "Helvetica-Bold" bold #f sans)
           ("Helvetica" "Oblique" "Helvetica-Oblique" #f oblique sans)
           ("Helvetica" "Bold Oblique" "Helvetica-BoldOblique" bold oblique sans)
           ;;
           ("Courier" "Regular" "Courier" #f #f fixed)
           ("Courier" "Bold" "Courier-Bold" bold #f fixed)
           ("Courier" "Oblique" "Courier-Oblique" #f oblique fixed)
           ("Courier" "Bold Oblique" "Courier-BoldOblique" bold oblique fixed)
           ;;
           ("Symbol" "Regular" "Symbol" #f #f symbolic)
           ("ZapfDingbats" "Regular" "ZapfDingbats" #f #f symbolic)))
        ;;
        (commit c db)
        (set! *current-font-database* (root-object c))
        (table-insert! *font-databases* file c)
        (root-object c)))))

(define-method commit ((self <font-database>))
  (commit (allocation-area->store (object->allocation-area self))))

(define-method commit ((self <type-1-font>))
  (commit (allocation-area->store (object->allocation-area self))))

(define-method commit ((self <font-entry>))
  (commit (allocation-area->store (object->allocation-area self))))

(define (load-font fname mname #key 
                   postscript
                   (variation default: #f)
                   (width default: #f)
                   (weight default: #f)
                   (angle default: #f)
                   (pitch default: #f)
                   (serif default: #f)
                   (symbolic default: #f)
                   (script default: #f)
                   (caps default: #f)
                   (standard-font default: #f))

  (let ((f (load-font* (current-font-database) fname mname postscript)))
    (define (sp name value)
      (if value
          (set-property! f name value)))
    ;;
    (sp 'style-variation variation)
    (sp 'style-width width)
    (sp 'style-weight weight)
    (sp 'style-angle angle)
    (sp 'style-pitch pitch)
    (sp 'style-serif serif)
    (sp 'style-symbolic symbolic)
    (sp 'style-script script)
    (sp 'style-caps caps)
    ;;
    (if (not standard-font)
        (get-font-pdf-data f))
    (commit (current-font-database))))

(define (load-font* (self <font-database>) fname mname psname)
  (let ((font (make <font-entry>
                    family-name: fname
                    member-name: mname
                    content: (make <type-1-font>
                                   postscript-name: psname
                                   afm: (get-afm psname)))))
    (table-insert! (family-index self)
                   fname
                   (cons font 
                         (or (table-lookup (family-index self) fname) 
                             '())))
    font))

(define (set-font-encoding! (self <font-entry>) (encoding <vector>))
  (assert (= (vector-length encoding) 256))
  (set-encoding! (content self) encoding))

(define (set-font-outlines! (self <font-entry>) (outlines <symbol-table>))
  (set-outlines! (content self) outlines))

;;;

(define-method get-font-definition ((self <font-entry>))
  (get-font-definition (content self)))

(define-method get-font-definition ((self <type-1-font>))
  (or (font-program self)
      (let ((file (get-font-definition (afm self))))
        (set-font-program! self (file->string file))
        (font-program self))))

;;;

(define (current-font-database)
  (or *current-font-database*
      (let ((db (open-font-database)))
        (set! *current-font-database* db)
        db)))

         
(define-method has-font-outlines? ((self <font-entry>))
  (if (outlines (content self))
      #t
      #f))

