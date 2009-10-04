;;;

(define (make-font-filter key value)
  (case key
    ((postscript)
     (lambda ((e <font-entry>))
       (string=? (postscript-name e) value)))
    ;;
    ((weight)
     (lambda ((e <font-entry>))
       (eq? (get-property e 'style-weight #f) value)))
    ((angle)
     (lambda ((e <font-entry>))
       (eq? (get-property e 'style-angle #f) value)))
    ((width)
     (lambda ((e <font-entry>))
       (eq? (get-property e 'style-width #f) value)))
    ((variation)
     (lambda ((e <font-entry>))
       (eq? (get-property e 'style-variation #f) value)))
    ((pitch)
     (lambda ((e <font-entry>))
       (eq? (get-property e 'style-pitch #f) value)))
    ((serif)
     (lambda ((e <font-entry>))
       (eq? (get-property e 'style-serif #f) value)))
    ((symbolic)
     (lambda ((e <font-entry>))
       (eq? (get-property e 'style-symbolic #f) value)))
    ((script)
     (lambda ((e <font-entry>))
       (eq? (get-property e 'style-script #f) value)))
    ((caps)
     (lambda ((e <font-entry>))
       (eq? (get-property e 'style-caps #f) value)))
    ;;
    ((member)
     (lambda ((e <font-entry>))
       (string=? (member-name e) value)))
    ;;
    (else
     (error "Unknown font filter: ~s" key))))

(define (do-query db keys values)
  (if (null? keys)
      (all-font-entries db)
      (if (car values)
          (if (eq? (car keys) 'family)
              (or (table-lookup (family-index db) (car values)) '())
              (select (make-font-filter (car keys) (car values))
                      (do-query db (cdr keys) (cdr values))))
          (do-query db (cdr keys) (cdr values)))))
        
        
(define (all-font-entries database)
  (apply append (value-sequence (family-index database))))

(define (query-font-database #key
                             (database default: (current-font-database))
                             (family default: #f)
                             (member default: #f)
                             (postscript default: #f)
                             (weight default: #f)
                             (angle default: #f)
                             (width default: #f)
                             (pitch default: #f)
                             (serif default: #f)
                             (symbolic default: #f)
                             (script default: #f)
                             (variation default: #f)
                             (caps default: #f))
  (do-query database
            '(postscript
              weight
              angle
              width
              pitch
              serif
              symbolic
              variation
              script
              caps
              member
              family)
            (list postscript
                  weight
                  angle
                  width
                  pitch
                  serif
                  symbolic
                  variation
                  script
                  caps
                  member
                  family)))
#|
  (cond
   ;;
   (postscript
    (select (lambda ((e <font-entry>))
              (string=? (postscript-name e) postscript))
            (query-font-database database: database
                                 family: family
                                 member: member)))
   ;;
   (weight
    (select (lambda ((e <font-entry>))
              (eq? (get-property e 'style-weight #f) weight))
            
   ;;
   (member
    (select (lambda ((e <font-entry>))
              (string=? (member-name e) member))
            (query-font-database database: database
                                 family: family)))
   ;;
   (family
    (or (table-lookup (family-index database) family) '()))
   ;;
   (else
    (apply append (value-sequence (family-index database))))))
  |#              

;;;

(define-method get-text-font ((self <font-entry>) size)
  (make <text-font>
        font-shape: self
        font-size: size))
  
(define-method get-text-font ((family <string>) style size)
  (let ((q (query-font-database family: family
                                member: style)))
    (if (null? q)
        (error "Cannot load font: family: ~s style: ~s" family style))
    (make <text-font>
          font-shape: (car q)
          font-size: size)))


(define-method font-family ((self <font-entry>))
  (family-name self))

(define-method font-style ((self <font-entry>))
  (member-name self))

(define-method font-family ((self <text-font>))
  (font-family (font-shape self)))

(define-method font-style ((self <text-font>))
  (font-style (font-shape self)))

(define-method string->outline ((text <string>) origin font)
  (let loop ((p origin)
             (cs (string->list text))
             (ws (char-widths (font-metrics font) text))
             (r '()))
    (if (null? cs)
        (values (reverse r) p)
        (loop (point+ p (make-size (car ws) 0))
              (cdr cs)
              (cdr ws)
              (cons
               (transform
                (get-outline font (car cs))
                (translate $identity-transform p))
               r)))))

(define-method get-outline ((self <text-font>) char)
  (transform (get-outline (font-shape self) char)
             (scale $identity-transform (/ (font-size self) 1000))))

(define-method get-outline ((self <font-entry>) char)
  (if (outlines (content self))
      (let* ((key (if (symbol? char)
                      char
                      (vector-ref (encoding (content self)) 
                                  (char->integer char))))
             (out (table-lookup (outlines (content self)) key)))
        (if out
            out
            (error "No outline available for ~s character ~s"
                   (postscript-name self)
                   char)))
      (error "No outlines available for ~s" (postscript-name self))))

(define-method font-glyph-names ((self <font-entry>))
  (if (outlines (content self))
      (key-sequence (outlines (content self)))
      (font-glyph-names (afm (content self)))))

(define-method font-characters ((self <font-entry>))
  (font-characters (afm (content self))))
