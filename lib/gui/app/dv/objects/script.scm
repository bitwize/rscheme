,(use iolib)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  persistent object model
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-class <script-shader> (<object>)
  name
  filename
  modification-time
  script)

(define-class <script-graphic> (<leaf-object>)
  name
  (shader type: <script-shader>))

(define-method status-line-when-sel ((self <script-graphic>))
  (format #f "Script ~d" (id self)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   <script-graphic>
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Map <script-shader>'s to their compiled procedures

(define *script-cache* (make-object-table))

(define-interactive (clear-script-cache view)
  (interactive (owner))
  (set! *shader-library* (make-string-table))
  ;; XXX this isn't quite right...
  (for-each reload-shader! (key-sequence *script-cache*))
  (set! *script-cache* (make-object-table))
  (clear-all-areas (in-document view)))

(define (get-script (self <script-graphic>))
  (or (table-lookup *script-cache* (shader self))
      (let ((f (eval (transform-shader (script (shader self))))))
        (table-insert! *script-cache* (shader self) f)
        f)))

(define-method pick-list* ((self <script-graphic>) pt ctm)
  (with-pick-device
   (lambda (dev)
     (concat* dev ctm)
     ;;
     (set-pick-point! dev pt)
     (set-owner! dev self)
     ;;
     ((get-script self) self 'artwork dev)
     (vector->list (dequeue-state (pick-list dev))))))

(define-method update-bbox! ((self <script-graphic>))
  (with-bbox-device
   (lambda (dev)
     ((get-script self) self 'artwork dev)
     (format #t "bbox device ==> ~s\n" (graphic-bounding-box dev))
     (set-graphic-bounding-box! self (graphic-bounding-box dev)))))

(define-method paint-artwork* ((self <script-graphic>) dev)
  ((get-script self) self 'artwork dev))

(define-method accum-handles ((self <script-graphic>) accum)
  (let ((hlist (with-pick-device
                (lambda (dev)
                  ((get-script self) self 'handles dev)
                  (handles dev)))))
    (let loop ((i (length hlist))
               (h hlist))
      (if (null? h)
          (values)
          (begin
            (accum self (car h) (- i 1))
            (loop (- i 1) (cdr h)))))))

(define-method paint-object* ((self <script-graphic>) dev)
  ((get-script self) self 'draw dev))

(define-method start-active-drag ((self <script-graphic>) 
				  (in-view <open-view>)
				  (initial-pt <point>)) ;; device-sheet coords
  (generic-dragger self in-view -1 initial-pt))


(define-method start-active-drag-handle ((self <script-graphic>) 
					 (in-view <open-view>)
					 handle-id
					 (initial-pt <point>)) ; sheetdevice
  (generic-dragger self in-view handle-id initial-pt))

(define-method make-adjuster ((self <script-graphic>) 
                              handle-id
                              o->d
                              initial-thunk)
  (make-handle-adjuster
   (case handle-id
     ;;------------ moving the whole object ------------
     ((-1)
      (let ((p0 (initial-thunk)))     ; compute initial point in obj. coords
        (dm "script initial point: ~s" p0)
        (lambda ((p <point>))
          ;; we are supposed to return the distance
          ;; to shift this object's origin.  for moving the whole object,
          ;; that's all we need
          (point- p p0))))
     ;;------------------------------------------------
     (else
      (let* ((p0 (initial-thunk))
             (twkr ((get-script self) self 'tweak handle-id (x p0) (y p0))))
        (if twkr
            (lambda ((p <point>))
              (bind ((x1 y1 (twkr (x p) (y p))))
                (make-size x1 y1)))
            ;; if no tweaker is defined, use the default one (which is to
            ;; translate the whole object around)
            (lambda ((p <point>))
              (point- p p0))))))
   ;
   (lambda (shift)
     (let ((ptss (with-outline-device
                  (lambda (dev)
                    (set-flatness! dev 'medium)
                    ((get-script self) self 'outline dev)
                    (vector-map (lambda (sp)
                                  ;(vector->list
                                   (explode-coords
                                    (map-object-points shift o->d sp)))
                                (all-subpaths dev))))))
       ;;
       ;(print ptss)
       (lambda (win gc)
         (vector-for-each
          (lambda (pts)
            (draw-lines win gc pts))
          ptss))))))

(define-interactive (instantiate-script view name)
  (interactive (owner) (minibuffer <string> "Script: "))
  (let* ((parent (page-contents (view-page (underlying-object view))))
         (g (make <script-graphic>
                  name: name
                  shader: (get-shader name)
                  graphic-bounding-box: $zero-rect
                  parent-object: parent
                  in-document: (in-document parent)
                  origin: (make-point 100 100))))
    ((get-script g) g 'init '())
    (update-bbox! g)
    (clear-all-areas (in-document view))
    (do-select view g 0)
    g))

(define *shader-library* (make-string-table))

;;;
;;;  Create code that will initialize one instance variable
;;;

;;;
;;;  Build a scheme expr for constructing the given initial value,
;;;  including normalizing initial values expressed in arbitrary units
;;;

(define (script-init-expr type init)
  (case type
    ((<number:length>)
     (list 'quote (quantity/ init 1pt)))
    ((<font>)
     (list 'quote
           (apply get-text-font 
                  (map (lambda (elem)
                         (cond
                          ((symbol? elem)
                           (symbol->string elem))
                          (else
                           elem)))
                       init))))
    ((<fill-style>)
     (list 'quote (list init)))
    ((<stroke-style>)
     (bind ((color width (if (pair? init)
                             (list->values init)
                             (values init 1pt))))
       (list 'quote (list color (quantity/ width 1pt)))))
    ((<point>)
     (if (pair? init)
         (cons 'make-point init)
         (list 'quote init)))
    ((<string> <real> <boolean> <number>)
     ;; these objects are self-evaluating... need do nothing
     init)
    (else
     (if (pair? type)
         (case (car type)
           ((list)
            (if (null? init)
                ''()
                (let ((s (map (lambda (sub)
                                (script-init-expr (cadr type) sub))
                              init)))
                  (if (every? (lambda (sx)
                                (and (pair? sx)
                                     (eq? (car sx) 'quote)))
                              s)
                      (list 'quote (map cadr s))
                      (cons 'list s)))))
           (else
            (em "define-instance-variable: unknown compound spec ~s" type)))
         (em "define-instance-variable: unknown type spec ~s" type)))))

(define (gen-initializer1 iv-spec)
  (if (not (= (length iv-spec) 4))
      (em "define-instance-variable: not up to snuff: ~s" iv-spec))
  (let ((name (cadr iv-spec))
        (init (caddr iv-spec))
        (type (cadddr iv-spec)))
    ;;  stitch together the actual code
    `(set-property! self 
                    ',name 
                    (let ((initial (assq ',name (car %args))))
                      (if initial
                          (cadr initial)
                          ,(script-init-expr type init))))))

;;;
;;;  Create code that will initialize all of the instance variables
;;;

(define (gen-initializer spec)
  (cons 'begin (map gen-initializer1 (collect-iv-decls spec))))

(define (collect-iv-decls spec)
  (select (lambda (s)
            (eq? (car s) 'define-instance-variable))
          spec))

;;;  Create fragments of the big `let-syntax' that alias parameter
;;;  names to a `get-property' call

(define (gen-instance-var-decls spec)
  (map (lambda (iv-spec)
         (let ((name (cadr iv-spec)))
           `(,name (else (get-property self ',name)))))
       (collect-iv-decls spec)))

(define (gen-procedure-decls spec)
  (deep-transform-setters
   (map cadr (collect-iv-decls spec))
   (map (lambda (proc-spec)
          (cons 'define (cdr proc-spec)))
        (select (lambda (s)
                  (eq? (car s) 'define-procedure))
                spec))))

(define (deep-transform-setters namelist code)
  (if (pair? code)
      (if (and (eq? (car code) 'set!)
               (memq (cadr code) namelist))
          ;; do the transform
          `(%set-attribute ',(cadr code) ,(caddr code))
          (map (curry deep-transform-setters namelist) code))
      code))

(define (gen-syntax-decls spec)
  (append (gen-instance-var-decls spec)
          *std-decls*))
  
;; adjust the coordinate system so that `from' is at 0,0
;; and `to' is at w,0 where w = |to - from|.  i.e., do a rotation
;; and a translation, but not a scale.  Returns w

(define (lay-flat device (from <point>) (to <point>))
  (translate device from)
  (let* ((x1 (- (x to) (x from)))
         (y1 (- (y to) (y from)))
         (w (sqrt (+ (* x1 x1) (* y1 y1))))
         (c (/ x1 w))
         (s (/ y1 w)))
    (concat device (vector->affine-transform (vector c s (- s) c 0 0)))
    w))

(define *std-decls*
  '(
    ;;----------------------------------------------------------------------
    (%set-attribute (syntax-form (n v)
                      (set-property! self n v)))
    ;;----------------------------------------------------------------------
    (translate (syntax-form (s)
                 (translate (car %args) s))
               (syntax-form (tx ty)
                 (translate (car %args) (make-point tx ty))))
    (scale (syntax-form (s)
             (concat (car %args)
                     (scale $identity-transform (make-point s s))))
           (syntax-form (sx sy)
             (concat (car %args)
                     (scale $identity-transform (make-point sx sy)))))
    ;; adjust the coordinate system so that `from' is at 0,0
    ;; and `to' is at W,0
    (lay-flat (syntax-form (from to)
                (lay-flat (car %args) from to)))
    (show (syntax-form (text)
            (show (car %args) text)))
    (setfont (syntax-form (font)
               (setfont (car %args) 
                        (if (instance? font <text-font>)
                            font
                            (apply get-text-font font)))))
    (gsaved (syntax-form item
              (with-gstate-saved (car %args)
                                 (lambda () (begin . item)))))
    (setcolor (syntax-form (c)
                (let ((d (car %args)))
                  (cond
                   ((real? c)
                    (setcolor d (device-color d (list 'gray c))))
                   (else
                    (setcolor d (device-color d c))))))
              (syntax-form (r g b)
                (let ((d (car %args)))
                  (setcolor d (device-color d (list 'rgb r g b))))))
    (moveto (syntax-form (x y)
              (moveto (car %args) (make-point x y)))
            (syntax-form (at)
              (moveto (car %args) at)))
    (rectpath (syntax-form (x y w h)
                (let ((d (car %args)))
                  (moveto d (make-point x y))
                  (lineto d (make-point x (+ y h)))
                  (lineto d (make-point (+ x w) (+ y h)))
                  (lineto d (make-point (+ x w) y))
                  (closepath d))))
    (outline/rect (syntax-form (x y w h)
                    (let ((p (car %args)))
                      (format #t "outline/rect ~s\n" p)
                      (p (vector (make-point x y)
                                 (make-point x (+ y h))
                                 (make-point (+ x w) (+ y h))
                                 (make-point (+ x w) y)
                                 (make-point x y))))))
    (outline/line (syntax-form (x0 y0 x1 y1)
                    (let ((p (car %args)))
                      (format #t "outline/line ~s\n" p)
                      (p (vector (make-point x0 y0)
                                 (make-point x1 y1))))))
    (closepath (syntax-form ()
                 (closepath (car %args))))
    (lineto (syntax-form (x y)
              (lineto (car %args) (make-point x y)))
            (syntax-form (at)
              (lineto (car %args) at)))
    (arc (syntax-form (cx cy r a1 a2)
           (arc (car %args) (make-point cx cy) r a1 a2))
         (syntax-form (c r a1 a2)
           (arc (car %args) c r a1 a2)))
    (arcn (syntax-form (cx cy r a1 a2)
            (arcn (car %args) (make-point cx cy) r a1 a2))
          (syntax-form (c r a1 a2)
            (arcn (car %args) c r a1 a2)))
    (fill (syntax-form ()
            (fill (car %args)))
          (syntax-form (style)
            (fill-with-style (car %args) style)))
    (stroke (syntax-form ()
              (stroke (car %args)))
            (syntax-form (style)
              (stroke-with-style (car %args) style)))
    (handle (syntax-form (x y)
              (show-handle (car %args) (make-point x y)))
            (syntax-form (at)
              (show-handle (car %args) at)))
    (handlef (syntax-form (x y n)
               (dm "handle (~d,~d) [~d] => ~s" x y 0 n)
               (show-handle (car %args) (make-point x y))))
               
    ;;----------------------------------------------------------------------
    ))

(define (transform-shader spec)
  `(lambda (self %action . %args)
     (let-syntax ,(gen-syntax-decls spec)
       ,@(gen-procedure-decls spec)
       (case %action
         ((draw) (draw))
         ((outline) (outline))
         ((artwork) (artwork))
         ((handles) (handles))
         ((tweak) (apply tweak %args))
         ((init) ,(gen-initializer spec) self)))))

(define *shaders-dir* (pathname->string
                       (append-dirs
                        (current-absolute-directory)
                        (string->dir "../shaders"))))

(define (load-shader-script name)
  (let ((text '())
        (file (format #f "~a~a.scm" *shaders-dir* name)))
    ;;
    (with-objects-from-file
     file
     (lambda (item)
       (set! text (cons item text))))
    (values (reverse! text) file)))

(define-method reload-shader! ((self <script-shader>))
  (bind ((text file (load-shader-script (name self))))
    (if (not (string=? file (filename self)))
        (begin
          (dm "Shader `~a' is now coming from file: ~a" (name self) file)
          (set-filename! self file)))
    (if (not (equal? text (script self)))
        (begin
          (dm "Shader `~a' has a new script" (name self))
          (set-script! self text)
          (set-modification-time! self (time))))
    self))

(define (load-shader name)
  (bind ((text file (load-shader-script name))
         (shader (make <script-shader>
                       name: name
                       filename: file
                       modification-time: (time)
                       script: text)))
    (table-insert! *shader-library* name shader)
    shader))

(define (get-shader name)
  (or (table-lookup *shader-library* name)
      (load-shader name)))

;;;

(define (style-stroke-color style)
  (car style))

(define (style-stroke-width style)
  (cadr style))

;;; XXX need to port to using real style objects (from style.scm)

(define (stroke-with-style device style)
  (dm "stroke style: ~s" style)
  (with-gstate-saved
   device
   (lambda ()
     (setcolor device (device-color device (car style)))
     (setlinewidth device (cadr style))
     ;; XXX need style support for cap and join...
     (stroke device))))

(define (fill-with-style device style)
  (with-gstate-saved
   device
   (lambda ()
     (setcolor device (device-color device (car style)))
     (fill device))))

;;;

#|
(define-method externalize ((self <script-graphic>>))
  `(script ,(name self)
           ,@(...)))
|#

;;;

(define (paste-script-from-extern extern group (offset <size>))
  (apply (lambda (#key name instance-vars)
           (let ((g (make <script-graphic>
                          name: name
                          shader: (get-shader name)
                          graphic-bounding-box: $zero-rect
                          parent-object: group
                          in-document: (in-document group)
                          origin: (size->point offset))))
             ((get-script g) g 'init instance-vars)
             (update-bbox! g)
             g))
         (cdr extern)))

;;;

(global-set-key '(#\C-x #\C-i) instantiate-script)
(global-set-key '(#\C-x #\C-r) clear-script-cache)
