;;;  First, create the base module and import rs.lang (for 'define-syntax')

(define-module graphics.script ()
  (&module (import rs.lang
                   graphics.afm)))

;;;  do all this munching and crunching in a separate namespace

(define-module graphics.script.manager ()
  ;;
  (&module
   (import usual-inlines
           tables
           repl
           paths
           mlink
           rs.util.reify
           graphics.device
           graphics.geometry))


;;;
;;;  Build, more or less by hand, the "graphic.script" module
;;;  which allows graphic operators to be used without an explicit
;;;  device argument
;;;


;;;
;;;  Use reflection to construct a `graphics.script' module
;;;  which, generally speaking, contains bindings for the graphics.device
;;;  operators but act on the current device instead of taking an argument
;;;


;;;  Operators which we only forward (although with a device prefix)
;;;  (these definitions can come from either graphics.device,
;;;   graphics.text.layout, or graphics.styles)

(define *shallow-operators* '(currentpoint
                              setdash
                              setlinewidth
                              curveto
                              style-apply
                              stroke fill
                              rectstroke rectfill
                              scale rotate translate
                              arc arcn
                              closepath
                              newpath
                              show xshow 
                              currentfont
                              setfont
                              clip
                              setlinejoin
                              setlinecap
                              setcolor
                              device-color
                              concat
                              composite-image
                              with-gstate-saved
                              starttext
                              endtext
                              startpage
                              endpage
                              include-eps
                              ;; gaudy (abstract) stuff
                              roundrectstroke
                              arrowstroke
                              draw-long-brace
                              draw-dimen-labels
                              ;; more abstract stuff
                              areastroke
                              areafill
                              areapath
                              areashow
                              ;; text layout stuff [from graphics.text.layout]
                              layout-text
                              ))

;;; Operators which we explicitly define new syntax for

(define *deep-operators* '(moveto lineto))

;;;
;;;  Stuff that we import from graphics.device but don't reexport
;;;

(define *private-from-graphics-device* '(current-graphics-device))
;(define *private-from-graphics-device* '())

;;;
;;;  Stuff that we define more-or-less on our own
;;;

(define *new-operators* '(gsaved cshow rshow))


(&module
 (load "annotation.scm"))

(&module
 (export annotation set-annotation-hook!))

;;;

(define *rename-from-elsewhere* '((graphics.geometry
                                   (rotate geom:rotate)
                                   (scale geom:scale)
                                   (translate geom:translate)
                                   :rest)
                                  ;;
                                  (graphics.script.manager
                                   annotation)
                                  ;;  these are being exported w/o change
                                  (graphics.device
                                   current-graphics-device
                                   ;;
                                   <font-style>
                                   <stroke-style>
                                   <fill-style>
                                   <solid-paint-style>
                                   <pattern-paint-style>
                                   ;;
                                   define-stroke-style
                                   define-fill-style
                                   define-font-style
                                   define-solid-paint-style
                                   define-pattern-paint-style)))

;;;
;;;  Now, import the graphics.device procedures with a "g:" prefix
;;;

(let ((src1 (table (top-level-envt (get-module 'graphics.device))))
      (src2 (table (top-level-envt (get-module 'graphics.text.layout))))
      (src3 (table (top-level-envt (get-module 'graphics.styles))))
      (dst (table (top-level-envt (get-module 'graphics.script)))))
  ;;
  (for-each
   (lambda (operator)
     (table-insert! dst
                    (symbol-append "g:" operator)
                    (or (table-lookup src1 operator)
                        (table-lookup src2 operator)
                        (table-lookup src3 operator))))
   (append *shallow-operators*
           *deep-operators*
           *private-from-graphics-device*)))



;;;
;;;  Import all graphics.geometry procedures, but also with a "g:" prefix
;;;

(define (import-all-with-prefix src-module-name prefix)
  (let* ((m (get-module src-module-name))
         (src (table (top-level-envt m)))
         (dst (table (top-level-envt (get-module 'graphics.script)))))
    ;;
    (for-each
     (lambda (operator)
       (table-insert! dst 
                      (symbol-append prefix operator)
                      (table-lookup src operator)))
     (key-sequence (module-exports m)))))
  
(import-all-with-prefix 'graphics.geometry "g:")
(import-all-with-prefix 'graphics.fontmgr "g:")
#|
(let ((src (table (top-level-envt (get-module 'graphics.geometry))))
      (dst (table (top-level-envt (get-module 'graphics.script)))))
  ;;
  (for-each
   (lambda (operator)
     (table-insert! dst 
                    (symbol-append "g:" operator)
                    (table-lookup src operator)))
   (key-sequence (module-exports (get-module 'graphics.geometry)))))
|#

;;;
;;;  For shallow operators, define the pass-through syntax
;;;

(let ((dst (top-level-envt (get-module 'graphics.script))))
  ;;
  (for-each
   (lambda (op)
     (eval-in-envt `(define-syntax (,op . args)
                      (,(symbol-append "g:" op)
                       (g:current-graphics-device)
                       . args))
                   dst))
   *shallow-operators*))

)

;;;


(define-module-extend graphics.script ()
  (define-syntax moveto
    (syntax-form (x y)
      (g:moveto (g:current-graphics-device) (g:make-point x y)))
    (syntax-form (p)
      (g:moveto (g:current-graphics-device) p)))
  ;;
  (define-syntax (gsaved . body)
    (g:with-gstate-saved
     (g:current-graphics-device)
     (lambda ()
       (begin . body))))
  ;;
  (define (re-x-show x-fn y str with-frame)
    (let* ((d (g:current-graphics-device))
           (fnt (g:currentfont d))
           (x (x-fn (string-width fnt str))))
      ;;
      (if with-frame
          (with-frame (g:offset-rect
                       (string-bbox (g:font-metrics (g:currentfont d)) str)
                       x y)))
      ;;
      (g:moveto d (g:make-point x y))
      (g:show d str)))
    
  (define (cshow x y str #key (with-frame default: #f))
    (re-x-show (lambda (w) (- x (/ w 2))) y str with-frame))
  ;;
  (define (rshow x y str #key (with-frame default: #f))
    (re-x-show (lambda (w) (- x w)) y str with-frame))
  ;;
  (define-syntax lineto 
    (syntax-form (x y)
      (g:lineto (g:current-graphics-device) (g:make-point x y)))
    (syntax-form (p)
      (g:lineto (g:current-graphics-device) p))))


(define-module-extend graphics.script.manager ()

;;
;;  Export them all

(let ((exp (module-exports (get-module 'graphics.script)))
      (dst (table (top-level-envt (get-module 'graphics.script)))))
  (for-each
   (lambda (op)
     (table-insert! exp op (or (table-lookup dst op)
                               (error "~s: missing from graphics.script" op))))
   (append *shallow-operators*
           *deep-operators*
           *new-operators*)))

;;; this is kind of illegal:  
;;;  graphics.script is exporting stuff from graphics.geometry
;;;  regardless of whether or not it imported it (although it did)


(let ((exp (module-exports (get-module 'graphics.script))))
  (for-each
   (lambda (group)
     (let* ((src-m (get-module (car group)))
            ;; note that we don't want to access the raw table
            ;; directly because some bindings we may be getting
            ;; from the module, but they are actually _imported_
            ;; into that one (e.g., `size' in graphics.geometry)
            (src-env (top-level-envt src-m))
            (did (make-symbol-table)))
       ;;
       (define (xfer1 src-name #optional (dst-name default: src-name))
         (table-insert! exp
                        dst-name
                        (or (with-module compiler (lookup src-env src-name))
                            (error "~s: not defined in ~s"
                                   src-name
                                   (car group)))))
       ;;
       (for-each
        (lambda (xfer)
          (cond
           ((eq? xfer ':rest)
            (for-each xfer1
                      (select (lambda (k)
                                (not (table-lookup did k)))
                              (key-sequence
                               (module-exports src-m)))))
           ((pair? xfer)
            (table-insert! did (car xfer) #t)
            (xfer1 (car xfer) (cadr xfer)))
           ((symbol? xfer)
            (table-insert! did xfer #t)
            (xfer1 xfer))
           (else
            (error "Bad module transfer form: ~s" xfer))))
        (cdr group))))
   *rename-from-elsewhere*))

(&module (load "gscript.scm"))
 
)
