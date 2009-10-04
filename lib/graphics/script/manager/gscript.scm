(define-class <graphic-script> (<object>)
  source
  top-level)

(define-method load-graphic-script ((self <file-name>))
  (load-graphic-script* self))

(define-method load-graphic-script ((self <string>))
  (load-graphic-script* (append-path (current-directory) 
                                     (string->file self))))

(define (load-graphic-script* (file <file-name>))
  (let ((m (make-user-initial)))
    ;;
    (use-in 'graphics.script m)
    (use-in 'graphics.fontmgr m)
    (use-in 'graphics.afm m)
    ;;
    (load-into m file)
    ;;
    (make <graphic-script>
          source: file
          top-level: m)))

(define (eval-in-graphic-script (self <graphic-script>) expr)
  (eval-in-envt expr (top-level self)))

(&module
 (export load-graphic-script
         eval-in-graphic-script))
 ;;
