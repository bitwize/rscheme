,(use paths
      graphics.script.manager ; implicitly builds graphics.script, too
      graphics.device
      graphics.geometry)

,(use rs.sys.threads.manager)

(load "uptodate.scm")

(define (graphic-dest-file (file <file-name>) entry-point)
  (let ((fn (if (eq? entry-point 'draw)
                file
                (append-path
                 (file-directory file)
                 (string->file (~ "~a_~a" (filename file) entry-point))))))
    (extension-related-path fn "eps")))

(define (get-meta-data s entry-point)
  (let* ((metadata (handler-case
                    (eval-in-graphic-script s '(meta))
                    ((<condition> condition: c)
                     (format (current-error-port)
                             "*** Could not eval (meta): ~a" c)
                     '())))
         (entry (assq entry-point metadata)))
    (if entry
        (cdr entry)
        (begin
          (format (current-error-port)
                  "*** No entry in metadata for `~s'\n" entry-point)
          '()))))
        

(define (generate-graphic (file <file-name>) 
                          #key (entry-point default: 'draw)
                               (force default: #f parameter: force?)
                               (arguments default: '())
                               (print-mode default: 'ps))
                          
  (let* ((src (pathname->os-path file))
         (dstf (graphic-dest-file file entry-point))
         (dst (pathname->os-path dstf)))
    ;;
    (format #t "[~a" (filename file))
    (flush-output-port (current-output-port))
    ;;
    (if (and (not force?) (up-to-date? src dst))
        (begin
          (format #t " ok]")
          (flush-output-port (current-output-port))
          (get-eps-bbox dstf))
        (let ((s #f))
          (format #t " renew")
          (flush-output-port (current-output-port))
          ;;
          (set! s (load-graphic-script file))
          ;;
          (let* ((meta (get-meta-data s entry-point))
                 ((proc <function>) (eval-in-graphic-script s entry-point))
                 (thunk (lambda ()
                          (apply proc arguments)))
                 (r (case print-mode
                      ;; full-blown print output, suitable for `lpr'
                      ((ps) (gen/raw-print dstf thunk))
                      ;; scaled eps, suitable for eps2png
                      ((scaled) (gen/scaled-eps dstf thunk))
                      ;; proof sheet
                      ((proof-sheet)
                       (gen/proof-sheet dstf thunk meta))
                      ;; unscaled eps, suitable for inclusion in print article
                      ((eps) (gen/eps dstf thunk))
                      ;;
                      (else
                       (error "Unknown print mode: ~s" print-mode)))))
            ;;
            (format #t "]")
            (flush-output-port (current-output-port))
            r)))))

(define *design-sizes*
  `((small ,(make-size (* 3 72) (* 2 72)))))

(define (meta-design-size meta)
  (cond
   ((assq 'design-size meta)
    => (lambda (p)
         (let ((v (cadr p)))
           (cond
            ((symbol? v)
             (cond
              ((assq v *design-sizes*) => cadr)
              (else (error "Unknown symbolic design size `~s'" v))))
            ((and (list? v) (= (length v) 2))
             (make-size (* (car v) 72) (* (cadr v) 72)))
            (else
             (error "Unknown design size expression: ~s" v))))))
   (else
    #f)))

(define (thunk->bbox thunk)
  (let ((b (open-bbox-device)))
    (with-graphics-device b thunk)
    (close-graphics-device b)))

(define (proof-zoom (use <rect>) (goal <rect>))
  (let ((d (current-graphics-device))
        (s (min (/ (size-width goal) (size-width use))
                (/ (size-height goal) (size-height use)))))
    (translate d (make-point (center-x goal)
                             (center-y goal)))
    (scale d s s)
    (translate d (make-point (- (center-x use))
                             (- (center-y use))))))


(define (landscape)
  (let ((d (current-graphics-device)))
    (translate d (make-point (* 8.5 72) 0))
    (rotate d 90)))

(define (gen/proof-sheet dstf thunk meta)
  (let* ((b (thunk->bbox thunk))
         (s (meta-design-size meta))
         (sr (and s (make-rect (origin-x b)
                               (origin-y b)
                               (dx s)
                               (dy s))))
         (goal (make-rect 36 36 (* 10 72) (* 7.5 72))))
    ;;
    (with-graphics-file
     (pathname->os-path (extension-related-path dstf "ps"))
     (lambda ()
       (let ((d (current-graphics-device)))
         (startpage d)
         (landscape)
         (rectstroke d goal)
         (proof-zoom (if sr (union-rect sr b) b) goal)
         (with-gstate-saved
          d
          (lambda ()
            (setcolor d (device-color d '(cmyk 1 0 0 0)))
            (rectstroke d b)
            (if sr
                (begin
                  (setcolor d (device-color d '(cmyk 0 1 1 0)))
                  (rectstroke d sr)))))
         (thunk)
         (endpage (current-graphics-device)))))))
  
(define (gen/raw-print dstf thunk)
  (with-graphics-file
   (pathname->os-path (extension-related-path dstf "ps"))
   (lambda ()
     (startpage (current-graphics-device))
     (thunk)
     (endpage (current-graphics-device)))))

(define (gen/eps dstf thunk)
  (with-graphics-file
   (pathname->os-path dstf)
   thunk
   type: 'eps))

(define (gen/scaled-eps dstf thunk)
  (with-graphics-file
   (pathname->os-path dstf)
   (lambda ()
     ;; draw them to PNGs 75% bigger
     (scale (current-graphics-device) 1.75 1.75)
     (thunk))
   type: 'eps))

;;;

(define (tg #optional (src default: "pagesinmem.scm"))
  (generate-graphic
   (append-path (current-directory) (string->file src))
   force: #t
   print-mode: 'scaled)
  (newline))
   

(define (tgp #optional (src default: "pagesinmem.scm"))
  (generate-graphic
   (append-path (current-directory) (string->file src))
   force: #t
   print-mode: 'ps)
  (newline))

(define (proof-sheet src)
  (generate-graphic
   (append-path (current-directory) (string->file src))
   print-mode: 'proof-sheet)
  (newline))

(define (teps src)
  (generate-graphic
   (append-path (current-directory) (string->file src))
   print-mode: 'eps)
  (newline))

(define (force-teps src)
  (generate-graphic
   (append-path (current-directory) (string->file src))
   force: #t
   print-mode: 'eps)
  (newline))

(define (files-of-type dir type)
  (select (lambda (f)
            (equal? (extension (string->file f)) type))
          (scandir dir)))

(define (gv name)
  (run "gv" "--watch" name))

;;;
;;;

(define *current-preview* #f)

(define (re-preview file)
  (with-module
      unixm
    (if *current-preview*
        (kill (process-id (cdr *current-preview*)) 2))
    (set! *current-preview*
          (cons file (run "gv" "--watch" file)))))

(define (preview src)
  (teps src)
  (let ((epsf (pathname->os-path
               (extension-related-path
                (string->file src)
                "eps"))))
    (if (or (not *current-preview*)
            (not (equal? (car *current-preview*) epsf)))
        (re-preview epsf))))


