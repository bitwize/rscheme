
(define-thread-var *current-graphics-device*)

(define (current-graphics-device) *current-graphics-device*)

(define (with-graphics-device (d <graphics-device>) thunk)
  (thread-let ((*current-graphics-device* d))
    (thunk)))

;;;

(define (compute-graphics-bbox thunk)
  (let* ((dev (open-bbox-device)))
    (thread-let ((*current-graphics-device* dev))
      (thunk))
    (bind ((b (close-graphics-device dev))
           (llx lly urx ury (ps-integer-bbox b)))
      (bbox-rect llx lly urx ury))))

;;;

(define (implicit-graphics-file-type file)
  (let ((extn (with-module paths (extension (string->file file)))))
    ;;
    (cond
     ((string-ci=? extn "eps") 'eps)
     ((string-ci=? extn "ps") 'postscript)
     ((string-ci=? extn "pdf") 'pdf)
     (else 
      (error "Can't decide graphics type from file extension ~s in ~s" 
             extn file)))))
    

(define (with-graphics-file file thunk #key (bbox default: #f)
                            (type default: #f))
  (let* (((type <symbol>) (or type 
                              (implicit-graphics-file-type file)))
         (result '())
         (dev (case type
                ((eps)
                 ;; if the type is EPS, return the bbox also
                 (set! result (list
                               (or bbox
                                   (compute-graphics-bbox thunk))))
                 (open-eps-device file (car result)))
                ((postscript)
                 (open-ps-device file))
                ((pdf)
                 (open-pdf-device file))
                (else
                 (error "Don't know how to output to type ~s graphics device" 
                        type)))))
    ;;
    (thread-let ((*current-graphics-device* dev))
      (thunk)
      (close-graphics-device dev))
    (list->values result)))
