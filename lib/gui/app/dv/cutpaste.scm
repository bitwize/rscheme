
(define-method unlink-graphic ((self <graphic-object>))
  (let ((p (parent-object self)))
    (set-group-contents! p (delq self (group-contents p)))))

(define *reflection* #f)

;;;
;;;  put the selection into a variable that we
;;;  can easily get to from the interactive
;;;  command loop
;;;

(define-interactive (reflect sel)
  (interactive (selection))
  (set! *reflection* sel))

(define-interactive (cut view sel)
  (interactive (owner) (selection))
  (set-clipboard! (map externalize sel))
  (clear-current-selection! view)
  (set-paste-offset! view (make-size 10 -10))
  (for-each (lambda ((thing <graphic-object>))
	      (unlink-graphic thing))
	    sel)
  ; in case we immediately say "paste", it'll go to the same spot
  (set-paste-offset! view $zero-size)
  (set-need-to-recompute-handles! view #t)
  (clear-all-areas (in-document view)))

(define-interactive (copy view)
  (interactive (owner))
  (em 666 "copy: not implemented"))

(define (paste-in view items)
  (clear-current-selection! view)
  (let (((group <root-group>) (page-contents 
			       (view-page (underlying-object view)))))
    (for-each (lambda (descr)
		(add-to-current-selection!
		 view
		 (paste-from-extern descr group (paste-offset view))))
	      items))
  (set-paste-offset! view (size+ (paste-offset view) (make-size 10 -10)))
  (set-need-to-recompute-handles! view #t)
  (clear-all-areas (in-document view)))

(define-interactive (paste view)
  (interactive (owner))
  (paste-in view (clipboard)))

(define (clipboard)
  (car (client-kill-ring (current-client))))

(define (set-clipboard! descr)
  ; keep only the topmost item... need to figure out
  ; how the "ring" part works, and if we want it
  (set-client-kill-ring! (current-client) (list descr)))
 
(graphic-set-key #\C-w cut)
(graphic-set-key #\C-y paste)

;;;

(define-method paste-from-extern ((extern <pair>) (group <group>) offset)
  (case (car extern)
    ((box)
     (paste-box-from-extern extern group offset))
    ((line)
     (paste-line-from-extern extern group offset))
    ((path)
     (paste-path-from-extern extern group offset))
    ((text)
     (paste-text-from-extern extern group offset))
    ((script)
     (paste-script-from-extern extern group offset))
    ((group)
     (for-each
      (lambda (sub)
	(paste-from-extern sub group offset))
      (cdr extern)))
    (else (em 921 "Unrecognized externalization: ~s" extern))))

