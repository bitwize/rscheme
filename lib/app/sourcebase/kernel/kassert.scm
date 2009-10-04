
(define-syntax (kassert expr)
  (let ((temp expr))
    (if (not temp)
	(error "kernel panic: ~s" (mquote expr)))
    temp))

;;
;;  a debug-level operation
;;

(define (phash label item)
   (format #t "~04x_~04x => (~a) ~s\n"
   	   (obj-high-bits item)
	   (obj-low-bits item)
	   label
	   item))
