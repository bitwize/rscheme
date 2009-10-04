(define-style 'red (color-style color: (make-color red: 1 green: 0 blue: 0)))

(define-style 'red-fill (fill-style basis: 'default-fill
                                    color: 'red))


(define-interactive (apply-style in-view objects style)
  (interactive (owner) (selection) (minibuffer <symbol> "Style: "))
  (let ((the-style (table-lookup *style-dictionary* style)))
    (if (null? objects)
        (set-current-style! the-style)
        (for-each
         (lambda ((item <graphic-object>))
           (apply-style-to-graphic item the-style))
         objects))
    (clear-all-areas (in-document in-view))
    (set-need-to-recompute-handles! in-view #t)))

(define-mm-generic-function apply-style-to-graphic)

;;

(define (get-current-style name)
  (let* (((ss <style-set>) (style-set (current-client)))
         (s (case name
              ((stroke) (style-set-stroke ss))
              ((fill) (style-set-fill ss))
              (else (em "Request for unknown current style ~s" name)))))
    (if (symbol? s)
        (table-lookup (style-dictionary (current-client)) s)
        s)))

(define (set-current-style! style)
  (let (((ss <style-set>) (style-set (current-client))))
    (cond
     ((instance? style <stroke-style>) 
      (set-style-set-stroke! ss style))
     ((instance? style <fill-style>) 
      (set-style-set-fill! ss style)))))
