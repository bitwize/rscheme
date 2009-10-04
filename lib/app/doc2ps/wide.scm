(load "bookstyles.scm")

(define-page-style wide-start-page (letter)
   content: (let ((b (trim-rect (media-box 'letter)
                                left: 0.5in
                                top: 0.5in
                                right: 0.5in
                                bottom: 0.5in)))
              (list
               (make <text-frame>
                     flow: 'a
                     frame: (trim-rect b bottom: 9.3in #|9.5in|#))
               (make <text-frame>
                     flow: 'a
                     frame: (trim-rect b top: 0.75in bottom: 9in))
               (make <text-frame>
                     flow: 'a
                     frame: (trim-rect b top: 1.25in))
               (make <line-graphic>
                     line-start: (make-point 0.5in (- 9.5in 6))
                     line-end: (make-point 8in (- 9.5in 6)))
               (make <line-graphic>
                     line-start: (make-point 0.5in (- 9.5in 8))
                     line-end: (make-point 8in (- 9.5in 8))))))

(define-page-style wide-single-page (letter)
   content: (let ((b (trim-rect (media-box 'letter)
                                left: 0.5in
                                top: 0.5in
                                right: 0.5in
                                bottom: 0.5in)))
              (list
               (make <text-frame>
                     flow: 'a
                     frame: (trim-rect b top: 0.5in)))))


(define *wide-page-sequence*
  (make <simple-page-sequence>
        initial-page-styles: '(wide-start-page)
        repeat-page-styles: '(wide-single-page)
        blank-page-style: 'intentionally-blank-page-body))

(table-insert! *page-format-definitions* "wide" *wide-page-sequence*)
