(define tr-afm (get-afm "Times-Roman"))
(define tb-afm (get-afm "Times-Bold"))
(define ti-afm (get-afm "Times-Italic"))

(define (cf-afm cf)
  (case (car cf)
    ((plain) tr-afm)
    ((italic) ti-afm)
    ((bold) tb-afm)))

(define (cf-size cf)
  (cadr cf))

(define (t2)
  (let* ((lay (layout-line *long-text* '(plain 24)))
         (lines (map strip-trailing-whitespace
                     (insert-line-breaks lay line-width: 400))))
    ;;
    (gen-tests
     width: 400
     tabs: '(50)
     test-procs: (append
                  ;;
                  (map
                   (lambda (i l)
                     (lambda ()
                       (values 
                        (format #f "line[~d]" i)
                        (lambda ()
                          (let* ((l (fold-over-struts (no-stretch l)))
                                 (x (render-layout l)))
                            (format #t "~d ticmark\n" x))))))
                   (reverse (range (length lines)))
                   (reverse lines))
                  ;;
                  (list (lambda ()
                          (values "skip" (lambda ()))))
                  ;;
                  (map
                   (lambda (i l)
                     (lambda ()
                       (values 
                        (format #f "line[~d]" i)
                        (lambda ()
                          (let* ((dw (if (< i (- (length lines) 1))
                                         (- 400 (line-width l))
                                         0))
                                 (l (fold-over-struts
                                     (apply-stretch l dw)))
                                 (x (render-layout l)))
                            (format #t "~d ticmark\n" x))))))
                   (reverse (range (length lines)))
                   (reverse lines))
                  ))))

(define (t1)
  (gen-tests
   width: 200
   tabs: '(100)
   test-procs: (list
                (lambda ()
                  (values 
                   "xshow"
                   (lambda ()
                     (format #t "f1 setfont (Aya) [~j] xshow\n"
                             (xshow-x-list tr-afm 48.0 "Aya")))))
                (lambda ()
                  (values
                   "show"
                   (lambda ()
                     (format #t "f1 setfont (Ayo) show\n"))))
                (lambda ()
                  (values
                   "layout"
                   (lambda ()
                     (let ((x (render-layout
                               (no-stretch
                                (layout-line '(#\A #\y #\o #\space
                                                   (font (bold 48))
                                                   #\A #\y #\/ #\.)
                                             '(plain 48))))))
                       (format #t "~d ticmark\n" x)))))
                (lambda ()
                  (bind ((lay tw (layout-line *test-text* '(plain 24)))
                         (dw (- 200 tw)))
                    (values
                     (format #f "stretch(~d)" (+ dw 0.0))
                     (lambda ()
                       (let* ((l (fold-over-struts (apply-stretch lay dw)))
                              (x (render-layout l)))
                         (format #t "~d ticmark\n" x))))))
                (lambda ()
                  (values
                   "stretch(25)"
                   (lambda ()
                     (let* ((l (fold-over-struts
                                (apply-stretch
                                 (layout-line *test-text* '(plain 24))
                                 25)))
                            (x (render-layout l)))
                       (format #t "~d ticmark\n" x)))))
                (lambda ()
                  (values
                   "stretch(10)"
                   (lambda ()
                     (let* ((l (fold-over-struts
                                (apply-stretch
                                 (layout-line *test-text* '(plain 24))
                                 10)))
                            (x (render-layout l)))
                       (format #t "~d ticmark\n" x)))))
                (lambda ()
                  (values
                   "long"
                   (lambda ()
                     (let* ((l (fold-over-struts
                                (no-stretch
                                 (layout-line *long-text* '(plain 12)))))
                            (x (render-layout l)))
                       (format #t "~d ticmark\n" x)))))
                )))

(define *test-text* '("This is a "
                      (font (bold 24))
                      "test"))

(define *long-text* '((font (italic 24.0))
                      (space 50 0)
                      "RScheme"
                      (font (plain 24.0))
                      " is an object-oriented, extended version of the"
                      " Scheme dialect of Lisp, principally a merger of"
                      " concepts from the Scheme language (see "
                      (font (italic 24.0))
                      "Revised^4 Report on Scheme"
                      (font (plain 24.0))
                      ") and the Dylan language (see "
                      (font (italic 24.0))
                      "Dylan"
                      (font (plain 24.0))
                      ") which are it's intellectual progenitors."))


(define (gen-tests #key 
                   test-procs
                   width
                   tabs)
  (init-dps)
  (let loop ((y 36)
             (tp test-procs))
    (if (pair? tp)
        (bind ((label gen-body ((car tp))))
          (with-output-to-ps
           (lambda ()
             (format #t "(~a) 36 ~d ~d linestart\n" label y width)
             (for-each (lambda (t)
                         (format #t "~d tabtic\n" t))
                       tabs)
             (format #t "0 0 moveto\n")
             (gen-body)
             (format #t "lineend\n")))
          (loop (+ y 50) 
                (cdr tp))))))


(define (render-layout runs)
  (reduce 
   + 
   0
   (map
    (lambda (run)
      (format #t "/~a findfont ~d scalefont setfont\n"
              (get-property (cf-afm (car run)) 'FontName)
              (cadr (car run)))
      (let ((z (cdr run)))
        (if (and (pair? z)
                 (pair? (car z))
                 (eq? (caar z) 'strut))
            (begin
              (format #t "~d 0 rmoveto\n" (cadar z))
              (set! z (cdr z))))
        (let ((widths (map (lambda (ent)
                             (let ((tcw (+ (cadr ent) (caddr ent))))
                               (if (integer? tcw)
                                   tcw
                                   (exact->inexact tcw))))
                           z)))
          (format #t "(~a) [~j] xshow\n" 
                  (postscript-escape (list->string (map car z)) )
                  widths)
          (reduce + 0 widths))))
    runs)))

(define (postscript-escape str)
  (string-join ""
               (map (lambda (ch)
                      (case ch
                        ((#\() "\\(")
                        ((#\)) "\\)")
                        (else (string ch))))
                    (string->list str))))




(define (t0)
  (insert-line-breaks
   (cons '((plain 12) (strut 18))
         (layout-line '("This is a fairly long test, but not too long")
                      '(plain 12)))
   line-width: 72))

