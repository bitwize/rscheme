
;;;

(define (port->run->port cmd . args)
  (bind ((r1 w1 (pipe))
         (r2 w2 (pipe))
	 (p (run* cmd args (vector r1 w2 2))))
    (fd-close r1)
    (fd-close w2)
    (values (open-queued-output w1)
            (open-mbox-input-port r2)
            p
            (lambda () (fd-close w1))
            (lambda () (fd-close r2)))))

(define *charpath-cache* (make-table))

(define (get-charpath ps-font char)
  (let* ((key (cons ps-font char)))
    (or (table-lookup *charpath-cache* key)
        (let ((v (build-new-charpath ps-font char)))
          (table-insert! *charpath-cache* key v)
          v))))

(define *charpath-process* #f)

(add-image-save-hook! 
 (lambda ()
   (set! *charpath-process* #f)))

(define (get-charpath-process)
  (if (not *charpath-process*)
      (bind ((o i p oclose iclose (port->run->port "gs" "-q" 
                                                   "-sDEVICE=nullpage"
                                                   "-dNOPAUSE"
                                                   "-dBATCH"
                                                   "-")))
        (dm 210 "Started charpath gs daemon: ~s" p)
        (set! *charpath-process* (list o i p oclose iclose (make-object-table)))
        ;(set! o (current-output-port))
        ;;
        (format o "/str 1 string def\n")
        (format o "/out 300 string def\n")
        (format o "/datum {\n")
        (format o "  (\\() print\n")
        (format o "  print\n")
        (format o "  { ( ) print out cvs print } repeat\n")
        (format o "  (\\)\\n) print\n")
        (format o "} def\n")

        (format o "/getfontinfo {\n")
        (format o " findfont dup\n")
        (format o "(#\\(encoding #\\() print\n")
        (format o "/Encoding get\n")
        (format o "{\n")
        (format o "  ( \") print out cvs print (\") print\n")
        (format o "}\n")
        (format o "forall\n")
        (format o "(\\)) print\n")
        (format o "flush\n")
        (format o "\n")
        (format o "(charsyms #\\() print\n")
        (format o "%/Times-Roman findfont\n")
        (format o "/CharStrings get\n")
        (format o "{\n")
        (format o "    pop ( \") print out cvs print (\") print\n")
        (format o "}\n")
        (format o "forall\n")
        (format o "(\\)\\)) print flush\n")
        (format o "} def\n")

        (format o "/chp { false newpath 0 0 moveto charpath (\\() print \n" string)
        (format o "{ 2 (moveto) datum }\n")
        (format o "{ 2 (lineto) datum }\n")
        (format o "{ 6 (curveto) datum }\n")
        (format o "{ 0 (closepath) datum }\n")
        (format o "pathforall (\\)) print flush } def\n")
        ;;
        (format o "/glyphfont {\n")
        (format o "  /glyphname exch def\n")
        (format o "  dup length dict begin\n")
        (format o "  { 1 index /FID ne {def} {pop pop} ifelse } forall\n")
        (format o "  /Encoding 256 array def\n")
        (format o "  0 1 255 { Encoding exch /.notdef put } for\n")
        (format o "  Encoding 33 glyphname put\n")
        (format o "  currentdict end\n")
        (format o "} def\n")
        ))
  (list->values *charpath-process*))

(define-constant $standard-font-scale (scale (make-affine-transform) (/ 1000)))

(define (download-1-font o font-entry)
  (if (get-property font-entry 'standard-pdf #f)
      (values)
      (let* ((pdf-data (get-font-pdf-data font-entry))
             (fontfile (if pdf-data
                           (cadr (memq 'content: (cdr pdf-data)))
                           (em 317 "No font file available for ~s" 
                               (postscript-name font-entry)))))
        (dm 309 "Loading font ~s (~d bytes)\n" 
            font-entry 
            (string-length fontfile))
        (write-string o fontfile)
        (flush-output-port o))))

(define (ensure-font-loaded ps-font)
  (bind ((o i p oclose iclose fonttab (get-charpath-process)))
    (if (table-lookup fonttab ps-font)
        (values)
        (let ((db (current-font-database))
              (l (query-font-database postscript: ps-font)))
          (if (pair? l)
              (download-1-font o (car l))
              (em 318 "No font entry known for ~s" ps-font))
          (table-insert! fonttab ps-font #t)
          (values)))))

(define (get-font-info ps-font)
  (ensure-font-loaded ps-font)
  (bind ((o i p oclose iclose fonttab (get-charpath-process)))
    (format o "/~a getfontinfo\n" ps-font)
    (flush-output-port o)
    (let ((info (read i)))
      info)))
  
(define (build-new-charpath ps-font char)
  (transform (build-new-charpath* ps-font char) $standard-font-scale))

(define (build-new-charpath* ps-font char)
  (ensure-font-loaded ps-font)
  (bind ((o i p oclose iclose (get-charpath-process)))
    (if (string? char)
        (begin
          (format o "/MyGlyphFont /~a findfont /~a glyphfont definefont" 
                    ps-font char)
          (format o "/MyGlyphFont"))
        (format o "/~a" ps-font))
    (format o " findfont 1000 scalefont setfont\n" ps-font)
    (if (string? char)
        (format o "(!) ")
        (begin
          (format o "str 0 ~d put str " (char->integer char))))
    (format o "chp\n")
    (flush-output-port o)
    ;; the operands come out in reverse order...
    (let ((item (map (lambda (op)
                       (cons (car op) (reverse (cdr op))))
                     (read i))))
      (dm 101 "Built charpath for ~s ~s (~d operators)" ps-font char (length item))
      (eval-postscript-path item))))
                 
(define (text->path ps-font size string)
  (bind ((o i p oclose iclose (port->run->port "gs" "-q" 
                                               "-sDEVICE=nullpage"
                                               "-dNOPAUSE"
                                               "-dBATCH"
                                               "-")))
    ;(set! o (current-output-port))
    (format o "/~a findfont ~d scalefont setfont\n" ps-font size)
    (format o "0 0 moveto\n")
    (format o "/str 20 string def\n")
    (format o "/datum {\n")
    (format o "  (\\() print\n")
    (format o "  print\n")
    (format o "  { ( ) print str cvs print } repeat\n")
    (format o "  (\\)\\n) print\n")
    (format o "} def\n")
    (format o "(~a) false charpath\n" string)
    (format o "{ 2 (moveto) datum }\n")
    (format o "{ 2 (lineto) datum }\n")
    (format o "{ 6 (curveto) datum }\n")
    (format o "{ 0 (closepath) datum }\n")
    (format o "pathforall\n")
    (flush-output-port o)
    (close-output-port o)
    (oclose)
    (let loop ((r '()))
      (let ((item (read i)))
        (if (eof-object? item)
            (begin
              (close-input-port i)
              (iclose)
              (reverse r))
            (loop (cons item r)))))))
        
(define (load-font-outlines font)
  (let* ((psname (postscript-name font))
         (info (get-font-info psname))
         (enc (vector-ref info (vassq 'encoding info)))
         (charsyms (vector-ref info (vassq 'charsyms info)))
         (outlines (make-symbol-table)))
    ;;
    (vector-for-each
     (lambda (name)
       (if (not (string=? name ".notdef"))
           (table-insert! outlines 
                          (string->symbol name)
                          (build-new-charpath* psname name))))
     (sort charsyms string<?))
    ;;
    (set-font-encoding! font (vector-map (lambda (name)
                                           (if (string=? name ".notdef")
                                               #f
                                               (string->symbol name)))
                                         enc))
    (set-font-outlines! font outlines)
    (values)))
  
(define (load-some-font-outlines font-db limit)
  (let loop ((l (query-font-database database: font-db))
             (n limit))
    (if (null? l)
        #t                                      ; all done
        (if (has-font-outlines? (car l))
            (loop (cdr l) n)
            (if (<= n 0)
                #f                              ; more work to do later
                (begin
                  (load-font-outlines (car l))
                  (loop (cdr l) (- n 1))))))))
