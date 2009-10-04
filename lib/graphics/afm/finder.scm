
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;		afm font name -> file name translation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define font-xlate-pat (reg-expr->proc
			'(entire
			  (seq
			   (* space)
			   (save (+ (or alpha #\- #\_ digit)))
			   (+ space)
			   (save (+ (or alpha #\- #\_ digit)))
			   (* space)))))

;;;  These patterns match what I see in Ghostscript's Fontmap file

;;; "/Bar   (blech.pfb) ;"
;;; "/Baz   (baz.pfa) ;"

(define font-xlate-pat2 (reg-expr->proc
			'(entire
			  (seq
			   (* space)
			   #\/
			   (save (+ (or alpha #\- #\_ digit)))
			   (+ space)
			   #\(
			   (save (+ (or alpha #\- #\_ digit #\/)))
			   (? (seq #\. (+ (or alpha digit))))
			   #\)
			   (* space)
			   #\;
			   (* space)))))

;;; "/Foo   /Bar ;"

(define font-xlate-pat3 (reg-expr->proc
			'(entire
			  (seq
			   (* space)
			   #\/
			   (save (+ (or alpha #\- #\_ digit)))
			   (+ space)
			   #\/
			   (save (+ (or alpha #\- #\_ digit)))
			   (* space)
			   #\;
			   (* space)))))

(define (read-font-translation-line line)
  (bind ((s e from to (font-xlate-pat line)))
    (if s
	(values from to)
	(bind ((s e from to (font-xlate-pat2 line)))
	  (if s
	      (values from to)
	      (bind ((s e from to (font-xlate-pat3 line)))
		(if s
		    (values from (list to))
		    (values))))))))

(define (read-font-translation-file file-name)
  (if (file-exists? file-name)
      (call-with-input-file
	  file-name
	(lambda (port)
	  (let ((tbl (make-table string=? string->hash)))
	    (let loop ()
	      (let ((ln (read-line port)))
		(if (eof-object? ln)
		    tbl
		    (begin
		      (bind ((from to (read-font-translation-line ln)))
			(if from
			    (table-insert! tbl from to))
			(loop)))))))))
      #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;		      afm directories and paths
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-class <afm-directory> (<object>)
  (afm-directory type: <directory-name>)
  (translation init-value: #f)) ; #f=>none, string=>file, table=>map

(define (check-afm-file (self <afm-directory>) (file-name <string>))
  (let ((f (append-path (afm-directory self) 
			(string->file (string-append file-name ".afm")))))
    (if (file-exists? f)
	f
	#f)))

(define-method get-afm-file ((self <afm-directory>) (font-name <string>))
  ;; if the translation is a string, it specifies a file to read.
  ;; in that case, read in the file now and replace the translation
  ;; with the corresponding table
  (if (string? (translation self))
      (let ((f (append-path (afm-directory self)
                            (string->file (translation self)))))
        (set-translation! 
         self 
         (if (file-exists? f)
             (read-font-translation-file (pathname->os-path f))
             (afm-scandir (afm-directory self))))))
  ;;
  (or (and (translation self)
	   (let ((try (table-lookup (translation self) font-name)))
	     (if (pair? try)
		 (get-afm-file self (car try))
		 (and try (check-afm-file self try)))))
      (check-afm-file self font-name)))

(define-method display-object ((self <afm-directory>) port)
  (format port "#[<afm-directory> ~a]" (afm-directory self)))

(define-method write-object ((self <afm-directory>) port)
  (format port "#[<afm-directory> ~a]" (afm-directory self)))
	  

(define *afm-path*
  (list (make <afm-directory>
	      afm-directory: (string->dir "~/lib/afm/adobe")
              translation: "Fontmap")
        (make <afm-directory>
	      afm-directory: (string->dir "~/lib/fonts")
              translation: "Fontmap")
	(make <afm-directory>
	      afm-directory: (string->dir "/usr/lib/ghostscript/fonts")
	      translation: "Fontmap")
	(make <afm-directory>
	      afm-directory: (string->dir "/usr/share/fonts/afms/adobe")
	      translation: "Fontmap")
	(make <afm-directory>
	      afm-directory: (string->dir "/usr/share/ghostscript/fonts")
	      translation: "Fontmap")
	(make <afm-directory>
	      afm-directory: (string->dir "/usr/lib/enscript"))))

(define (afm-scandir (dir <directory-name>))
  (let ((tbl (make-string-table)))
    (handler-case
     (for-each
      (lambda (ent)
        (if (and (> (string-length ent) 4)
                 (string-ci=? (substring ent (- (string-length ent) 4))
                              ".afm"))
            (let* ((f (append-path dir (string->file ent)))
                   (s (stat (pathname->os-path f))))
              (if (and s (stat-file? s))
                  (let ((info (peek-afm-header f)))
                    (if info
                        (table-insert! tbl (car info) (cdr info))))))))
      (scandir (pathname->os-path dir)))
     ((<condition>)))
    tbl))

(define (push-afm-directory (dir <string>) 
			    #optional (translation-file default: "font.map"))
  (let ((d (string->dir dir))
        (xlate translation-file))
    (if (and (string? xlate)
             (not (file-exists? (append-path d (string->file xlate)))))
        ;; scan the directory and build up an explicit map
        (set! xlate (afm-scandir d)))
    (set! *afm-path* (cons (make <afm-directory>
                                 afm-directory: d
                                 translation: xlate)
                           *afm-path*))))

(define (get-afm-from-path font-name)
  (let loop ((p *afm-path*))
    (if (null? p)
	#f
	(let ((f (get-afm-file (car p) font-name)))
	  (if f
	      (register-afm! font-name (load-afm (pathname->os-path f)))
	      (loop (cdr p)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;			 in-memory afm cache
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *afm-cache* (make-string-table))

(define (register-afm! font-name (afm <afm>))
  (table-insert! *afm-cache* font-name afm)
  afm)

(define (get-afm-from-table font-name)
  (table-lookup *afm-cache* font-name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;				 API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-afm font-name)
  (or (get-afm-from-table font-name)
      (get-afm-from-path font-name)
      (error "~a: no AFM loaded" font-name)))
