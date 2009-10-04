;;

(define-class <person> (<object>)
  full-name
  first-name
  user-id
  gender
  home-page-proc)

(define (submit-button name label)
  (let ((d (underlying-output-port (current-output-port))))
    (format d "<INPUT type=\"submit\" name=\"~a\" value=~s>\n" name label)))

(define (big-text-field name rows cols default)
  (let ((d (underlying-output-port (current-output-port))))
    (format d "<TEXTAREA name=\"~a\" ROWS=~d COLS=~d>~a</TEXTAREA>\n" 
	    name rows cols
	    default)))

(define *query* #f)

(define (root-handler path query data)
  (console "path: ~s\n" path)
  (cond
   ((and (pair? path)
	 (> (string-length (car path)) 0)
	 (eq? (string-ref (car path) 0) #\~))
    (home-page-dispatch (substring (car path) 1)
			(cdr path)
			query
			data))
   ((and (pair? path)
	 (string=? (car path) "echo"))
    (echo-responder path query data))
   ;;
   ((and (pair? path)
	 (string=? (car path) "cmvc"))
    (cmvc-responder (cdr path) query data))
   ;;
   ((and (pair? path)
	 (string=? (car path) "rnote"))
    (rnote-responder (cdr path) query data))
   ;;
   (else
    (fluid-let ((*query* query))
      (virtual-html-page-dispatch path)))))

;;
(define (virtual-html-page-dispatch path)
  (if (null? path)
      (virtual-html-page-dispatch '(""))
      (if (string=? (last path) "")
	  (virtual-html-page-dispatch 
	   (reverse (cons "Welcome.html" (cdr (reverse path)))))
	  ;;
	  ;; at last, in canonical form
	  ;;
	  (real-html-page-dispatch path))))

(define *root-path* (string->dir "/u/donovan/net/www_clone"))

(define (real-html-page-dispatch path)
  (console "real-page: ~s\n" path)
  (let* ((pathn (string->file (string-join #\/ path)))
	 (type (assoc (extension pathn)
		     '(("gif" image/gif)
		       ("jpg" image/jpeg)
		       ("html" text/html)))))
    (if type
	(let ((sub (append-path *root-path* pathn))
	      (t (cadr type)))
	  (console "Checking subfile of [root]: ~a\nfull path: ~a\n"
		   (string-join "/" path) 
		   sub)
	  (let ((s (stat (pathname->os-path sub))))
	    (if s
		(begin
		  (console " File exists (~d bytes), will return type ~s\n"
			   (stat-size s)
			   t)
		  (case t
		    ((text/html)
		     (console " ** will run through pagefilter **\n")
		     (filtered-html-page path pathn))
		    ((image/gif image/jpeg)
		     (display (file->string (pathname->os-path sub)))
		     t)))
		(error/url-not-found *query*))))
	(error/url-not-found *query*))))


;; BUGS:  A '?' in a pathname causes Netscape (at least) to stop
;; escaping the pathname after that point.  
;; Also, %'s in pathnames do not get escaped even before that
;;
;; NOTE: That's not a bug.  I think it's consistent with the
;; syntax for URI's
;;   see http://www.w3.org/hypertext/WWW/Addressing/URL/uri-spec.html
;;
(define (print-result query data)
  (let ((path (cdr (assq 'request-path query))))
    ;; choke on relative pathnames
    (root-handler (cdr (map (lambda (s) 
			      (de-escape-str s 0))
			    path))
		  query
		  data)))
#|(select (lambda (i) 
(not (eq? (string-length i) 0)))
(map (lambda (s) (de-escape-str s 0)) path))
|#

(define (full-dispatch path query data)
  (table-insert! *proc-cache*
		 path
		 echo-responder)
  (echo-responder query data))

(define (home-page-dispatch who path query data)
  (let ((a (assoc who (map (lambda (p) (cons (user-id p) p)) *people*))))
    (if a
	((home-page-proc (cdr a)) (cdr a) path query data)
	(directory-assistance-handler who path query data))))

(define (mail-to name addr)
   (output-port-control (current-output-port)
			'ref
			(string-append "mailto:" addr))
   (display name)
   (output-port-control (current-output-port)
			'ref
			#f))

(define *enable-console* #f)

(define (console fmt . args)
  (if *enable-console*
      (apply format 
	     (fluid-ref *console-output-port*)
	     fmt
	     args)))

(define (home-page-handler (who <person>) path query data base-path home-page)
  (if (or (null? path)
	  (equal? path '("")))
      (begin
	(console "Returning home page for: ~a\n" (full-name who))
	(display (file->string (string-append base-path "/" home-page)))
	'text/html)
      (let ((type (assoc (extension (string->file (last path)))
			 '(("gif" image/gif)
			   ("html" text/html)))))
	(if type
	    (let ((sub (string-append base-path "/" (string-join "/" path))))
	      (console "Checking subfile of ~a: ~a\n"
		       (full-name who)
		       (string-join "/" path))
	      (if (stat sub)
		  (begin
		    (console " File exists (~d bytes), will return type ~s\n"
			     (stat-size (stat sub))
			     (cadr type))
		    (display (file->string sub))
		    (cadr type))
		  (error/url-not-found query)))))))

(define *visits* (make-object-table))

(define (dynamic-page-handler (who <person>) path query data)
  (html
   (html-header 
    (title (format #t "~a's home page" (full-name who))))
   (html-body
    (let ((peer (cdr (assq '*client-machine* query))))
      (header-1 (format #t "~a's home page" (full-name who)))
      (bold (format #t "Automatically generated for ~a."
		    (cdr (assq '*client-machine* query))))
      (par)
      (let ((t (table-lookup *visits* who)))
	;;
	(if (not t)
	    (begin
	      (set! t (make-table string=? string->hash))
	      (table-insert! *visits* who t)))
	;;
	(table-insert! t peer (+ 1 (or (table-lookup t peer) 0)))
	(format #t "This page has been visited ~d times by:\n"
		(apply + (value-sequence t)))
	(par)
	(preformatted
	 (for-each (lambda (w)
		     (format #t "  ~a (~d time~p)\n" 
			     w 
			     (table-lookup t peer)
			     (table-lookup t peer)))
		   (sort (key-sequence t) string<?)))
	(par)
	(horz-rule)
	(format (underlying-output-port (current-output-port))
		"<b><font size=-1>Generated on <i>~a</i></font></b>\n"
		(time->string (time) "%A, %B %d, %Y, %H:%S %Z")))))))

