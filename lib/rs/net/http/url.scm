(define url-pattern (reg-expr->proc
                     '(entire
                       (seq
                        (? (seq
                            (save (+ alpha)) 
                            #\:
                            "//"
                            ;; what are valid domain name characters?
                            (save (+ (or alpha digit #\- #\.)))
                            (? (seq #\: (save (+ digit))))))
                        (save (* any))))))

(define path-pattern (reg-expr->proc
                      '(entire
                        (seq
                         (save (* (not (or #\# #\?))))
                         (? (seq #\# (save (+ (not (or #\# #\?))))))
                         (? (seq #\? (save (* any))))))))


(define (parse-path str)
  (bind ((s e main tag query (path-pattern str)))
    (append
     (cond
      ((string=? main "")
       '(()))
      ((string=? main "/")
       '(("")))
      (else
       (list (map http-url-decode (string-split main #\/)))))
     (if tag
         (list 'tag: tag)
         '())
     (if query
         (list 'query: query)
         '()))))

;;;
;;;  NOTE:  This procedure also decodes url encoding
;;;         (i.e., processing '+' and '%XX')
;;;
;;;  1. Relative
;;;
;;;     "foo"               -> (() ("foo"))
;;;     "foo/bar"           -> (() ("foo" "bar"))
;;;     "a/b+x%2f/c"        -> (() ("a" "b x/" "c"))
;;;
;;;  2. Absolute, perhaps w/ Extras
;;;
;;;     "/a/b?x=1&y=2"      -> (() ("" "a" "b") query: "x=1&y=2")
;;;     "/foo#bob"          -> (() ("" "foo") tag: "bob")
;;;     "/a#x?y=1"          -> (() ("" "a") tag: "x" query: "y=1")
;;;
;;;  3. Absolute w/ Server
;;;
;;;     "http://www.rscheme.org/" -> (("http" "www.rscheme.org") (""))
;;;     "http://www.abc.com/foo"  -> (("http" "www.abc.com") ("" "foo"))
;;;
;;;     "https://www.rscheme.org:4343/foo")
;;;                              -> (("https" "www.rscheme.org" "4343")
;;;                                  ("" "foo"))

(define (string->url (str <string>))
  (bind ((s e method domain port path (url-pattern str)))
    (if s
        (if method
            `((,method ,domain ,@(if port (list port) '()))
              ,@(parse-path path))
            `(() ,@(parse-path path)))
        #f)))

;;;
;;;  Compute a combined URL, interpreting "new"
;;;  relative to the document identified by "base"
;;;
;;;   Suppose base is (parse-url "http://www.rscheme.org:123/foo/bar#hello")
;;;
;;;   Then if new is parse-url of:    then the result represents:
;;;
;;;     #bax                            http://www.rscheme.org:123/foo/bar#bax
;;;     ?x=1                            http://www.rscheme.org:123/foo/bar?x=1
;;;     rats                            http://www.rscheme.org:123/foo/rats
;;;     ../can                          http://www.rscheme.org:123/can
;;;     ../../can                       http://www.rscheme.org:123/../can
;;;     /hat                            http://www.rscheme.org:123/hat
;;;     http://www.rscheme.org/a        http://www.rscheme.org/a

(define (expand-relative-url base new)
  (cond
   ;;
   ((pair? (car new))
    ;; the new url has specified a method part and everything!
    new)
   ;;
   ((null? (cadr new))
    ;; no path was specified; apply the modifiers relative to the same resource
    (cons* (car base)
           (cadr base)
           (cddr new)))
   ;;
   ((equal? (caadr new) "")
    ;; keep the host part, but the path part (and modifiers) are absolute
    (cons (car base) (cdr new)))
   ;;
   (else
    ;; strip off the base modifiers, but the path part is relative
    (cons* (car base)
           (cons ""
                 (follow-ups
                  (cdr (reverse (cdadr base)))
                  (cadr new)))
           (cddr new)))))

(define (follow-ups base new)
  (if (and (pair? new)
           (string=? (car new) "..")
           (pair? base))
      (follow-ups (cdr base) (cdr new))
      (append (reverse base) new)))

      
      

;;;

(define (local-part->string local-path extras)
  ;;
  (define (prpath port)
    (if (null? (cdr local-path))
        (write-string port "/")
        (let loop ((l (cdr local-path)))
          (if (pair? l)
              (begin
                (write-string port "/")
                (write-string port (url-encode-step (car l)))
                (loop (cdr l)))))))
  ;;
  (define (prextras port)
    (let loop ((x extras))
      (if (pair? x)
          (case (car x)
            ((tag:) 
             (write-char #\# port)
             (write-string port (cadr x))
             (loop (cddr x)))
            ((query:)
             (write-char #\? port)
             (write-string port (cadr x))
             (loop (cddr x)))
            (else
             (error "bad extra: ~s" x))))))
  ;;
  (call-with-output-string
   (lambda (port)
     (prpath port)
     (prextras port))))

(define (url->string url)
  (~ "~a://~a~a~a" 
     (caar url)
     (cadar url)
     (if (pair? (cddar url))
         (~ ":~a" (caddar url))
         "")
     (local-part->string (cadr url) (cddr url))))

;;;

(define (url-local->string url)
  (local-part->string (cadr url) (cddr url)))
