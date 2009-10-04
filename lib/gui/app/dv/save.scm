,(use rs.db.rstore)

;;; temporary ---------------------------------------------------------------

#|
(extend-named-pivot-index! '(module rs.util.quantity <derived-unit>))
(extend-named-pivot-index! '(module rs.util.quantity <base-unit>))

(define-module-extend rs.utixl.quantity ()
  (if-implements
   (available rs.db.rstore)
   (with-module rs.db.rstore
     (define (rp n v)
       (let ((k (register-pivot! `(module rs.util.quantity ,n) v)))
         (format #t "Registered pivot[~d] ~s\n" k n)))
     ;;
     (rp '<nquantity> <nquantity>)
     (rp '<base-unit> <base-unit>)
     (rp '<derived-unit> <derived-unit>))))
|#

;;; ------------------------------------------------------------------

,(use rs.db.lss)

;;;  create the repository for transient versions
;;;  of the given document

(define (get-sb-dir)
  (let loop ((dir "/tmp/.dv")
             (i 0))
    (let ((sb (stat dir)))
      (cond
       ((not sb)
        (with-module primops (os-mkdir dir))
        dir)
       ((stat-directory? sb)
        dir)
       ((< i 10)
        (loop (format #f "/tmp/.dv~a" i) (+ i 1)))
       (else
        (error "get-sb-dir: Could not find a temporary directory, even ~s"
               dir))))))

(define *docnum* 0)

(define (get-tmp-name)
  (set! *docnum* (+ *docnum* 1))
  (format #f "~a/dv_~d_~d" 
          (get-sb-dir) 
          (with-module unixm (getpid))
          *docnum*))

(define (open-document-lss new? #optional underlying-file)
  (let* ((t (get-tmp-name))
         (f0 (or underlying-file (string-append t ".v0")))
         (filev (vector f0
                        (string-append t ".v1")
                        (string-append t ".v2")))
         (l (if new?
                (begin
                  (dm "lss-create: ~s" filev)
                  (let ((tl (lss-create (vector f0))))
                    (lss-commit tl)
                    (lss-close tl))
                  (lss-open filev))
                (begin
                  (dm "lss-open: ~s" filev)
                  (lss-open filev)))))
    (lss-commit l)
    (dm "lss-attach-vol[1]: ~s" (vector-ref filev 1))
    (lss-attach-vol l 1 (vector-ref filev 1))
    (lss-commit l)
    (dm "lss-attach-vol[2]: ~s" (vector-ref filev 2))
    (lss-attach-vol l 2 (vector-ref filev 2))
    (dm "open-document-lss => ~s" l)
    l))

(define (init-unnamed-buffer (doc <open-document>))
  (let* ((lss (open-document-lss #t))
         (ps (open-pstore-on-lss lss)))
    (setup-indirects ps)
    (set-property! doc 'pstore ps)
    (set-property! doc 'lss lss)
    (lss-set-tip lss 1)
    (commit ps (underlying-object doc))
    (root-object ps)))

(define-interactive (save-as-file (doc <open-document>) (file <string>))
  (interactive (open-document) (minibuffer <string> "File: "))
  (if (stat file)
      (unlink file))
  (let* ((lss (open-document-lss #t file))
         (ps (open-pstore-on-lss (print lss))))
    (dm "save-as-file: setting up for save")
    (setup-indirects ps)
    (lss-set-tip lss 0)
    (dm "save-as-file: about to commit")
    (let ((x (commit ps (underlying-object doc))))
      (set-file-name! doc file)
      (dm "save-as-file: commit ~s to ~s" x file)
      (set-property! doc 'pstore ps)
      (set-property! doc 'lss lss)
      (mark-as-clean doc file)
      (lss-set-tip lss 1)
      (values))))

(define-interactive (load-file (file <string>))
  (interactive (minibuffer <string> "File: "))
  (load-file-by-extn file))

(define (load-file-by-extn file)
  (if (equal? (extension (string->file file)) "svg")
      (load-file-by-extn/svg file)
      (load-file-by-extn/lss file)))

(define (load-file-by-extn/svg file)
  (open-document (import-svg->document file)))

(define (load-file-by-extn/lss file)
  (let* ((lss (open-document-lss #f file))
         (ps (open-pstore-on-lss lss)))
    (setup-indirects ps)
    (let ((od (open-document (root-object ps) file)))
      (set-file-name! od file)
      (set-property! od 'pstore ps)
      (set-property! od 'lss lss)
      (lss-set-tip lss 1)
      od)))

(global-set-key '(#\C-x #\C-f) load-file)

(define-interactive (save-file (doc <open-document>))
  (interactive (open-document))
  (if (get-property doc 'pstore #f)
      (let* ((x (compact-store (get-property doc 'pstore)
                               (get-property doc 'lss)))
             (str (format #f "Saved: ~s" x)))
        (dm "save-as-file: commit ~s to ~s" x (file-name doc))
        (for-each (lambda ((v <open-view>))
                    (set-status-line! v str))
                  (open-views doc))
	(mark-as-clean doc)
	(set-dirty?! doc #f))
      (call-interactive save-as-file)))

(global-set-key '(#\C-x #\C-s) save-file)

;;;

(define (setup-indirects ps)
  ; geometry
  (register-indirect-page ps 100 *geometry-classes*)
  ; application
  (register-indirect-page ps 101 (vector
				  <document>
				  <root-group>
				  <user-group>
				  <page>
				  <view>
				  (with-module corelib <mp-rational>)
				  (with-module corelib <mp-data>)
				  (with-module corelib <bignum>)
				  ))
  ; modules
  ;
  ; ...this should be improved to support dynamic modules
  ; and forward evolution.  Doing so would probably involve
  ; some tricks about storing some metadata in the store,
  ; and making sure we can read the metadata without needing
  ; any module objects...
  (register-indirect-page ps 102 (vector <box-graphic>
					 ;
					 <line-graphic>
					 ;
					 <text-graphic>
					 <text-run>
					 <text-font>
                                         ;
                                         <path-graphic>
                                         <subpath>
                                         <path-point>
                                         ;
                                         <script-graphic>
                                         <script-shader>))
  (register-indirect-page ps 103 (vector
                                  <style-set>
                                  ;;
                                  <stroke-style>
                                  <fill-style>
                                  <color-style>
                                  <character-style>
                                  <font-style>
                                  <paragraph-style>
                                  <page-style>))
  )
                                  

;;;
;;;  Compact the persistent store to force all the data into
;;;  the underlying file
;;;

(define (compact-store pstore lss)
  (let ((a (lss-record-query lss '(1 . 0) '(1 . #x1FFFFFFF)))
        (b (lss-record-query lss '(2 . 0) '(2 . #x1FFFFFFF)))
        (t (lss-get-tip lss)))
    (dm "moving LSS tip from vol[~d] to vol[0]" t)
    (lss-set-tip lss 0)
    (vector-for-each (lambda (r) 
                       (dm "moving record ~x from vol[1]" r)
                       (lss-move-record lss 0 r))
                     a)
    (vector-for-each (lambda (r) 
                       (dm "moving record ~x from vol[2]" r)
                       (lss-move-record lss 0 r)) 
                     b)
    ;;
    (let ((k (commit pstore)))
      (dm "committed => ~s, resetting tip to [~d]" k t)
      ;;
      (lss-set-tip lss t)
      k)))
