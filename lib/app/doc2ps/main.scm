#|
(define-module-extend graphics.afm ()

(define-method xshow-x-list ((afm <afm>) (font-size <real>) (str <string>))
  (let ((fscale (/ font-size 1000)))
    (map (lambda (cm dx)
           (+ (* (x-width cm) fscale)
              (* dx font-size)))
         (get-char-metrics afm str)
         (append (string-x-deltas afm str) '(0)))))

)
|#

,(use regex)
(define inpat (reg-expr->proc '(entire
                                (seq
                                 (save (seq
                                        (? #\-) 
                                        (+ (or digit #\.))))
                                 (save (or "in" "cm" "mm" "pt" "pi"))))))

(define (parse-inches str)
  (bind ((s e num suffix (inpat str)))
    (if s
        (* (cond
            ((string=? suffix "in") 72)
            ((string=? suffix "cm") (/ 7200 254))
            ((string=? suffix "mm") (/ 720 254))
            ((string=? suffix "pi") 12)                 ; pica
            ((string=? suffix "pt") 1)
            (else
             (error "Unknown unit suffix: ~s" str)))
           (string->number num))
        #f)))

(add-alternate-number-parser! parse-inches)


,(use graphics.device)
,(use graphics.fontmgr)
,(use graphics.styles)
,(use graphics.afm)
(push-afm-directory "~/lib/fonts" "Fontmap")

,(use graphics.geometry)
,(use graphics.color)

,(use rs.util.properties)
,(use tables)
,(use rs.util.msgs)
;,(use rs.util.static-call-graph)

;;; BASELINE STUFF

;(load "../../gui/app/dv/export.scm")

;(snapshot-current-environment)

;;; SGML STUFF

(load "read.scm")
(load "docbook.scm")
(load "rschemebook.scm")

;;; LAYOUT STUFF

;;;

;(define *dev* (open-gs-device))

(load "iterator.scm")
(load "reqstream.scm")

(load "contentmodel.scm")
(load "changebar.scm")

(load "paradev.scm")
(load "placement.scm")
(load "textframe.scm")
(load "hlist.scm")
(load "para2.scm")
(load "para.scm")

(load "pages2.scm")
(load "fakepages.scm")

(load "flowlayout.scm")
(load "paralayout.scm")
(load "linewrap.scm")
(load "hyphenate.scm")
(load "table.scm")
(load "graphic.scm")

;(load "bookstyles.scm")
(load "refentry.scm")
(load "example.scm")
(load "partpage.scm")


;;;  bridging between SGML and LAYOUT worlds

(load "bridge.scm")
(load "title.scm")
(load "toc.scm")
(load "index.scm")

;;;

;(load "test2.scm")
;(load "test3.scm")

(load "style-support.scm")

;(define $dv-version "0.999")

#|
(define (go)
  (tbridge2)
  (tprp)
  (process-exit 0))

(define (tdoc)
  (book->ps "/u/donovan/rscheme/f/0.7/3.3/doc/decl.sgml"
            "/u/donovan/rscheme/f/0.7/3.3/doc/rscheme.sgml"))
|#
