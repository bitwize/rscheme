(define (load* f)
  (format #t "*** Loading: ~a\n" f)
  (load f))

(load* "rs/sys/config/module.scm")

(with-module rs.sys.config
  (for-each 
   (lambda (item)
     (if (> (string-length item) 0)
         (let ((eq (string-search item #\=)))
           (if eq
               (let ((k (string->symbol (substring item 0 eq)))
                     (v (substring item (+ eq 1))))
                 ;;
                 (format #t "*** Configuring ~a ::= ~s\n" k v)
                 (config-set! k v))
               (let ((k (string->symbol item)))
                 (format #t "*** Configuring ~s\n" k)
                 (config-enable! k))))))
   (string-split (or (getenv "RSLIB_WITH") "") #\space)))


(load* "rational.scm")
(load* "rs/backend/c/pragma_sealed.scm")
(load* "rs/sys/undefine/module.scm")
(load* "rs/sys/multimethod/module.scm")
(load* "rs/sys/generic-math/module.scm")
(load* "rs/sys/numeric/module.scm")

(load* "rs/util/stdopt/module.scm")
(load* "rs/util/subprocess/module.scm")
(load* "rs/util/charset/module.scm")
(load* "rs/util/collection/module.scm")
(load* "rs/util/msgs/module.scm")
(load* "rs/util/properties/module.scm")
(load* "rs/util/unicode/module.scm")
(load* "rs/util/text/module.scm")
(load* "rs/util/iterate/module.scm")
(load* "rs/util/quantity/module.scm")

(load* "rs/io/conditional-read/module.scm")
(load* "rs/io/here-strings/module.scm")
(load* "rs/io/textport/module.scm")
(load* "rs/io/scanner/module.scm")
(load* "rs/io/parser/module.scm")
(load* "rs/io/pushback/module.scm")

(load* "rs/backend/c/module.scm")
(load* "rs/sys/compression/module.scm")
(load* "rs/util/pack/module.scm")

(load* "rs/net/html/module.scm")
(load* "rs/net/html/parse/module.scm")
(load* "rs/net/nvt/module.scm")
(load* "rs/net/md5/module.scm")
(load* "rs/net/sha1/module.scm")
(load* "rs/net/pem/module.scm")
(load* "rs/net/ssl/module.scm")

(load* "rs/util/bits/module.scm")
(load* "util/readline/module.scm")
(load* "util/paste/module.scm")
(load* "util/pretty-print/module.scm")

(load* "util/patterns/module.scm")

(load* "util/sxml/module.scm")
(load* "util/xml/module.scm")
(load* "util/xpath/module.scm")

(load* "graphics/geometry/module.scm")
(load* "graphics/afm/module.scm")

;
(load* "graphics/color/module.scm")
(load* "graphics/color/dither/module.scm")
(load* "graphics/image/module.scm")

#|
(cond-expand
 (os.aix
  (values))
 (else
  (load* "graphics/gd/module.scm")
  (load* "graphics/png/module.scm")))
|#

;(cond-expand (os.unix (load* "rs/unix/security/module.scm")))

;
(load* "gui/x/module.scm")
(load* "graphics/tiff/module.scm")
(load* "gui/util/x/module.scm")
(load* "rs/sys/errno/module.scm")

(load* "rs/util/types/module.scm")
(load* "rs/util/logfile/module.scm")
(load* "rs/util/realm/module.scm")
(load* "rs/net/httpd/module.scm")
(load* "rs/net/http/module.scm")


(if-implements config.sqlite3 (load* "rs/db/sqlite3/module.scm"))

(load* "rs/backend/c/pragma_not_sealed.scm")

(with-module rs.backend.c (flush-all-code))
(with-module rs.sys.errno (init-errnos))
