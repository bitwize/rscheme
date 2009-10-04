(define-module rs.util.charset (&module)
 (&module
  (import usual-inlines tables sort)
  (load "charsets.scm")
  (load "commonset.scm")
  (export
    *digit*
    *hex-digit*
    ;*id-continued*
    ;*id-initial*
    *letter*
    ;*num-continued*
    ;*num-initial*
    *octal-digit*
    ;*special-continued*
    ;*special-initial*
    *whitespace*
    <char-set>
    <char-table>
    char-set-union
    make-char-set
    make-char-table
    ranges->char-set
    members->char-set
    members->char-table
    ;;
    *xml-combining-char*
    *xml-digit-char*
    *xml-extender-char*
    *xml-base-char*
    *xml-letter-char*
    *xml-ideographic*)))
