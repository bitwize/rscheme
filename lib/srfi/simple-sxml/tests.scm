
#|
In RScheme:

  To test the reference implementation:

     rsf -q +srfi.64 +srfi.66 tests.scm -exit

  To test the native implementation:

     rsf -q +srfi.64 +util.xml tests.scm -exit

|#


;;;  NOTE 1.
;;;
;;;  The implementation is free to crack a text into separate strings
;;;  for performance or other reasons (e.g., the reference implementation
;;;  separates strings at entity boundaries, so that "foo&lt;bar" is
;;;  returned as ... "foo" "<" "bar"
;;;
;;;  Thus, we'll define a canonicalization procedure `flatten-text' that
;;;  unifies all string sequences
;;;

(define (flat sxml)
  (if (pair? sxml)
      (if (and (pair? (cdr sxml))
               (string? (car sxml))
               (string? (cadr sxml)))
          ;; This isn't terribly efficient...  You wouldn't want
          ;; to do this in production
          (flat (cons (string-append (car sxml) (cadr sxml))
                      (cddr sxml)))
          (cons (flat (car sxml)) (flat (cdr sxml))))
      sxml))

(test-begin "SRFI XX - Simple XML Input/Output")

(test-begin "1. Writing SXML")

(test-group
 "1.1. Trivial Examples"
 ;;
 (test-equal (sxml->string '(a "foo")) "<a>foo</a>")
 (test-equal (sxml->string '(*TOP* (a "foo"))) "<a>foo</a>"))

(test-group
 "1.2. Attributes"
 ;;
 ;;  Attributes are quoted, but with either #\' or #\"
 ;;
 (test-assert (member (sxml->string '(i (@ (x "1")) "foo"))
                      '("<i x=\"1\">foo</i>"
                        "<i x='1'>foo</i>")))
 ;;
 ;;  Attributes should be output in order
 ;;
 (test-assert (member (sxml->string '(i (@ (x "1") (y "2")) "foo"))
                      ;; theoretically, x and y could use different 
                      ;; string delimiters, but that'd be strange
                      '("<i x=\"1\" y=\"2\">foo</i>"
                        "<i x='1' y='2'>foo</i>"))))
 
(test-group
 "1.3. Standard XML Entities"
 ;;
 (test-equal (sxml->string '(i "x<y")) "<i>x&lt;y</i>"))

(test-group
 "1.9. Errors are signalled"
 (test-error (sxml->string '(a 1)))
 (test-error (sxml->string '(a #t)))
 (test-error (sxml->string '(a #f)))
 (test-error (sxml->string '(a #(hello))))
 (test-error (sxml->string '(a ())))
 (test-error (sxml->string '(a foo)))
 (test-error (sxml->string '(a ("foo"))))
 (test-error (sxml->string '(a . "foo")))
 ;; RScheme's native implementation actually allows this...
 (cond-expand
  (rscheme-native-xml (test-skip (test-match-nth 1)))
  (else #t))
 ;;
 (test-error (sxml->string '(a (@ (foo)) "foo")))
 (test-error (sxml->string '(a (@ (foo . "1")) "foo")))
 (test-error (sxml->string '(a (@ (foo "1") . bar) "foo")))
 (test-error (let ((circ (list "1")))
               (set-cdr! circ circ)
               (sxml->string (cons 'a circ)))))




(test-end "1. Writing SXML")

(test-begin "2. Reading SXML")

(test-group
 "2.1. Trivial Examples"
 ;;
 (test-equal (flat (string->sxml "<a>foo</a>")) '(*TOP* (a "foo")))
 (test-equal (flat (string->sxml "<a>foo<b/>bar</a>")) '(*TOP* (a "foo" (b) "bar"))))

(test-group
 "2.2. Standard XML Entities"
 ;;
 (test-equal (flat (string->sxml "<a>foo&lt;bar&gt;</a>")) '(*TOP* (a "foo<bar>")))
 (test-equal (flat (string->sxml "<a>foo&amp;lt;</a>")) '(*TOP* (a "foo&lt;"))))

(test-end "2. Reading SXML")

(test-end "SRFI XX - Simple XML Input/Output")
