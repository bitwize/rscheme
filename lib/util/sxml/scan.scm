;;;
;;;   An XML scanner
;;;

(define-class <xml-token> (<object>) :abstract
  (location init-value: #f)
  (properties type: <vector> init-value: '#()))

(define-class <xml-pi> (<xml-token>)
  target
  pi)

(define-class <xml-doctype-decl> (<xml-token>)
  name
  sysid
  (pubid init-value: #f)        ; #f if it's a SYSTEM doctype decl 
  (subtype init-value: #f))

(define-class <xml-text> (<xml-token>)
  items)

(define-class <xml-element> (<xml-token>)
  gi
  attributes
  namespace)

(define-class <xml-start-element> (<xml-element>))

(define-class <xml-end-element> (<xml-token>)
  gi
  namespace)

(define-class <xml-comment> (<xml-token>)
  item)

(define-class <xml-scanner-state> (<object>)
  ;;
  (namespaces type: <list> init-value: '()))

(define-class <xml-attr> (<xml-token>)
  name
  namespace
  items)

(define *xml-token-classes*
  (vector '#()
          <xml-element>
          <xml-start-element>
          <xml-end-element>
          <xml-text>
          <xml-pi>
          <xml-doctype-decl>
          <xml-comment>
          <xml-attr>))


(define-safe-glue (xml-scan (s <xml-scanner-state>)
                            (buffer <string>)
                            (offset <raw-int>)
                            (len <raw-int>)
                            (lastq <boolean>))
  properties: ((other-h-files "scanner.h")
               (other-c-files "scanner.c"))
  literals: ((& *xml-token-classes*))
{
  obj r;
  unsigned i = offset;
  int tc;
  
  tc = rscheme_xml_scanner( s, 
                            buffer,
                            &i,
                            len,
                            TLREF(0),
                            truish(lastq),
                            &r,
                            0 );
  if (tc == TC_MORE) {
    REG0 = ZERO;
    RETURN1();
  } else {
    REG0 = int2fx( tc );
    REG1 = r;
    REG2 = int2fx( i );
    RETURN(3);
  }
})

(define-safe-glue (xml-scan+ (s <xml-scanner-state>)
                             (buffer <string>)
                             (offset <raw-int>)
                             (len <raw-int>)
                             (lastq <boolean>)
                             (goal <raw-int>)) 
  properties: ((other-h-files "scanner.h")
               (other-c-files "scanner.c"))
  literals: ((& *xml-token-classes*))
{
  obj r;
  unsigned i = offset;
  int tc;
  
  tc = rscheme_xml_scanner( s, 
                            buffer,
                            &i,
                            len,
                            TLREF(0),
                            truish(lastq),
                            &r,
                            goal );
  if (tc == TC_MORE) {
    REG0 = ZERO;
    RETURN1();
  } else if (i > offset) {
    REG0 = int2fx( tc );
    REG1 = r;
    REG2 = int2fx( i );
    RETURN(3);
  } else {
    RETURN0();
  }
})

;;;

(define (make-char-class-table)
  (with-module rs.util.charset
    (with-module tables
      (string-join
       "," 
       (map (lambda (i)
              (let ((ch (integer->char i)))
                (to-string
                 (+ (if (table-lookup *xml-letter-char* ch) #b1 0)
                    (if (or (table-lookup *xml-letter-char* ch)
                            (table-lookup *xml-digit-char* ch)
                            (memq ch '(#\. #\- #\_ #\:))
                            (table-lookup *xml-combining-char* ch)
                            (table-lookup *xml-extender-char* ch))
                        #x02    ; CHAR_IS_NAMECHAR
                        0)
                    (if (memq ch '(#\x20 #\x0D #\x0A #\x09) #x04 0)
                        #x04    ; CHAR_IS_SPACE
                        0)
                    (if (or (memq ch '(#\x20 #\x0D #\x0A))
                            (string-search "-'()+,./:=?;!*#@$_%" ch )
                            (char-alphabetic? ch)
                            (char-numeric? ch))
                        #x08    ; CHAR_IS_PUBIDCHAR
                        0)
                    ))))
            (range 256))))))
