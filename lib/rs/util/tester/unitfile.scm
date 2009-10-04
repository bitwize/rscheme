#|
  Here is the formal structure of a unit test file:

  Notes
  =====

  1. If an input line destined for the REPL ends in a comment,
     then you will get a secondary prompt instead of a primary
     prompt.  e.g., given:

     +---------------------------------------+
      (define (foo x)
        (list x))  ; here is a definition
      (define (bar) 3)
     +---------------------------------------+

     the system will emit a primary prompt ("test[]=>") for the
     foo definition, but only a secondary prompt for the bar definition.
     This can be confusing because it will look like the system isn't
     prompting...

  2. Use @reprompt at the end of a note section to reissue the prompt,
     otherwise the next REPL input line will appear to be promptless


  3. If there are breaks (notes) that cross markup contours, then 
     treeify-markup may fail.  For example, if there is an i/o block
     that creates a break loop, then a note, and then an i/o block
     that unwinds past it (as by ",top"), then treeify markup will
     see a (markup end) indicator that is unmatched.

  Unit Test File Grammar
  ======================

  file ::=  item+

  item ::=  escape-line NL
        |   note-block
        |   stdin-line NL

  escape-line ::= ";;;" (title-line
                         | tag-line
                         | reprompt-line
                         | import-line
                         | note-line)

  note-line ::= "@note" LWS+ CHAR+
  tag-line ::= "@tag" LWS+ CHAR+
  reprompt-line ::= "@reprompt" LWS*
  title-line ::= "@title" LWS+ CHAR+
  import-line ::= "@import" LWS+ CHAR+

  note-block ::= "#|@note[[" (CHAR | NL)* "]]" LWS* "|#" LWS* NL

  stdlin-line ::= CHAR* - (";;;@" CHAR*)


  CHAR ::= " " | ... | "~"

  NL := "\n"

  LWS ::= " " | "\t"

|#

(define (load-unit-test src)
  (call-with-input-file
      src
    (lambda (port)
      (let loop ((r '()))
        (let ((l (read-line port)))
          (if (eof-object? l)
              (reverse! r)
              (bind ((s e cmd rest (esc-line (trim-whitespace l))))
                (if s
                    (if (string=? cmd "note[[")
                        (loop (cons (list 'esc 'note
                                          (eat-until-end-of-note port rest))
                                    r))
                        (loop (cons (list 'esc (string->symbol cmd) rest) r)))
                    (loop (cons l r))))))))))

(define (eat-until-end-of-note port first)
  (let ((save (open-output-string)))
    (let loop ((l first))
        (if (eof-object? l)
            (error "End of input during extended `@note[[' sequence")
            (bind ((s e (endofxnote l)))
              (if s
                  (begin
                    (write-string save (substring l 0 s))
                    (get-output-string save))
                  (begin
                    (write-string save l)
                    (newline save)
                    (loop (read-line port)))))))))
                    

(define endofxnote (reg-expr->proc '(suffix (seq "]]" 
                                                 (* space)
                                                 (? "|#")
                                                 (* space)))))

(define esc-line (reg-expr->proc '(entire
                                   (seq (or ";;;" "#|") 
                                        #\@
                                        (save (seq (+ alpha) (? "[[")))
                                        (* space)
                                        (save (* any))))))
