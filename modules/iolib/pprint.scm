#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/iolib/pprint.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.5
 | File mod date:    2003-03-03 01:16:33
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  iolib
 |
 `------------------------------------------------------------------------|#

; PRETTY PRINTER FOR DATA STRUCTURES

(define (list-max lis)
   (let ((max 0))
      (for-each (lambda (x)
                   (if (> x max)
                       (set! max x)))
                lis)
      max))

(define (list-sum x)
   (if (null? x)
       0
       (+ (car x) (list-sum (cdr x)))))

(define (display-n-spaces n)
   (cond ((> n 0)
          (display " ")
          (display-n-spaces (- n 1)))))

(define (non-list-print-length>= item)
   (cond ((number? item)
          (+ 1 (string-length (number->string item))))
         ((symbol? item)
          (+ 1 (string-length (symbol->string item))))
         ((string? item)
          (+ 3 (string-length item)))
         ((boolean? item)
          3)
         ((null? item)
          3)
         ((char? item)
          7)
         (#t
          20)))

; PRINT-LENGTH>= tries to compute an overestimate of the size of a
; printed expression as a sequence of characters.  The level argument
; says how far to go down nested lists.  If a list is nested more
; deeply than that, it gives up and returns a large number, to signify
; that you shouldn't try to print this expression on one line.
; That seems like a reasonable heuristic---even if a deeply nested
; expression could fit on one line, it's probably a good idea to consider
; it "big" and break it over several lines for clarity.

(define (print-length>= item level)
   (cond
    ((pair? item)
     (if (> level 0)
         (let loop ((total 1)
                    (lst item))
           (if (pair? lst)
               (loop (+ total (print-length>= (car lst) (- level 1))) 
                     (cdr lst))
               (if (null? lst)
                   (+ total 1)
                   (+ total 4 (print-length>= lst (- level 1))))))
         1000))
    (#t
     (non-list-print-length>= item))))

; PP-AUX does the real work for pretty-printing.  You tell it what to
; print, how far it should be indented, where the cursor is on the
; current line, and how many columns it can use (the text width).

(define (pp-aux x indent curr-pos max-col)

   ; if the cursor isn't where we need to start, fill with blanks.
   (cond ((> indent curr-pos)
          (display-n-spaces (- indent curr-pos))
          (set! curr-pos (- indent curr-pos))))

   (cond ((pair? x)
          (let ((flat-pl (print-length>= x 4)))
             (cond ((> max-col                    ; if it'll fit in the rest
                       (+ (+ indent flat-pl) 2))  ; of the current line
                    (display "(")                
                    (write (car x))
                    (for-each (lambda (x)
                                 (display " ")
                                 (write x))
                              (cdr x))
                    (display ")"))
                   (#t                            ; else break up and indent

                    ; print left paren & advance to next position
                    (display "(")
                    (set! curr-pos (+ curr-pos 1))

                    ; recurse to print first item on same line
                    (pp-aux (car x) (+ indent 1) curr-pos max-col)

                    ; and loop to print rest on succeeding lines
                    (for-each (lambda (elem)
                                 (newline)
                                 (pp-aux elem (+ indent 1) 0 max-col))
                              (cdr x))
                    (display ")") ))))  ; add closing paraen to last line
         (#t
           (write x))))                        
                  
(define (pp x . args)
  (let ((max-columns (if (null? args) 75 (car args))))
    (pp-aux x 0 0 max-columns)
    (newline)))
