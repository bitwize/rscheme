(define-module rs.lang ()
  (&module
   (import primops *scheme* iolib low-scheme objsys mathlib tables high-scheme)
   (import compiler codegen editinp paths mlink start sort)
   (import corelib)
   ;;
   ;;  Section 1. R4RS
   ;;  ===============
   ;;  (Note: Anything missing from here is missing from RScheme's
   ;;   R4RS compliance and/or `r4rs' module)
   ;;
   ;;  -- block structure --
   (export define 
	   begin)
   ;;
   ;;  -- bindings --
   (export let
	   let*
	   letrec
	   bind
	   lambda
	   set!)
   ;;  -- conditionals ---
   (export if
	   case
	   cond
	   and
	   or
	   do
	   not)
   ;;  -- quoting --
   (export quote
	   quasiquote)
   ;;  -- evaluation ---
   (export delay force)
   ;;  -- collections --
   (export <collection>
	   initial-state
	   next-state
	   size
	   element
	   set-element!)
   ;;  -- lists --
   (export append
	   reverse  
	   apply
	   map for-each
	   assoc assq assv
	   member memq memv
	   cons
	   set-car!
	   set-cdr! 
	   car cdr
	   caar cadr cddr cdar caaar caadr caddr 
	   cadar cdaar cdadr cdddr cddar caaaar caaadr 
	   caaddr caadar cadaar cadadr cadddr caddar
	   cdaaar cdaadr cdaddr cdadar cddaar cddadr 
	   cddddr cdddar
	   list? length list list->string list->vector list-ref list-set! 
           list-tail)
   ;; -- object system --
   (export eq?
	   char? quantity? number? integer? complex? pair?
           boolean? output-port?  input-port? 
           procedure?  rational?
           string? eof-object? null? real? symbol?)
   ;; -- equality --
   (export equal? eqv?)
   ;;  -- characters --
   (export char-alphabetic? 
	   char->integer
	   integer->char
	   char-ci<=? char-ci<?
	   char-ci=? char-ci>=? char-ci>? char-downcase
	   char-lower-case? char-numeric? 
	   char-upcase char-upper-case? char-whitespace?
	   char<=? char<? char=? char>=? char>?)
   ;; -- input --
   (export open-input-file char-ready?
	   read read-char peek-char 
	   call-with-input-file 
	   close-input-port
	   current-input-port
	   with-input-from-file)
   ;; -- output --
   (export open-output-file current-output-port
	   newline display call-with-output-file
	   close-output-port with-output-to-file write write-char)
   ;; -- strings --
   (export string->number
	   string->symbol
	   symbol->string
	   string-append
	   string-ci<=? string-ci<? string-ci=?
	   string-ci>=? string-ci>? string-copy
	   string-fill! string-length string-ref
	   string-set! string<=? string<? string=?
	   number->string
	   string>=? string>?
	   substring make-string string string->list)
   ;; vectors
   (export make-vector
	   vector vector->list
	   vector-fill! vector-length vector-ref
	   vector-set! vector?)
   ;; -- math --
   (export max  min modulo negative?
	   * + - / < <= = > >= abs acos cos  asin
	   atan ceiling log 
	   odd?  exp expt floor  even? inexact->exact inexact?
	   exact->inexact exact?
	   positive? quotient gcd lcm
	   remainder
	   tan sin sqrt zero? round truncate)
   (export make-rectangular imag-part real-part)
   ;; -- misc --
   (export call-with-current-continuation)
   ;;
   ;;  Section 2. RScheme Base
   ;;  =======================
   ;;
   (export cond-expand          ; SRFI-0
           if-implements)       ; extension thereto
   ;;  (macros are not R5RS compliant yet...)
   (export define-constant
	   define-macro
	   define-inline
	   define-rewriter ;; this underlies the `define-macro' impl.
	   let-syntax      ;; also underlies `define-method'
	   letrec-syntax
	   define-syntax)
   ;;
   ;;  -- dynamic variable --
   ;;
   (export define-thread-var
	   thread-let
           dynamic-wind)
   ;; why are these needed...?  some non-hygienic macro, perhaps in threads...
   (export
    gvec-length gvec?     ;; ??
    make-gvec gvec-ref dynamic-call-thunk  ; `thread-let'?
    get-dynamic-state-reg get-thread-state-reg ; `thread-let'
    add-thread-var! ;; `define-thread-var'
    indirect-thread-var-ref
    indirect-thread-var-set!)
   ;;
   ;;  -- Multiple Value Return --
   ;;
   (export values           ;; special form
	   values->list     ;; special form
	   bind             ;; special form
	   vector->values   ;; procedure
	   list->values     ;; procedure
	   )
   ;;
   ;;  -- Dequeues --
   ;;
   (export make-dequeue
	   dequeue-push-front!
	   dequeue-push-back!
	   dequeue-pop-front!
	   dequeue-pop-back!
	   dequeue-state
	   dequeue-count
	   dequeue-empty?
	   dequeue-ref
	   dequeue-set!)
   ;;
   ;;  -- Lists --
   ;;
   (export cons* range last any? every? reduce delq delq!
	   apply* select append! reverse!)
   ;;
   ;;  -- Vectors --
   ;;
   (export vector-append
	   subvector
	   vassq
	   vmemq
	   vector-map
	   vector-for-each
	   vector->string) ;; why no `string->vector'?
   ;;
   ;;  -- I/O --
   ;;
   (export display-object
	   write-object
	   current-error-port
	   <output-port>
	   <input-port>
	   flush-output-port
	   format
	   print
	   pp
	   write-string
	   with-output-to-string
	   open-input-string 
	   open-output-string)
   ;;
   ;;  -- Object System --
   ;;
   (export define-class
           define-generic-function
	   define-method
	   make
	   initialize
	   <object>
	   <string>
	   <symbol> <char>
	   <vector>
	   <pair>
	   <boolean>
	   <list>
	   <empty-list>
	   <number>
	   <complex>
	   <real>
	   <rational>
	   <integer>
	   <fixnum>     fixnum?
	   <double-float>
           <rect-complex>
	   <byte-vector>
	   <function>
	   <<class>>    class?)
   ;;
   (export name ;; common GFs
	   to-string)
   ;;
   ;;  -- MOP --
   ;;
   (export object-class
	   instance?
	   subclass?)
   ;;
   ;;  -- Condition System --
   ;;
   (export <condition>
	   <error>
	   handler-case
	   handler-bind
	   signal
	   error
	   assert)
   (export *handler-chain* set-handler-proc!) ;; OUCH
   ;;
   ;;  -- Module System --
   ;;
   (export define-module
	   define-module-extend
	   with-module)
   ;;
   ;;  -- Symbols/Keywords --
   ;;
   (export keyword?
	   flag?
	   symbol->keyword
	   symbol->flag
	   flag->symbol
	   keyword->symbol
	   symbol-append
	   gensym
           keyword-list->assoc-list)
   ;;
   ;;  -- Functional Composition --
   ;;
   (export curry
	   rcurry
	   compose
	   identity)
   ;;
   ;;  -- Bit Operations & other math --
   ;;
   (export add1 sub1 
	   logical-shift-left 
           logical-shift-right
           bitwise-and 
           bitwise-not
	   bitwise-xor
           bitwise-or)
   ;;
   ;;  -- Strings --
   ;;
   (export string-split string-join string-search)
   ;;
   ;;  -- System Operations --
   ;;
   (export getenv
	   process-exit
	   )
   ;; UNCATEGORIZED (TODO)
   (export find-next-method-1 ; `next-method'
	   %early-once-only ;; via `define-thread-var'?
	   random
           $Pi)
   ;;
   ;;  Section 3. RScheme Common
   ;;  =========================
   ;;
   ))
