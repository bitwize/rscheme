#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/regex/rxcompil.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.8
 | File mod date:    2006-01-28 16:56:27
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  regex
 |
 | Purpose:          regex compiler (icode from nested expr)
 `------------------------------------------------------------------------|#

(define (push-branch node)
  (make <pda-state>
        data: (list node)
	opcode: $branch))

;;
;;  *reg-space* is used to keep track of what the save_array[]
;;  is being used for.  Each element in *reg-space* denotes a
;;  PAIR of save_array[] entries.  The first element is always
;;  the name "substring", because the string boudaries are always
;;  stored in save_array[0,1]

(define *reg-space* '())

(define (compile-reg-expr rexpr)
  (fluid-let ((*reg-space* (cons 'substring '())))
    (bind ((first last min-len (compile-reg-expr-sub rexpr)))
      (set-next! last (make <pda-state>
			    opcode: $accept))
      (values first *reg-space*))))

;;
;;   returns three values:
;;	[0] => the first <pda-state> for the regexpr recognition
;;	[1] => the last <pda-state> for the regexpr recognition
;;	[2] => lower bound on length of recognized string (0=may recognize e)

(define (unit-pda node count)
  (let ((temp node))
    (values temp temp count)))

(define (compile-reg-expr-sub rexpr)
  (bind ((first-state last-state at-least-one? 
		      (compile-reg-expr-dispatch rexpr)))
    (assert (not (next last-state)))
    (values first-state last-state at-least-one?)))

(define (empty-string)
  (let ((n (make <pda-state>
		 opcode: $nop)))
    (values n n 0)))

;; returns #t iff the pda matches exactly the empty string
;; in a straightforward way (analagous to the way single-char-form decides,
;; and in fact, is in support of that function by being used
;; to compress out empty strings in sequences)

(define (empty-string? first last)
  (and (eq? first last)
       (eq? (opcode first) $nop)))

(define (compile-reg-expr-dispatch rexpr)
  (cond
   ((pair? rexpr)
      (case (car rexpr)
	((or)
	 (compile-reg-expr-or rexpr))
	((seq)
	 (compile-reg-expr-seq rexpr))
	((+ * ?)
	 ;; +, ? and * are handled by the same code,
	 ;; because the pda structure is basically the same
	 ;; -- the only diff is whether
	 ;; to START with the sub-expr node or the
	 ;; branch node, and where the sub-expr's NEXT should
	 ;; point to
	 (compile-reg-expr-kleene rexpr))
	((bound)
	 (compile-reg-expr-bound rexpr))
	((range)
	 (compile-reg-expr-range rexpr))
	((not)
	 (compile-reg-expr-not rexpr))
	((let)
	 (compile-reg-expr-let rexpr))
	((save)
	 (compile-reg-expr-save rexpr))
	((prefix)
	 (compile-reg-expr-prefix rexpr))
	((suffix)
	 (compile-reg-expr-suffix rexpr))
	((entire)
	 (compile-reg-expr-entire rexpr))
        ((posix)
         (compile-reg-expr-dispatch (parse-posix-regex (cadr rexpr))))
	(else
	 (error "compile-reg-expr: unrecognized form: ~s" rexpr))))
   ((null? rexpr)
    (error "the empty list is an invalid rexpr"))
   ;;
   ;; must be an atom...
   ;;
   ((vector? rexpr)
    ;; allow direct specification of character bit vectors
    (if (and (eq? (vector-length rexpr) 32)
	     (every? (lambda (i)
		       (and (fixnum>=? i 0)
			    (fixnum<? i 256)))
		     (vector->list rexpr)))
	(char-set->pda rexpr)
	(error "compile-reg-expr: apparent bit vector not valid: ~s" rexpr)))
   ;;
   ((string? rexpr)
    (compile-reg-expr-string rexpr))
   ;;
   ((char? rexpr)
    (unit-pda (make <pda-state>
		    opcode: $match-char
		    data: (list (char->integer rexpr)))
	      1))
   ;;
   ((eq? rexpr 'any)
    (unit-pda (make <pda-state>
		    opcode: $match-any)
	      1))
   ((eq? rexpr 'eos)
    (unit-pda (make <pda-state>
		    opcode: $match-end)
	      0))
   ;;
   ;; see if it's a defined macro
   ;;
   (else
    (let ((macro (assq rexpr *regexp-macros*)))
      (if macro
	  (if (vector? (cdr macro))
	      (char-set->pda (cdr macro))
	      (compile-reg-expr-sub (cdr macro)))
	  (error "compile-reg-expr: unrecognized form: ~s" rexpr))))))

(define *regexp-macros*
  '((digit . #(0 0 0 0 0 0 255 192 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)) 
    (alpha . #(0 0 0 0 0 0 0 0 127 255 255 224 127 255 255 224 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
    (uppercase . #(0 0 0 0 0 0 0 0 127 255 255 224 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)) 
    (lowercase . #(0 0 0 0 0 0 0 0 0 0 0 0 127 255 255 224 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)) 
    (hex-digit . #(0 0 0 0 0 0 255 192 126 0 0 0 126 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)) 
    (printable . #(0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 254 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
    (space . #(0 96 0 0 128 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))))

#|
;; to generate a new vector from a charset expr, you can say:
      (data (compile-reg-expr <expr>))

  '((digit (range #\0 #\9))
    (alpha (or uppercase lowercase))
    (uppercase (range #\A #\Z))
    (lowercase (range #\a #\z))
    (space (or #\space #\tab #\lf))
    (hex-digit (or (range #\0 #\9) 
		   (range #\A #\F) 
		   (range #\a #\f)))
    (printable (range #\space #\~))
    (any (not))))
|#
(define (compile-reg-expr-any)
  (unit-pda (make <pda-state>
		  opcode: $match-any)
	    1))

(define (compile-reg-expr-char (ch <ascii-char>))
  (let ((n (make <pda-state>
		 opcode: $match-char
		 data: (cons (char->integer ch) '()))))
    (values n n 1)))

(define (compile-reg-expr-string (str <string>))
  (let ((n (make <pda-state>
		 opcode: $match-str
		 data: (cons (string-length str)
			     (map char->integer (string->list str)))))) 
    (values n n (string-length str))))


(define (char-set->pda vec)
  (unit-pda (make <pda-state>
		  opcode: $match-char-set
		  data: vec)
	    1))


(define (compile-reg-expr-not (rexpr <pair>))
  (syntax-check rexpr 2)
  (bind ((first last len (compile-reg-expr-sub (cadr rexpr))))
    (if (and (eq? len 1)
	     (single-char-form first last))
	(char-set->pda (bit-vector-not (single-char-bit-vector first)))
	(error "compile-rex-expr: not can only be used around a primitive expr: ~s" rexpr))))

;; match a range of characters

(define (compile-reg-expr-range (rexpr <pair>))
  (syntax-check rexpr 3)
  (let ((from (cadr rexpr))
	(to (caddr rexpr)))
    (if (and (char? from)
	     (char? to)
	     (char<=? from to))
	(let ((v (make-vector 32 0)))
	  ;(format #t "range: ~s - ~s\n" from to)
	  (let ((max (char->integer to)))
	    (let range-loop (((i <fixnum>) (char->integer from)))
	      (vector-bit-set! v i)
	      (if (eq? i max)
		  (char-set->pda v)
		  (range-loop (add1 i))))))
	(error "compile-reg-expr: invalid `range' use: ~s" rexpr))))


(define (compile-reg-expr-seq (rexpr <pair>))
  (if (null? (cdr rexpr))
      (empty-string)
      (bind ((first last min (compile-reg-expr-sub (cadr rexpr))))
	(if (and (eq? min 0)
		 (empty-string? first last))
	    (compile-reg-expr-seq (cdr rexpr))
	    (let loop ((items (cddr rexpr))
		       (last last)
		       (min min))
	      (if (null? items)
		  (values first last min)
		  (bind ((nxt new-last n (compile-reg-expr-sub (car items))))
		    (if (and (eq? n 0)
			     (empty-string? nxt new-last))
			(loop (cdr items) last min)
			(begin
			  ;;
			  ;; check for the special case of creating
			  ;; a sequence by appending a $match-str or $match-char
			  ;; onto another $match-str or $match-char
			  ;;
			  (if (and (match-literal-string? last)
				   (match-literal-string? nxt))
			      ;;
			      ;; in which case, we want to just join them
			      ;; together
			      ;;
			      (begin
				(set-next! last (next nxt))
				(merge-literal-string-search! last nxt)
				(if (eq? nxt new-last)
				    (set! new-last last)))
			      (set-next! last nxt))
			  ;; 
			  ;; now, loop for the remaining items in the
			  ;; sequence
			  ;;
			  (loop (cdr items) new-last (+ min n)))))))))))

(define (match-literal-string? node)
  (or (eq? (opcode node) $match-char)
      (eq? (opcode node) $match-str)))

;; 
;; returns a list of the character codes matched
;; by a node which satisifies match-literal-string?
;;

(define (character-codes-literally-matched node)
  (if (eq? (opcode node) $match-char)
      (data node)
      (begin
	;; just for grins, make sure this data string
	;; makes sense for what we're expecting, which in 
	;; this case is a length-encoded string
	;;
	(assert (eq? (car (data node))
		     (length (cdr (data node)))))
	(cdr (data node)))))

;;
;; merges the literal string searching that `new' does
;; into that which `target' does.
;;

(define (merge-literal-string-search! target new)
  (let ((new-str (append (character-codes-literally-matched target)
			 (character-codes-literally-matched new))))
    (set-opcode! target $match-str)
    (set-data! target (cons (length new-str) new-str))))
  
(define (syntax-check rexpr n)
  (if (not (eq? (length rexpr) n))
      (error "reg-expr operator `~a' expects exactly ~a argument~p, got ~d"
	     (car rexpr)
	     (vector-ref '#("zero" "one" "two" "three") (- n 1))
	     (- n 1)
	     (- (length rexpr) 1))))

(define (compile-reg-expr-bound (rexpr <pair>))
  (syntax-check rexpr 3)
  (bind ((first last leastlen (compile-reg-expr-sub (caddr rexpr)))
         (min-len (car (cadr rexpr)))
         (max-len (if (pair? (cdr (cadr rexpr)))
                      (cadadr rexpr)
                      #f))
         (n (make <pda-state>
                  opcode: $repeat-n-times
                  data: (+ min-len (* max-len 16384)))))
    (values n n (* min-len leastlen))))
        
    
(define (compile-reg-expr-kleene (rexpr <pair>))
  (syntax-check rexpr 2)
  (bind ((first last leastlen (compile-reg-expr-sub (cadr rexpr))))
    ;;
    ;; `+' and `*' are invalid when their operand matches the empty string
    ;;
    (if (and (eq? leastlen 0)
	     (not (eq? (car rexpr) '?)))
	(error "regexpr operator `~s' operand `~s' might match empty string" 
	       (car rexpr) 
	       (cadr rexpr)))
    ;;
    ;; check for special cases for single-char-eating * and + operands
    (let ((arg (and (eq? leastlen 1)
		    (not (eq? (car rexpr) '?))
		    (single-char-form first last))))
      (if arg
	  (let ((n (make <pda-state>
			 opcode: (if (eq? (car rexpr) '*)
				     $match-star
				     $match-plus)
			 data: (list arg))))
	    (values n n (if (eq? (car rexpr) '*)
			    0
			    1)))
	  ;;
	  ;; not a single-char-eating * or +, so compile the 
	  ;; fully general loop case
	  ;;
	  (let* ((j (make <pda-state>
			  opcode: $nop))
		 (br (make <pda-state>
			   opcode: $branch
			   next: j
			   data: (list first))))
	    (if (eq? (car rexpr) '?)
		(set-next! last j)
		(set-next! last br))
	    (if (eq? (car rexpr) '+)
		(values first j leastlen)
		(values br j 0)))))))

;;
;; in implementation, the `let' value indicates which
;; pair of VM registers (starting at REG2) is used to hold
;; the offset
;;   so (let 0 ...) is stored in REG2/REG3
;;  and (let 1 ...) is stored in REG4/REG5
;; etc.

;;
;; the function takes a tag to denote where to save stuff
;; tag 0 is for the entire string itself
;;

(define (declare-reg tag)
  (cond
   ((symbol? tag)
    (let ((t (length *reg-space*))
	  (m (memq tag *reg-space*)))
      (if m
	  (- t (length m))
	  (begin
	    (append! *reg-space* (cons tag '()))
	    t))))
   ((eq? tag #f)
    (let ((t (length *reg-space*)))
      (append! *reg-space* (cons t '()))
      t))
   ((fixnum? tag)
    (let loop ((n (length *reg-space*)))
      (if (>= tag n)
	  (begin
	    (append! *reg-space* (cons #f '()))
	    (loop (+ n 1)))
	  (begin
	    (list-set! *reg-space* tag tag)
	    tag))))
   (else
    (error "~s: invalid regexpr register tag" tag))))

(define (savers tag0)
  ;;
  (let ((tag (declare-reg tag0)))
    ;;
    (if (or (< tag 1) (> tag 20))
	(error "compile-reg-expr: save tag ~s cannot be represented" tag0))
    ;;
    (values (make <pda-state>
		  opcode: $save-place
		  data: (list (* tag 2)))
	    (make <pda-state>
		  opcode: $save-place
		  data: (list (+ (* tag 2) 1))))))
  
(define (compile-reg-expr-let (rexpr <pair>))
  (syntax-check rexpr 3)
  (bind ((save-start save-end (savers (cadr rexpr)))
	 (first last leastlen (compile-reg-expr-sub (caddr rexpr))))
    (set-next! save-start first)
    (set-next! last save-end)
    (values save-start save-end leastlen)))

(define (compile-reg-expr-save (rexpr <pair>))
  (syntax-check rexpr 2)
  (bind ((save-start save-end (savers #f))
	 (first last leastlen (compile-reg-expr-sub (cadr rexpr))))
    (set-next! save-start first)
    (set-next! last save-end)
    (values save-start save-end leastlen)))

;;
;;

(define (compile-reg-expr-prefix rexpr)
  (syntax-check rexpr 2)
  (bind ((first last min (compile-reg-expr-sub (cadr rexpr))))
    (values (make <pda-state>
		  opcode: $match-start
		  next: first)
	    last 
	    min)))

(define (compile-reg-expr-suffix rexpr)
  (syntax-check rexpr 2)
  (bind ((first last min (compile-reg-expr-sub (cadr rexpr)))
	 (very-last (make <pda-state> 
			  opcode: $match-end)))
    (set-next! last very-last)
    (values first very-last min)))

(define (compile-reg-expr-entire rexpr)
  (syntax-check rexpr 2)
  (bind ((first last min (compile-reg-expr-sub (cadr rexpr)))
	 (very-last (make <pda-state>
			  opcode: $match-end)))
    (set-next! last very-last)
    (values (make <pda-state>
		  opcode: $match-start
		  next: first)
	    very-last
	    min)))

;;
;; look at a pda node and determine whether or not the
;; machine is anchored
;;

(define (should-anchor? node)
  (eq? (opcode node) $match-start))
