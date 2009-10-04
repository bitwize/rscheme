
(define (unparse-consts form)
  (cond
   ((pair? form)
    (cons-dif form
	      (unparse-consts (car form))
	      (unparse-consts (cdr form))))
   ((symbol? form) 
    form)
   ((string? form)
    (format #f "~s" form))
   ((char? form)
    (case form
      ((#\newline) "'\\n'")
      ((#\tab) "'\\t'")
      ((#\return) "'\\r'")
      (else
       (if (< (char->integer form) 32)
	   (format #f "'\\~03o'" (char->integer form))
	   (format #f "'~a'" form)))))
   (else
    form)))
   

(define-patterns unparse
  ;;
  ;; expressions
  ;;
  ;;
  ((field (ref ?x) ?y)         ("(" ?x ")->" ?y))
  ((field ?x ?y)               ("(" ?x ")." ?y))
  ;;
  ((aref ?x ?y)                ("" ?x "[" ?y "]"))
  ((sizeof ?t)                 ("sizeof( " (c-type ?t "") ")"))
  ((ref ?x)                    ("*(" ?x ")"))
  ((addr ?x)                   ("&(" ?x ")"))
  ;;
  ((inc! ?x)                   ("++" ?x))
  ((dec! ?x)                   ("--" ?x))
  ((arithmetic-shift ?x (- ?y))("(" ?x " >> " ?y "))"))
  ((arithmetic-shift ?x ?qu)   ("(" ?x " >> " ?-qu ")") 
			       where: ((?-qu (- ?qu)))
			       when: (< ?qu 0))
  ((arithmetic-shift ?x ?y)    ("(" ?x " << " ?y ")"))
  ((bitwise-not ?x)            ("~(" ?x ")"))
  ((bitwise-and ?x ?y)         ("(" ?x " & " ?y ")"))
  ((bitwise-xor ?x ?y)         ("(" ?x " ^ " ?y ")"))
  ((bitwise-or ?x ?y)          ("(" ?x " | " ?y ")"))
  ;;
  ((+ ?x ?y)                   ("(" ?x " + " ?y ")"))
  ((- ?x ?y)                   ("(" ?x " - " ?y ")"))
  ((* ?x ?y)                   ("(" ?x " * " ?y ")"))
  ((/ ?x ?y)                   ("(" ?x " / " ?y ")"))
  ((modulo ?x ?y)              ("(" ?x " % " ?y ")"))
  ;
  ((= ?x ?y)                   ("(" ?x " == " ?y ")"))
  ((< ?x ?y)                   ("(" ?x " < " ?y ")"))
  ((> ?x ?y)                   ("(" ?x " > " ?y ")"))
  ((<= ?x ?y)                  ("(" ?x " <= " ?y ")"))
  ((>= ?x ?y)                  ("(" ?x " >= " ?y ")"))
  ;
  ((and ?x ?y)                 ("(" ?x " && " ?y ")"))
  ((or ?x ?y)                  ("(" ?x " || " ?y ")"))
  ((not ?x)                    ("(!" ?x ")"))
  ;
  ((as ?t ?x)                  ("(" (c-type ?t "") ")" ?x))
  ;
  ((call ?proc ?arg ...)       ("" ?proc "( " (arg-list ?arg ...) " )"))
  ((arg-list)                  (""))
  ((arg-list ?x)               ("" ?x))
  ((arg-list ?x . ?r)          (", " ?x (arg-list . ?r)))
  ;
  ;
  ;; statements
  ;;
  ;---------------------------------------------------------------------------
  ((case ?e . ?c)            ("switch (" ?e ") {"
				(#\tab #\newline
				 (case-labels . ?c)) 
				"}" #\newline))
  ((case-labels)               (""))
  ((case-labels (else . ?s) . ?r)("default:" #\newline (case-block . ?s)
				   (case-labels . ?r)))
  ((case-labels (?c . ?s) . ?r)("" (case-labels* . ?c) (case-block . ?s)
				   (case-labels . ?r)))
  ((case-labels*)              (""))
  ((case-labels* ?x . ?r)      ("case " ?x ":" #\newline (case-labels* . ?r)))
  ((case-block . ?s)           ("{" (#\tab #\newline 
				     (begin-contents . ?s) #\newline 
				     "break;" #\newline 
				    ) "}" #\newline))
  ;---------------------------------------------------------------------------
  ((go ?x)                     ("goto " ?x))
  ((if ?c ?s1)                 ("if (" ?c ")" #\tab #\newline ?s1))
  ((if ?c ?s1 ?s2)             ("if (" ?c ")" #\tab #\newline 
				       ("{" #\tab ?s1  ";" #\newline "}")
				       #\newline "else" #\newline
				       ("{" #\tab ?s2  ";" #\newline "}")))
  ((set! ?x ?y)                ("" ?x " = " ?y))
  ((let ?decl . ?s)            ("{" (#\tab #\newline
				     (var-decls . ?decl)
				     (begin-contents . ?s)
				     #\newline)
				"}"))
  ((var-decls)                 (""))
  ((var-decls ?v . ?r)         ("" (var-decl ?v) (var-decls . ?r)))
  ((var-decl ((?v ?t) ?init))  ("" (c-type ?t ?v) " = " ?init ";" #\newline))
  ((var-decl ((?v ?t)))        ("" (c-type ?t ?v) ";" #\newline))
  ;
  ((for ?x ?y ?z . ?s)         ("for (" ?x ";" ?y ";" ?z ")" #\tab
					#\newline
					(begin . ?s)))
  ((while ?p . ?s)             ("while ( " ?p " ) " #\Tab
					   #\newline (begin . ?s)))
  ((return ?x)                 ("return " ?x))
  ;;
  ;; preprocessor
  ;;
  ((include ?file)             ("#include " ?file #\newline))
  ((include :sys ?file)        ("#include <" ?file ">" #\newline))
  ;;
  ;; top-level definitions
  ;;
  ;; translate `:public' entries
  ;;
  ((define-type ?t1 :public ?t2)       (:header (define-type ?t1 ?t2)))
  ((define-extern ?var :public ?type)  (:header (define-extern ?var ?type)))
  ((define (?fn . ?args) :public . ?b) ((:header (define (?fn . ?args)))
					(define (?fn . ?args) . ?b)))
  ((define ?var :public ?type ?init)   ((:header (define-extern ?var ?type))
					(define ?var ?type ?init)))
  ((define ?var :public ?type)         ((:header (define-extern ?var ?type))
					(define ?var ?type)))
  ;;
  ((define-type ?t1 ?t2)           ("typedef " (c-type ?t2 (c-type-name ?t1))
					       ";"
					       #\newline))
  ((define-extern ?var ?type)      ("extern " (c-type ?type ?var) ";"
					      #\newline))
  ((define (?fn . ?args) . ?body)  ("" 
				    (c-type (fn-ret . ?args) ?fn) 
				    "(" (fn-args . ?args) ")" #\Tab
				   #\newline (begin . ?body)))
  ((define ?var ?type ?init)      ((c-type ?type ?var) " = " ?init ";"
						       #\newline))
  ((define ?var ?type)            ((c-type ?type ?var) ";" #\newline))
  ((array . ?elems)               ("{" (array-elems . ?elems) " }"))
  ((array-elems)                  (""))
  ((array-elems ?e)               (" " ?e))
  ((array-elems ?e . ?r)          (" " ?e "," (array-elems . ?r)))
  ;
  ((fn-ret)                       <void>)
  ((fn-ret => ?t)                 ?t)
  ((fn-ret ?x . ?rest)            (fn-ret . ?rest))
  ;
  ((c-type <ptr> ?v)              (c-type (ptr-to <void>) ?v))
  ;
  ((c-type <void> ?v)             ("void " ?v))
  ((c-type <int> ?v)              ("int " ?v))
  ((c-type <long> ?v)             ("long " ?v))
  ((c-type <short> ?v)            ("short " ?v))

  ((c-type <uint> ?v)             ("unsigned int " ?v))
  ((c-type <ulong> ?v)            ("unsigned long " ?v))
  ((c-type <ushort> ?v)           ("unsigned short " ?v))
  ((c-type <size> ?v)             ("size_t " ?v))

  ((c-type <byte> ?v)             ("unsigned char " ?v))
  ((c-type <char> ?v)             ("char " ?v))

  ((c-type (ptr-to ?x) ?v)        ("" (c-type ?x ("*" ?v))))
  ((c-type (array-of ?x) ?v)      ("" (c-type ?x ("" ?v "[]"))))
  ((c-type (array-of ?x ?n) ?v)   ("" (c-type ?x ("" ?v "[" ?n "]"))))
  ((c-type ?x:s ?v)               ("" (c-type-name ?x:s) " " ?v))
  ((c-type (typename ?x) ?v)      ("" ?x " " ?v))
  ;
  ((c-type-name <obj>)            ("obj"))
  ((c-type-name <env>)            ("RSEnv"))
  ((c-type-name <file>)           ("FILE"))
  ((c-type-name (typename ?x))    ("" ?x))
  ;
  ((c-type (struct ?x) ?v)        ("struct " ?x " " ?v))
  ((c-type (struct ?x . ?s) ?v)   ("struct " ?x " {" (#\tab #\newline
						      (struct-slots . ?s))
					     #\newline "} " ?v))
  ((struct-slots)              (""))
  ((struct-slots ?s1 . ?r)     ("" (var-decl (?s1)) (struct-slots . ?r)))
  ;
  ((fn-args)                   (""))
  ((fn-args => . ?rest)        (""))
  ((fn-args (?x ?t))           ("" (c-type ?t ?x)))
  ((fn-args (?x ?t) => . ?r)   ("" (c-type ?t ?x)))
  ((fn-args (?x ?t) . ?rest)   ("" (c-type ?t ?x) ", " (fn-args . ?rest)))
  ;
  ((begin . ?code)             ("{" (#\Tab #\newline (begin-contents . ?code))
				    #\newline "}"))
  ((begin-contents)            (""))
  ((begin-contents ?x)         ("" ?x ";"))
  ((begin-contents ?x . ?rest) ("" ?x ";" #\newline
				   (begin-contents . ?rest))))

;;;

(define (print-c form #optional (port default: (current-output-port))
		                (header-port default: #f))
  (let ((x (unparse (unparse-consts form))))
    (if header-port
	(set! x (divert-header-material x header-port)))
    (with-output-to-port
	port
      (lambda ()
	(cprint x)
	(newline)))))

(define (divert-header-material item port)
  (if (pair? item)
      (if (eq? (car item) ':header)
	  (begin
	    (with-output-to-port 
		port
	      (lambda ()
		(cprint (cdr item))
		(newline)))
	    "")
	  (cons-dif item
		    (divert-header-material (car item) port)
		    (divert-header-material (cdr item) port)))
      item))

