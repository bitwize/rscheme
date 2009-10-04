(define-module util.readline (meta)
  (meta
   (import usual-inlines)
   (import rs.sys.threads.manager)
   (import rs.net.nvt)
   (import editinp syscalls tables))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-syntax (while expr . body)
    (let loop ()
      (if expr
	  (begin
	    (begin . body)
	    (loop)))))
  
  (meta
   (load "state.scm")
   (load "editport.scm")
   
   (load "colout.scm")
   (load "keymap.scm")
   (load "readline.scm")
   (load "termattr.scm")
   (load "completion.scm")
   (load "open.scm")
   (load "isearch.scm")))
