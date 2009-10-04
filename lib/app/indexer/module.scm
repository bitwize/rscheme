;;; -*-Scheme-*-

(define-module app.indexer ()
  (&module
   (import usual-inlines)
   (import tables)
   (import regex)
   ;;
   (import calendar)  ;; for string->time
   (import syscalls)  ;; for time->string
   ;;
   (import rs.db.lss)
   (import rs.db.rstore)
   (import rs.util.properties)
   (import rs.util.msgs)
   ;;
   (load "bitset.scm")
   (load "app.scm")
   (load "document.scm")
   ;;
   (load "email.scm")
   (load "parse-text.scm")
   (load "parse-domain.scm")
   (load "parse-addr.scm")
   (load "parse-email.scm")
   ;;
   (load "adder.scm")
   ;;
   (load "query.scm")
   ;;
   (load "dpi.scm")  ;; Data/Program Interface
   ;;
   (export make-document-index    ;; CREAT
	   open-document-index    ;; RW
	   access-document-index  ;; RO
	   close-document-index
	   add-document
	   eval-query)))
