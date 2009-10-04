#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/iolib/stdio.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.10
 | File mod date:    2000-04-08 10:58:15
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  iolib
 |
 | Purpose:          bytecoded extension primops
 |                   provide access to C's stdio,
 |                  structured as a bytecode interpreter extension
 `------------------------------------------------------------------------|#

(define-syntax (stdio-extension) 10)

(define-primop (stdin) => <obj>
  (ccode "stdiox_stdin")
  (bytecode (stdio-extension) 0))

(define-primop (stdout) => <obj>
  (ccode "stdiox_stdout")
  (bytecode (stdio-extension) 1))

(define-primop (stderr) => <obj>
  (ccode "stdiox_stderr")
  (bytecode (stdio-extension) 2))

(define-primop (fopen <raw-string> <raw-string>) => <obj>
  (ccode "stdiox_fopen")
  (bytecode (stdio-extension) 3))

(define-primop (fclose <obj>) => <raw-int> ;; returns fclose() rc
  (ccode "stdiox_fclose")
  (bytecode (stdio-extension) 4))

(define-primop (fwrite/str <obj> <string>) => <raw-int>
  (ccode "stdiox_fwrite_str")
  (bytecode (stdio-extension) 5))

(define-primop (fflush <obj>) => <raw-int>
  (ccode "stdiox_fflush")
  (bytecode (stdio-extension) 7))

(define-primop (ftell <obj>) => <raw-int>
  (ccode "stdiox_ftell")
  (bytecode (stdio-extension) 8))

(define-primop (fseek <obj> <raw-int> <raw-int>) => <raw-int>
  (ccode "stdiox_fseek")
  (bytecode (stdio-extension) 9))

(define-primop (fread-fill <obj> <bvec> <raw-int> <raw-int>) => <raw-int>
  (ccode "stdiox_fread_fill")
  (bytecode (stdio-extension) 10))


(define-primop (fputc <obj> <ascii-char>)
  (ccode "stdiox_fputc")
  (bytecode (stdio-extension) 11))

(define-primop (fgetc <obj>) => <obj> ;; #f on EOF, else an <ascii-char>
  (ccode "stdiox_fgetc")
  (bytecode (stdio-extension) 12))

;; only handles up to 1K lines
(define-primop (fgets <obj>) => <obj> ;; #f on EOF, else a <string>
  (ccode "stdiox_fgets")
  (bytecode (stdio-extension) 13))

(define-primop (popen <raw-string> <raw-string>) => <obj>
  (ccode "stdiox_popen")
  (bytecode (stdio-extension) 14))

(define-primop (fgetln <obj>) => <obj>
  (ccode "stdiox_fgetln")
  (bytecode (stdio-extension) 15))

(define-primop (pclose <obj>) => <raw-int>
  (ccode "stdiox_pclose")
  (bytecode (stdio-extension) 16))

;;
;; error handling stuff
;;

(define-primop (ferror <obj>) => <raw-int>
  (ccode "stdiox_ferror")
  (bytecode (stdio-extension) 17))

(define-primop (clearerr <obj>)
  (ccode "stdiox_clearerr")
  (bytecode (stdio-extension) 18))

(define-primop (feof <obj>) => <raw-bool>
  (ccode "stdiox_feof")
  (bytecode (stdio-extension) 19))

(define-primop (fcanget <obj>) => <raw-int>
  (ccode "stdiox_fcanget")
  (bytecode (stdio-extension) 20))

(define-primop (fpeekc <obj>) => <obj> ;; #f on EOF, else an <ascii-char>
  (ccode "stdiox_fpeekc")
  (bytecode (stdio-extension) 21))

