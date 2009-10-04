(define-module rs.sys.paths ()
  (&module
   (import paths)
   (export <file-name>
	   <directory-name>
	   string->file
	   string->dir
	   pathname->string
	   pathname->os-path
	   append-path
	   append-dirs)))
