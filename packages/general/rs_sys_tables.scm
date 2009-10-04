(define-module rs.sys.tables ()
  (&module
   (import corelib tables)
   (export make-table
	   table?
	   table-for-each
	   make-string-table
	   make-string-ci-table
	   make-fixnum-table
	   make-symbol-table
           table-size
	   table-lookup
	   table-insert!
	   table-remove!
	   table-key-present?
	   hash-code
	   integer->hash
           key-sequence
           value-sequence
	   char->hash
	   <table>
	   <string-table>
	   <string-ci-table>
	   <symbol-table>
           <object-table>)))
