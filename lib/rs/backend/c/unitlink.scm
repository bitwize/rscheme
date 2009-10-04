
(define (write-unit-linkage port
			    #key unit-name
			         parts
			         (bytecode-extensions default: '())
				 (private-interface-header default: #f)
				 (root-variable-info default: #f))
  (display-disclaimer (car parts) port)
  ;;
  (if private-interface-header
      (format port 
	      "#include \"~a\"\n\n"
	      private-interface-header))
  ;;
  (center-* port "Linkage for the `~a' module" unit-name)
  (newline port)
  ;;
  (if (pair? bytecode-extensions)
      (begin
	(format port "static struct bcx_descr bcx_tab[] = {\n")
	(for-each 
	 (lambda (x)
	   (format port "  { ~d, bc_~a_extension, ~s, &module_~a },\n" 
		   (car x)
		   (cadr x)
		   (cadr x)
		   unit-name))
	 bytecode-extensions)
	(format port "};\n")))
  ;;
  (newline port)
  (for-each
   (lambda (a-part)
     (format port
	     "extern struct part_descr ~a_part_~a;\n"
	     unit-name
	     (link-name a-part)))
   parts)
  (newline port)
  (format port "static struct part_descr *(parts_table[]) = {\n")
  (for-each
   (lambda (a-part)
     (format port "    &~a_part_~a,\n" unit-name (link-name a-part)))
   parts)
  (format port "    (struct part_descr *)0 };\n")
  (newline port)
  (format port
	  "struct module_descr module_~a = { \"~a\", parts_table,\n"
	  unit-name 
	  unit-name)
  (if root-variable-info
      (begin
	(format port "\t~a,\n" (car root-variable-info))
	(format port "\t~a,\n" (cadr root-variable-info))
	(format port "\t(struct root_info *)0,\n\n"))
      (begin
	(format port "\t0 /* num roots */,\n")
	(format port "\t(obj *)0,\n")
	(format port "\t(struct root_info *)0,")))
  ;;
  (if (null? bytecode-extensions)
      (begin
	(format port "\t(struct bcx_descr *)0, /* bc_extensions */\n")
	(format port "\t0 /* num_bc_extensions */\n"))
      (begin
	(format port "\tbcx_tab, /* bc_extensions */\n")
	(format port "\t~d /* num_bc_extensions */\n"
		(length bytecode-extensions))))
  ;;
  (format port "    };\n\n")
  ;;
  (let ((n unit-name))
    (format port "struct module_descr *RS_module_~a = &module_~a;\n" n n)
    (format port "struct module_descr *RS_fm_~a( void )\n" n)
    (format port "{ return &module_~a; }\n" n)))
