#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/corelib/linkinfo.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.17
 | File mod date:    2004-03-24 14:41:02
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  corelib
 |
 | Purpose:          Provide scheme interface to C units (modules)
 `------------------------------------------------------------------------|#

(define-glue (get-c-units)
{
  REG0 = get_c_units();
  RETURN1();
})

(define-glue (flush-link-data)
{
  REG0 = flush_link_data();
  RETURN1();
})

(define-glue (set-c-module-descr-loaded-from! c_module lf)
{
  struct module_descr *m = (struct module_descr *)OBJ_TO_RAW_PTR(c_module);
  m->loaded_from = strdup( string_text( lf ) );
  RETURN0();
})
  
(define-glue (get-c-module-descr c_module)
{
  struct module_descr *m = (struct module_descr *)OBJ_TO_RAW_PTR(c_module);
  struct part_descr **p;
  obj lst = NIL_OBJ;

  for (p=m->parts; *p; p++)
    {
      lst = cons( RAW_PTR_TO_OBJ(*p), lst );
    }
  REG0 = make_string( m->name );
  REG1 = lst;
  REG2 = int2fx( m->num_roots );
  REG3 = m->loaded_from ? make_string( m->loaded_from ) : FALSE_OBJ;
  RETURN(4);
})

(define-glue (c-module-root-ref c_module ko)
{
  struct module_descr *m = (struct module_descr *)OBJ_TO_RAW_PTR(c_module);
  int k = fx2int(ko);

  if ((k < 0) || (k >= m->num_roots)) {
    scheme_error( "bad root index ~s", 1, ko );
  }
  REG0 = m->root_vector[ k ];
  RETURN1();
})

(define-glue (c-module-root-set! c_module ko item)
{
  struct module_descr *m = (struct module_descr *)OBJ_TO_RAW_PTR(c_module);
  int k = fx2int(ko);
  obj old;

  if ((k < 0) || (k >= m->num_roots)) {
    scheme_error( "bad root index ~s", 1, ko );
  }
  old = m->root_vector[k];   /* return the old value */
  m->root_vector[k] = item;
  REG0 = old;
  RETURN1();
})

(define-glue (get-c-part-descr part_ptr)
{
  struct part_descr *p = (struct part_descr *)OBJ_TO_RAW_PTR(part_ptr);
  struct function_descr **f;
  obj lst = NIL_OBJ;

  for (f=p->functions; *f; f++)
    {
      lst = cons( RAW_PTR_TO_OBJ(*f), lst );
    }
  REG0 = make_string( p->name );
  REG1 = int2fx( p->tag );
  REG2 = lst;
  REG3 = RAW_PTR_TO_OBJ( p->in_module );
  REG4 = p->sccsid ? make_string( p->sccsid ) : FALSE_OBJ;
  RETURN(5);
})

(define-glue (get-c-function-descr fn_d)
{
  struct function_descr *f = (struct function_descr *)OBJ_TO_RAW_PTR(fn_d);
  jump_addr *m;
  obj prev, lst;

  if (f->in_part->tag == STUB_PART_TAG) {
    f = resolve_function_descr( f );
  }

  lst = cons( JUMP_ADDR_TO_OBJ(f->monotones[0]), NIL_OBJ );

  prev = lst;
  for (m=f->monotones+1; *m; m++)
    {
      obj cell = cons( JUMP_ADDR_TO_OBJ(*m), NIL_OBJ );
      gvec_write_fresh_ptr( prev, SLOT(1), cell );
      prev = cell;
    }
  REG0 = make_string( f->name );
  REG1 = lst;
  REG2 = RAW_PTR_TO_OBJ( f->in_part );
  RETURN(3);
})

(define-glue (find-linked-module name)
{
  struct module_descr *m;

  m = find_module( string_text(name) );
  REG0 = m ? RAW_PTR_TO_OBJ(m) : FALSE_OBJ;
  RETURN1();
})

(define-glue (find-part-in-linked-module module part)
{
  struct part_descr *p;

  p = find_part( (struct module_descr *)OBJ_TO_RAW_PTR(module), fx2int(part) );
  REG0 = p ? RAW_PTR_TO_OBJ(p) : FALSE_OBJ;
  RETURN1();
})

(define-glue (find-code-ptr-in-part part fn_num)
{
  struct function_descr *f;
  struct part_descr *p = (struct part_descr *)OBJ_TO_RAW_PTR(part);

  f = p->functions[ fx2int(fn_num) ];
  REG0 = RAW_PTR_TO_OBJ( f->monotones[0] );
  REG1 = RAW_PTR_TO_OBJ( f );
  RETURN(2);
})

(define-class <dynamic-link-error> (<condition>)
  (arguments type: <vector>)
  (message type: <string>))

(define-method display-object ((self <dynamic-link-error>) port)
  (__format port "** dynamic linkage error in:\n   ~a\n" 
	    (gvec-ref (arguments self) 0))
  (__format port "   ~a\n" (message self)))

(define (dl-error file)
  (signal (make <dynamic-link-error>
		arguments: (make-gvec <vector> file)
		message: (dl-error-message))))

(define-glue (dl-error-message)
  literals: ("<no message>")
{
  const char *txt = dynamic_link_errors();
  if (txt)
    REG0 = make_string( txt );
  else
    REG0 = LITERAL(0);
  RETURN1();
})

(define-safe-glue (dl-resolve ent (sym <raw-string>))
{
  void *ptr;
  ptr = resolve_link_symbol(OBJ_TO_RAW_PTR(ent),sym);
  REG0 = ptr ? (RAW_PTR_TO_OBJ(ptr)) : FALSE_OBJ;
  RETURN1();
})

(define-safe-glue (dl-done ent)
{
  done_resolving(OBJ_TO_RAW_PTR(ent));
  RETURN0();
})

(define-safe-glue (dl-call cfn)
{
 void *r, *(*fn)( void );

 fn = ((void *(*)(void))OBJ_TO_RAW_PTR(cfn));
 r = fn();
 REG0 = r ? (RAW_PTR_TO_OBJ(r)) : FALSE_OBJ;
 RETURN1();
})

(define-safe-glue (dl-install m)
{
  install_module( OBJ_TO_RAW_PTR(m) );
  RETURN0();
})

(define-safe-glue (dl-open (path <raw-string>))
  literals: ((& dl-error))
{
  void *ent;
  ent = dynamic_link_file(path);
  if (!ent)
   {
     REG0 = raw_path;
     APPLY(1,TLREF(0));
   }
  else
   {
     REG0 = RAW_PTR_TO_OBJ(ent);
     RETURN1();
   }
})


(define-safe-glue (dl-c-unit (path <raw-string>) (unit_name <raw-string>))
  literals: ((& dl-error))
{
  if (!dynamic_link_c_unit( path, unit_name ))
    {
      REG0 = raw_path;
      APPLY(1,TLREF(0));
    }
  else
    {
      RETURN0();
    }
})
