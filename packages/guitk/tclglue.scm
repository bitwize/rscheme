
(define-class <tcl-interp> (<object>)
  raw-tcl-interp)

(define-safe-glue (make-tcl-interp)
 literals: ((& <tcl-interp>))
{
  REG0 = make1( TLREF(0),
	        RAW_PTR_TO_OBJ( Tcl_CreateInterp() ) );
  RETURN1();
})

(define-safe-glue (tcl-eval (interp <tcl-interp>) (expr <raw-string>))
{
  int rc;
  Tcl_Interp *i = OBJ_TO_RAW_PTR( gvec_ref( interp, SLOT(0) ) );

  rc = Tcl_Eval( i, expr );
  REG0 = make_string( i->result );
  RETURN1();
})

(define-safe-glue (make-tcl-gateway (interp <tcl-interp>)
				    (command <raw-string>))
  literals: ((& tcl-gateway))
{
  Tcl_Interp *i = OBJ_TO_RAW_PTR( gvec_ref( interp,SLOT(0) ) );
  obj cmd_info = bvec_alloc( sizeof(Tcl_CmdInfo), byte_vector_class );

  if (!Tcl_GetCommandInfo( i,
			   command, 
			   (Tcl_CmdInfo *)PTR_TO_DATAPTR(cmd_info) ))
    {
      scheme_error( "make-tcl-gateway: command ~s not defined",
		    1, raw_command );
    }
  REG0 = make2( closure_class,
	       make4( binding_envt_class, 
		     NIL_OBJ,
		     cmd_info,
		     RAW_PTR_TO_OBJ(i),
		     raw_command),
	       TLREF(0) );
  RETURN1();
})

(define-safe-glue (tcl-gateway #rest)
  :template
  literals: ((& error))
{
char **argp, *(args[102]);
Tcl_CmdInfo *info;
Tcl_Interp *interp;
char temp[1000], *d;
int i, rc;

    assert( arg_count_reg <= 100 );
    info = (Tcl_CmdInfo *)PTR_TO_DATAPTR(LEXREF0(0));
    interp = (Tcl_Interp *)OBJ_TO_RAW_PTR(LEXREF0(1));

    d = temp;
    argp = args;
    *argp++ = (char *)string_text( LEXREF0(2) );
    for (i=0; i<arg_count_reg; i++)
      {
	obj arg;

	arg = reg_ref(i);
	if (STRING_P(arg))
	  {
	    *argp++ = (char *)string_text(arg);
	  }
	else if (OBJ_ISA_FIXNUM(arg))
	  {
	    *argp++ = d;
	    sprintf( d, "%d", fx2int(arg) );
	    d += strlen(d) + 1;
	  }
	else if (SYMBOL_P(arg))
	  {
	    *argp++ = (char *)symbol_text(arg);
	  }
	else
	  {
	    scheme_error( "tcl-gateway: ~s invalid as arg", 1, arg );
	  }
      }
    *argp++ = NULL;
    Tcl_ResetResult( interp );
    rc = info->proc( info->clientData,
		     interp,
		     arg_count_reg + 1,
		     args );
    if (rc)
      {
	REG0 = make_string( interp->result );
	Tcl_ResetResult( interp );
	APPLY(1,TLREF(0));
      }
    else
      {
	if (interp->result[0])
	  {
	    REG0 = make_string( interp->result );
	    Tcl_ResetResult( interp );
	  }
	else
	  REG0 = FALSE_OBJ;
	RETURN1();
      }
})

(define-safe-glue (tk-create-main-window (the_interp <tcl-interp>)
					 screen_name
					 app_name
					 class_name)
{
  Tcl_Interp *interp = (Tcl_Interp *)OBJ_TO_RAW_PTR(gvec_ref(the_interp,
							     SLOT(0)));
  Tk_Window w;
  int rc;
  extern int the_delayed_callback( ClientData data, Tcl_Interp *interp,
	 		           int argc, const char **argv );

  w = Tk_CreateMainWindow( interp, 
			  STRING_P(screen_name)
			  ? string_text(screen_name)
			  : NULL,
			  STRING_P(app_name)
			  ? string_text(app_name)
			  : NULL,
			  STRING_P(class_name)
			  ? string_text(class_name)
			  : NULL );

  if (!w)
    goto tcl_error;
  
  Tcl_SetVar(interp, "tcl_interactive","0", TCL_GLOBAL_ONLY);
  Tcl_CreateCommand(interp,
		    "delayed-callback",
		    the_delayed_callback,
		    (ClientData)0,
		    NULL);

  if ((rc = Tcl_Init(interp)) == TCL_ERROR) {
    goto tcl_error;
  }
  if ((rc = Tk_Init(interp)) == TCL_ERROR) {
    goto tcl_error;
  }
  REG0 = RAW_PTR_TO_OBJ(w);
  RETURN1();
 tcl_error:
      REG0 = make_string( interp->result );
      Tcl_ResetResult( interp );
      scheme_error( "Tk_CreateMainWindow() failed: ~a", 1, REG0 );
})

(define-safe-glue (tk-make-window-exist main_tk_win)
{
  Tk_MakeWindowExist( OBJ_TO_RAW_PTR(main_tk_win) );
  RETURN0();
})

(define-safe-glue (tk-do-one-event)
{
extern obj queued_callbacks;

  OUT_CALL( Tk_DoOneEvent(TK_ALL_EVENTS|TK_DONT_WAIT); );
  REG0 = queued_callbacks;
  RETURN1();
})

