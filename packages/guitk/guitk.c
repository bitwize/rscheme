#include <tcl.h>
#include <tclExtend.h>
#include <tk.h>
#include "runtim.h"
#include "vinsns.h"
#include <ctype.h>

void *tcl_gateway( void )
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
	    scheme_error( "tcl_gateway: ~s invalid", 1, arg );
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
	REG1 = int2fx( rc );
	RETURN(2);
      }
    else
      {
	if (interp->result[0])
	  REG0 = make_string( interp->result );
	else
	  REG0 = TRUE_OBJ;
	RETURN1();
      }
}

static Tk_Window main_tk_win;
static obj evts;

int the_callback( ClientData data, Tcl_Interp *interp,
	 	  int argc, const char **argv )
{
obj item, info = NIL_OBJ;

    while (argc > 1)
      {
	const char *a = argv[--argc];
	if (isdigit(a[0]) || a[0] == '-')
	  item = int2fx( atoi(a) );
	else
	  item = make_string(a);
	info = cons( item, info );
      }
    evts = cons( info, evts );
    return TCL_OK;
}

#define FUNCTION "gui"

jump_addr gui_call( void )
{
Tcl_Interp *interp;
int rc = 0;

    COUNT_ARGS_AT_LEAST(1);
    if (EQ(REG0,FALSE_OBJ))
      {
	COUNT_ARGS(1);
	interp = Tcl_CreateInterp();
	REG0 = RAW_PTR_TO_OBJ( interp );
      }
    else if (arg_count_reg > 2 && EQ(REG1,int2fx(4)))
      {
	obj info;

	COUNT_ARGS(3);
	interp = (Tcl_Interp *)OBJ_TO_RAW_PTR(REG0);

	/* this hook creates a Scheme procedure 
	   for calling the given Tcl command
	   The arguments to the scheme procedure had
	   better be strings, fixnums, or symbols.
	   */
	info = bvec_alloc( sizeof(Tcl_CmdInfo), byte_vector_class );
	/*printf( "seeking info on `%s'\n", string_text(REG2) );*/
	if (!Tcl_GetCommandInfo( interp, 
				(char *)string_text(REG2),
				(Tcl_CmdInfo *)PTR_TO_DATAPTR(info) ))
	  {
	    REG0 = make_string( "command not found" );
	    REG1 = int2fx(1);
	    RETURN(1);
	  }

	REG0 = make2(closure_class,
		     make4(bindingenvt_class,
			   NIL_OBJ,
			   info,
			   RAW_PTR_TO_OBJ(interp),
			   REG2 ),
		     make2(template_class,
			   JUMP_ADDR_TO_OBJ(tcl_gateway),
			   ZERO));
	RETURN1();
      }
    else
      {
	COUNT_ARGS(2);
	interp = (Tcl_Interp *)OBJ_TO_RAW_PTR(REG0);

	if (EQ(REG1,int2fx(0)))
	  {
	    switch_hw_regs_back_to_os();
	    main_tk_win = Tk_CreateMainWindow( interp, NULL, "rs", "RScheme" );
	    if (!main_tk_win)
	    {
		switch_hw_regs_into_scheme();
		goto tcl_error;
	    }
	    printf( "main window = %#x\n", main_tk_win );
	    /*
	    Tk_GeometryRequest( main_tk_win, 200, 200 );
	    */
	    Tcl_SetVar(interp, "tcl_interactive","0", TCL_GLOBAL_ONLY);
	    Tcl_CreateCommand(interp,
			      "scheme-callback",
	    		      the_callback,
			      (ClientData)0, 
			      NULL);
	    switch_hw_regs_into_scheme();

	    if ((rc = Tcl_Init(interp)) == TCL_ERROR) {
		goto tcl_error;
	    }
	    if ((rc = Tk_Init(interp)) == TCL_ERROR) {
		goto tcl_error;
	    }
	}
	else if (EQ(REG1,int2fx(2)))
	{
	    Tk_MakeWindowExist( main_tk_win );
	    RETURN0();
	}
	else if (EQ(REG1,int2fx(1)))
	  {
	    evts = NIL_OBJ;
	    switch_hw_regs_back_to_os();
	    Tk_DoOneEvent(TK_ALL_EVENTS|TK_DONT_WAIT);
	    switch_hw_regs_into_scheme();
	    REG0 = evts;
	    RETURN(1);
	  }
	else if (EQ(REG1,int2fx(3)))
	{
	    evts = NIL_OBJ;
	    /* flush events */
	    switch_hw_regs_back_to_os();
	    while (Tk_DoOneEvent(TK_ALL_EVENTS|TK_DONT_WAIT));
	    switch_hw_regs_into_scheme();
	    REG0 = evts;
	    RETURN(1);
	}
	else
	  {
	    assert( STRING_P(REG1) );
	    rc = Tcl_Eval( interp, (char *)string_text(REG1) );
	  }
	REG0 = make_string( interp->result );
      }
    RETURN(1);
 tcl_error:
    REG0 = make_string( interp->result ); 
    REG1 = int2fx(rc);
    RETURN(2);
}
