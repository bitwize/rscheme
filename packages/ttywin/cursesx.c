#include <rscheme/scheme.h>
#include <rscheme/bcextend.h>

#ifdef PLATFORM_AIX
#include <cur01.h>   /* used libcur instead */
#else
#include <curses.h>
#endif

UINT_8 *bc_curses_extension( UINT_8 *pc, RS_bc_datum **arg_lim )
{
RS_bc_datum *arg_p = *arg_lim;

#define ARG(i,t) arg_p[i].t ## _val
#define str ARG(1,raw_str)
#define rc ARG(0,raw_int)
#define rch ARG(0,obj)
#define tch GET_IMMEDIATE_VALUE(ARG(1,obj))
#define y ARG(1,raw_int)
#define x ARG(2,raw_int)

#define screen (*((WINDOW **)&ARG(0,obj)))

#define NARGS(n) arg_p -= n; 

switch (*pc++)
{
case 0: NARGS(2); rc = waddch( screen, tch ); break;
case 1: NARGS(1); rch = MAKE_ASCII_CHAR(wgetch( screen )); break;
case 2: NARGS(2); rc = waddstr( screen, str ); break;
case 3: NARGS(2); rc = wgetstr( screen, str ); break;
case 4: NARGS(3); rc = wmove( screen, y, x ); break;
case 5: NARGS(1); rc = wclear( screen ); break;
case 6: NARGS(1); rc = werase( screen ); break;
case 7: NARGS(1); rc = wclrtobot( screen ); break;
case 8: NARGS(1); rc = wclrtoeol( screen ); break;
case 9: NARGS(1); rc = winsertln( screen ); break;
case 10: NARGS(1); rc = wdeleteln( screen ); break;
case 11: NARGS(1); rc = wrefresh( screen ); break;
case 12: NARGS(1); rch = MAKE_ASCII_CHAR(winch( screen )); break;
case 13: NARGS(2); rc = winsch( screen, tch ); break;
case 14: NARGS(1); rc = wdelch( screen ); break;
case 15: NARGS(1); rc = wstandout( screen ); break;
case 16: NARGS(1); rc = wstandend( screen ); break;

case 30: NARGS(0); rc = raw(); break;
case 31: NARGS(0); rc = noraw(); break;
#ifdef PLATFORM_AIX
case 32: 
case 33: 
  scheme_error( "cbreak() not available in AIX libcur -- use raw\n", 0 );
#else
case 32: NARGS(0); rc = cbreak(); break;
case 33: NARGS(0); rc = nocbreak(); break;
#endif
case 34: NARGS(0); rc = crmode(); break;
case 35: NARGS(0); rc = nocrmode(); break;
case 36: NARGS(0); rc = noecho(); break;
case 37: NARGS(0); rc = nl(); break;
case 38: NARGS(0); rc = nonl(); break;
case 39: NARGS(0); rc = savetty(); break;
case 40: NARGS(0); rc = 0; resetty(); break;


case 51: NARGS(4); screen = newwin( ARG(0,raw_int), ARG(1,raw_int),
				    ARG(2,raw_int), ARG(3,raw_int) ); break;
case 52: NARGS(5); screen = subwin( (WINDOW *)VAL(ARG(0,obj)), 
				    ARG(1,raw_int), ARG(2,raw_int),
				    ARG(3,raw_int), ARG(4,raw_int) ); break;
 default:
  scheme_error( "curses: illegal extension opcode '~d'", 1, int2fx(pc[-1]) );
}
 *arg_lim = arg_p + 1; /* always returns a value */
 return pc;
}
