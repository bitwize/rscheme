/*-----------------------------------------------------------------*-C-*---
 * File:    handc/cfg/rdln/gnu/rdln_gnu.c
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.8
 * File mod date:    1997-11-29 23:10:45
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose:          interface to GNU readline
 *------------------------------------------------------------------------*/

/*
    read-line

    Scheme interface to the GNU readline library
*/

#include <stdio.h>
#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif
#include <readline/readline.h>
#include <readline/history.h>
#ifdef __cplusplus
}
#endif
#include <rscheme/platform.h>

#ifdef RETURN	/* this gets defined in readline/chardefs.h */
#undef RETURN
#endif

#include <rscheme/scheme.h>
#include <rscheme/vinsns.h>
#include <string.h>
#include <rscheme/rdln.h>
#include <unistd.h>   /* for isatty() */

static const char *scheme_generator( char *text, int state );

static obj the_completions;


/* **************************************************************** */
/*                                                                  */
/*                  Interface to Readline Completion                */
/*                                                                  */
/* **************************************************************** */

/* Tell the GNU Readline library how to complete.  We want to try to complete
   on command names if this is the first word in the line, or on filenames
   if not. */
   
static void init_readline( void )
{
  /* Allow conditional parsing of the ~/.inputrc file. */
    rl_readline_name = "rscheme";
    rl_basic_word_break_characters = " \t\n\"\\()[]{},";

  /* Tell the completer that we want a crack first. */
  
    rl_completion_entry_function = (Function *)scheme_generator;
}

static const char *scheme_generator( char *text, int state )
{
static obj current;
static int len;
obj item;
const char *name;

    if (state == 0)	/* restarting generation */
    {
	current = the_completions;
	len = strlen( text );
    }
    
    while (!EQ( current, NIL_OBJ ))
    {
        assert( PAIR_P(current) );
	item = pair_car( current );
	current = pair_cdr( current );

	if (STRING_P(item))
	{
	    name = string_text(item);
	}
	else
	{
	    assert( SYMBOL_P(item) );
	    name = symbol_text(item);
	}

	if (strncmp( name, text, len ) == 0)
	{
	char *name2;
	
	    name2 = (char *)malloc( strlen( name ) + 1 );
	    strcpy( name2, name );
	    return name2;
	}
    }
    return NULL;
}

rs_bool rdln_isa_tty( void )
{
  return isatty(0);
}

rs_bool rdln_enabled( void )
{
  return rdln_isa_tty() && !getenv("EMACS");
}

void rdln_add_history( obj str )
{
  add_history( string_text(str) );
}

obj read_console_line( obj completions, const char *prompt )
{
char *buffer = NULL;
static rs_bool init_yet = NO;
obj result;

    if (!init_yet)
	init_readline();
	
    the_completions = completions;

    buffer = readline( prompt );

    if (!buffer)
	return FALSE_OBJ;

    result = make_string(buffer);
    free(buffer);
    return result;
}
