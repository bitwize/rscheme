/**********************************************
THIS FILE WAS AUTOMATICALLY COPIED FROM THE
RSCHEME SOURCE TREE, AND THE ORIGINAL MAY CHANGE.
HENCE, DO NOT MODIFY THIS FILE BY HAND!
RScheme Build (v0.7.3.4-b7u, 2007-05-30)
**********************************************/

#line 1 "modules/regex/runmatch.c"
/*-----------------------------------------------------------------*-C-*---
 * File:    modules/regex/runmatch.c
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.9
 * File mod date:    2007-02-01 13:37:26
 * System build:     v0.7.3.4-b7u, 2007-05-30
 * Owned by module:  regex
 *
 * Purpose:          regex interpreter engine
 *------------------------------------------------------------------------*/

#include "regex_p.h"

struct RXCounter *rxmach_bound;
UINT_8 **rxmach_save_array;
UINT_8 *rxmach_machine;
UINT_8 *rxmach_start, *rxmach_limit;
UINT_8 *rxmach_on_eos;

void rxmach_internal_error( int code )
{
  scheme_error( "regex-interp: internal error code ~d", 1, int2fx(code) );
}

static UINT_8 *max_range( UINT_8 *str, UINT_8 *limit,
			  UINT_8 *sub_machine );


/*
 *  try to find a match
 *  return NULL if failed
 *  otherwise, return pointer to the place where the match ends
 */

#define FAILED      return NULL
#define EOS         return rxmach_on_eos

#define save_array rxmach_save_array
#define machine rxmach_machine
#define limit rxmach_limit
#define bound rxmach_bound

UINT_8 *run_match( UINT_8 *str, UINT_32 pc )
{
  UINT_8 *min;

  while (1)
    {
      switch (machine[pc++])
	{
	case RXM_MATCH1:
	  if (str < limit)
	    {
	      if (*str++ != machine[pc++])
		FAILED;
	    }
	  else
	    EOS;
	  break;

	case RXM_MATCH_ANY:
	  if (str == limit)
	    EOS;
	  str++;
	  break;

	case RXM_STR_END:
	  if (str != limit)
	    FAILED;
	  break;

	case RXM_STR_START:
	  if (str != rxmach_start)
	    FAILED;
	  break;

	case RXM_MATCH_SET:
	  {
	    UINT_8 ch;

	    if (str == limit)
	      EOS;
	    
	    ch = *str++;
	    if (!(machine[pc+ch/8] & (0x80 >> (ch%8))))
	      FAILED;
	    pc += 32;
	  }
	  break;
	  
	case RXM_MATCHN:
	  {
	    UINT_8 i, n = machine[pc++];

	    while (n > 0)
	      {
		if (str >= limit)
		  EOS;
		else if (*str != machine[pc++])
		  FAILED;
		str++;
		n--;
	      }
	  }
	  break;

	case RXM_ACCEPT:
	  return str;

	case RXM_BRANCH:
	  {
	    UINT_32 subr;
	    UINT_8 *result;

	    subr = (machine[pc] << 8) + machine[pc+1];
	    pc += 2;

	    result = run_match( str, subr );
	    if (result)
	      return result;
	   
	    break;
	  }

        case RXM_INC:
          {
            UINT_8 reg = machine[pc++];
            bound[reg].count++;
            /*
            printf( "-- [%02u] inc r%u [%u,%u] now %u\n",
                    pc-2, reg, bound[reg].min, bound[reg].max,
                    bound[reg].count );
            */
            break;
          }

        case RXM_BOUNDLOOP:
          {
            UINT_32 subr, pcsave = pc-1;
            UINT_8 *result;
            UINT_8 reg;
            UINT_16 save;

            reg = machine[pc++];
 	    subr = (machine[pc] << 8) + machine[pc+1];
	    pc += 2;

            save = bound[reg].count;
            /*
            printf( "-- [%02u] <%p> boundloop r%u (count %u, [%u,%u])\n", 
                    pcsave, &subr, reg, save, bound[reg].min, bound[reg].max );
            */
            if (save < bound[reg].max) {
              result = run_match( str, subr );
              if (result) {
                /*
                  printf( "-- [%02u] <%p> boundloop run_match OK\n",
                        pcsave, &subr );
                */
                return result;
              }
              /*printf( "-- [%02u] <%p> boundloop run_match failed (fix n=%u)\n",
                      pcsave, &subr, save );
              */
              bound[reg].count = save;
              if (save < bound[reg].min) {
                /*
                  printf( "-- [%02u] <%p> boundloop total failure w/%u\n",
                        pcsave, &subr, save );
                */
                FAILED;
              }
              /*
              printf( "-- [%02u] <%p> boundloop continue w/%u (no match)\n",
                      pcsave, &subr, save );
              */
            } else {
              /*
                printf( "-- [%02u] <%p> boundloop continue w/%u (hit max)\n",
                      pcsave, &subr, save );
              */
            }
            break;
          }
            
        case RXM_SETBOUND:
          {
            UINT_32 pcsave = pc-1;
            UINT_8 reg = machine[pc++];

            bound[reg].count = 0;
            bound[reg].min = (machine[pc] << 8) + machine[pc+1];
            bound[reg].max = (machine[pc+2] << 8) + machine[pc+3];
            pc += 4;
            /*
            printf( "-- [%02u] setbound r%u [%u,%u]\n",
                    pcsave, reg, bound[reg].min, bound[reg].max );
            */
            break;
          }

	case RXM_SAVE_PLACE:
	  {
	    UINT_8 *p, reg = machine[pc++];

	    p = run_match( str, pc );
	    if (p)
	      {
		save_array[reg] = str;
		return p;
	      }
	    else
	      FAILED;
	  }

	case RXM_MATCH_PLUS:
	  min = str + 1;
	  goto match_kleene;

	case RXM_MATCH_STAR:
	  min = str;
	match_kleene:
	  {
	    UINT_8 *range = max_range( str, 
				       limit,
				       machine 
				       + (machine[pc] << 8)
				       + machine[pc+1] );
	    UINT_8 *result;
	    pc += 2;

	    /* If we can eat everything, then this is
	     * a valid prefix
	     * (although the remainder of the pattern
	     * might mean we are not a valid match)
	     */
	    if ((range == limit) && rxmach_on_eos)
	      EOS;

	    while (range >= min)
	      {
		result = run_match( range, pc );
		if (result)
		  return result;
		range--;
	      }
	    FAILED;
	  }

	case RXM_REJECT:
	  FAILED;

	case RXM_JUMP:
	  pc = (machine[pc] << 8) + machine[pc+1];
	  break;

	default:
	  rxmach_internal_error( RXERR_INT_BAD_OPCODE );
	}
    }
} 


static UINT_8 *max_range( UINT_8 *str, UINT_8 *limit,
			  UINT_8 *sub_machine )
{
  switch (sub_machine[0])
    {
    case RXM_MATCH_SET:
      {
	UINT_8 ch, *vec = sub_machine + 1;

	while (str < limit)
	  {
	    ch = *str;
	    if (!(vec[ch/8] & (0x80 >> (ch%8))))
	      return str;
	    str++;
	  }
	return limit;
      }
    case RXM_MATCH_ANY:
      return limit;
    case RXM_MATCH1:
      {
	UINT_8 ch = sub_machine[1];
	while (str < limit)
	  {
	    if (*str != ch)
	      return str;
	    str++;
	  }
	return limit;
      }
    default:
      rxmach_internal_error(RXERR_INT_BAD_KLEENE);
      return NULL; /* quiet compiler */
    }
}
