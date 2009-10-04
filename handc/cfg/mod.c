/*-----------------------------------------------------------------*-C-*---
 * File:    handc/cfg/mod.c
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.4
 * File mod date:    1997-11-29 23:10:47
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose:          Config file to test behavior of C '%'
 *------------------------------------------------------------------------*/

#include <stdio.h>

/*
    The sign convention for the C operator `%' is
    implementation dependent, so REMDR insulates us from that, and
    promises to use the convention defined by Scheme,
    which apparently is that the remainder always has the sign
    of the dividend (the first argument)

    "Remainder and modulo differ on negative arguments ---
     the remainder is either zero or has the sign of the dividend,
     while the modulo always has the sign of the divisor:"
    
				    -- Revised^4 Scheme, p.22
*/


main()
{
  int a, b;

  a = 13;
  b = 4;

/*
#define REMDR(x,y) ((x)%(y))
#define MOD(x,y) (((x)%(y))+((x)<0?((y)<0?0:y):((y)<0?y:0)))

  printf( "%d %% %d = %d\n",  a,  b, MOD( a, b) );
  printf( "%d %% %d = %d\n", -a,  b, MOD(-a, b) ); 
  printf( "%d %% %d = %d\n",  a, -b, MOD( a,-b) ); 
  printf( "%d %% %d = %d\n", -a, -b, MOD(-a,-b) ); 
*/

  if ((a%b == 1) && (-a%b == -1) && (a%-b == 1) && (-a%-b == -1))
    {
      /* apparently, this system (RS/6000, or something similar),
	 has % take on the sign of the dividend (in dividend % divisor)
      */
      printf("#define REMDR(x,y) ((x)%%(y))\n");
      printf( 
	     "#define MOD(x,y) (((x)%%(y))+((x)<0?((y)<0?0:y):((y)<0?y:0)))\n"
	     );
    }
  else
    {
      fprintf( stderr, "unrecognized `a%%b' style -- defaulting\n" );
  
      printf( "#define UMOD(x,y) ((int)((unsigned)x %% (unsigned)y))\n");
      printf( "#define XFER_SIGN(x,y) ((y < 0) ? -x : x)\n");
      printf( "#define REMDR(x,y) XFER_SIGN(UMOD(x,y),x)\n");
      printf( "#define MOD(x,y) XFER_SIGN(UMOD(x,y),y)\n");
    }
}
