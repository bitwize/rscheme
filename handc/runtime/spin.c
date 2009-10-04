/*-----------------------------------------------------------------*-C-*---
 * File:    handc/runtime/spin.c
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.10
 * File mod date:    1998-01-06 09:57:19
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose:          Top-level quasi-interpretive loop
 *------------------------------------------------------------------------*/

#include "runtime.h"
#include "intrs.h"
#include "vinsns.h"
#include "osglue.h"

#ifdef TIMER_IS_MONOTONE_COUNTER
UINT_32 system_timeout = 0;
#endif             

void quasi_interp_spin( jump_addr f )
{
  while (1)
    {
      /* eventually, this should be rolled
	 into the prologue of each monotone
	 for now, we probably don't allocate more than 1000 or so
	 words in any given monotone */

      safe_point( SLOT(3000) );

#ifdef SIGUSR_HOOKS
      if (sigusr1_flag)
	{
	  sigusr1_flag = 0;
	  run_sigusr_hook(1);
	}
      if (sigusr2_flag)
	{
	  sigusr2_flag = 0;
	  run_sigusr_hook(2);
	}
#endif
      if (rssig_ready)
	{
	  if (rsprof_active)
	    rsprof_mt_intr();
	  f = dispatch_interrupt(f);
	}
      
#ifdef TIMEPOINT
      timepoint( VAL(literals_reg) + 0x80000000 );
#endif
      if (rsprof_active)
	rsprof_mt_start(f);
      f = (jump_addr) f();
      if (rsprof_active)
	rsprof_mt_done();

#ifdef TIMEPOINT
      timepoint( VAL(literals_reg) + 0x80000000 );
#endif
      if (rsprof_active)
	rsprof_mt_start(f);
      f = (jump_addr) f();
      if (rsprof_active)
	rsprof_mt_done();

#ifdef TIMEPOINT
      timepoint( VAL(literals_reg) + 0x80000000 );
#endif
      if (rsprof_active)
	rsprof_mt_start(f);
      f = (jump_addr) f();
      if (rsprof_active)
	rsprof_mt_done();
      
#ifdef TIMER_IS_MONOTONE_COUNTER
      if (system_timeout && (--system_timeout == 0))
	{
	  struct RSSIG_info t;
	  t.signum = RSSIG_TIMEOUT;
	  os_enqueue_sig( &t );
	}
#endif /* TIMER_IS_MONOTONE_COUNTER */
    }
}
