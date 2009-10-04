#ifndef INLINES_HH
#define INLINES_HH

#include <rtgc/gc.hh>
#include <rtgc/objmgr.hh>

/* This file ensures that the callee inline functions are included before
the callers. Don't change the order of the file inclusion. Any CI file must 
not included before this comment. Each CI file should not include other
CI file. Especially, the client has to be careful not to include any CI file
from gcclient.ci. */

#ifdef INLINES

#ifndef FOREIGN
#include <rtgc/gcclient1.ci>
#endif

#include <rtgc/scanroot.ci>

#include <rtgc/gen1.ci>
#include <rtgc/igps1.ci>
#include <rtgc/gc1.ci>
#include <rtgc/objmgr.ci>
#include <rtgc/colorset1.ci>
#include <rtgc/gc2.ci>

#ifndef FOREIGN
// gcserver.ci includes allocator.ci.
#include <rtgc/gcserver1.ci>
#include <rtgc/gcserver2.ci>
#include <rtgc/gcclient1.ci>
#include <rtgc/gcclient2.ci>
#endif // FOREIGN

#include <rtgc/pointers.ci>
#include <rtgc/igps2.ci>
#include <rtgc/gen2.ci>
#include <rtgc/colorset2.ci>

#endif  INLINES

#endif

