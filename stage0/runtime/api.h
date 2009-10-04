/*-----------------------------------------------------------------*-C-*---
 * File:    handc/runtime/api.h
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.8
 * File mod date:    2003-08-20 13:34:47
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose:          Minimal public interface to RScheme runtime system
 *------------------------------------------------------------------------*/

/*
 *   Declaration of the minimal public interface to the
 *   RScheme runtime system
 *
 *   
 */

#ifndef _H_RSCHEME_API
#define _H_RSCHEME_API

#include <rscheme/obj.h>
#include <rscheme/linktype.h>
#include <rscheme/dynlink.h>

/*
 *   rscheme_std_main()
 *
 *   a convenience function to provide all the functionality
 *   of the standard shell.
 *   default_image is optional 
 *          (NULL => use standard default image (system.img)
 */

int rscheme_std_main( int argc, const char **argv, 
		      struct module_descr **modules,
		      const char *default_image );

/*
 *   init_scheme()
 *
 *   initialize the RScheme runtime
 *   if available, the path to the current executable should
 *   be passed in `executable_path'
 *
 *   `boot_image' is the path of the image to be loaded
 *   that contains the standard object definitions etc.
 *
 *   `verbose' indicates how much initialization greetings
 *   should be produced.  If true, the version line from
 *   the boot image is printed, and the garbage collector
 *   may print a greeting of it's own
 *
 *   `modules' is a pointer to a NULL-terminated array
 *   of the module descriptors that have been statically
 *   linked in.  the runtime system will keep this pointer
 *   around, so make sure it sticks around.
 *
 *   the value returned by this function is the `start'
 *   value (typically a procedure) stored in the boot
 *   image.
 *
 *   this function must be called before placing any demands
 *   on the runtime system.
 *
 *   this function initializes two RScheme globals:
 *       boot_image  rscheme_global[0]  the entry point to the loaded image
 *       boot_args   rscheme_global[1]  a list of the argc/argv's
 *
 *   the boot_args may occasionally be used by a dynamic linker module
 *   to locate the executable
 */

obj init_scheme( int argc, const char **argv,
		 const char *boot_image_path,
		 rs_bool verbose,
		 struct module_descr **modules );

/*
 *   call_scheme()
 *
 *   call a scheme procedure with some number of arguments
 *   (all of which should be of type `obj').  Returns the
 *   first return value from the function, or NOVALUE_OBJ
 *   if the function returned no arguments.
 *
 *   --NOTE--
 *   in the current version, we make no provision for handling
 *   calls to continuations that cross this function call, and
 *   you had better be sure to be at a safe point when you
 *   call this (because call_scheme() will come to, directly
 *   or indirectly, it's own safe points)
 */

obj call_scheme( obj closure, unsigned num_args, ... );

/*
 *    rs_install_dir
 *
 *    the location of the RScheme install root, which is where
 *    the system will look for modules, system images, and other
 *    resource by default.
 *
 *    this external is defined in the runtime system, and should
 *    be initialized by the client before calling rscheme_std_main()
 */

extern char *rs_install_dir;

#endif /* _H_RSCHEME_API */
