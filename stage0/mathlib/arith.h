/**********************************************
THIS FILE WAS AUTOMATICALLY COPIED FROM THE
RSCHEME SOURCE TREE, AND THE ORIGINAL MAY CHANGE.
HENCE, DO NOT MODIFY THIS FILE BY HAND!
RScheme Build (v0.7.3.4-b7u, 2007-05-30)
**********************************************/

#line 1 "modules/mathlib/arith.h"
/*-----------------------------------------------------------------*-C-*---
 * File:    modules/mathlib/arith.h
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.5
 * File mod date:    1997-11-29 23:10:51
 * System build:     v0.7.3.4-b7u, 2007-05-30
 * Owned by module:  mathlib
 *
 * Purpose:          macros to interpret number forms for arithmetic operations
 *------------------------------------------------------------------------*/

#define INVOKE_GENERIC() APPLY(arg_count_reg,TLREF(0))

#define ARITHMETIC_ORDER_PREDICATE(op) \
    int flag; \
	if (OBJ_ISA_FIXNUM(REG0)) { \
	    if (OBJ_ISA_FIXNUM(REG1)) \
		flag = IVAL(REG0) op IVAL(REG1); \
	    else if (LONGFLOAT_P(REG1)) \
		flag = (IEEE_64)fx2int(REG0) op extract_float(REG1); \
	    else goto invoke_generic_fn; } \
	else if (LONGFLOAT_P(REG0)) { \
	    if (OBJ_ISA_FIXNUM(REG1)) \
		flag = extract_float(REG0) op (IEEE_64)fx2int(REG1); \
	    else if (LONGFLOAT_P(REG1)) \
		flag = extract_float(REG0) op extract_float(REG1); \
	    else goto invoke_generic_fn; } \
	else goto invoke_generic_fn; \
	REG0 = flag ? TRUE_OBJ : FALSE_OBJ; RETURN(1); \
	invoke_generic_fn: INVOKE_GENERIC();

#define REAL_OPERATOR(op) \
    COUNT_ARGS(1); \
    REG0 = make_float( op( get_float( REG0, FUNCTION, "z" ) ) ); \
    RETURN(1);

