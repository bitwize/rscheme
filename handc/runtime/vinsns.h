/*-----------------------------------------------------------------*-C-*---
 * File:    handc/runtime/vinsns.h
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.25
 * File mod date:    2005-09-16 09:06:42
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose:          virtual machine instruction set
 *------------------------------------------------------------------------*/

#ifndef _H_VINSNS
#define _H_VINSNS

#include <rscheme/api.h>
#include <rscheme/regs.h>
#include <rscheme/smemory.h>
#include <rscheme/profile.h>

#ifdef GNU_VINSNS
#include <rscheme/vinsns_g.h>
#else

/*  Structuring things  */

#define PROLOGUE(fn_name)
#define MONOTONE(name)		static jump_addr name( void )
#define EPILOGUE(fn_name)

#define BEGIN_FWD(fn_name)
#define FWD_MONOTONE(name)	static jump_addr name( void );
#define END_FWD(fn_name)

#define JUMP_TABLE2(fn_name)   fn_name ## _tab
#define JUMP_TABLE(fn_name)   JUMP_TABLE2(fn_name)
#define BEGIN_BACK(fn_name)   static jump_addr JUMP_TABLE(fn_name) [] = {
#define BACK_MONOTONE(name)   (jump_addr)name,
#define END_BACK(fn_name)     (jump_addr)NULL };


/*  The virtual machine instructions  */


#define TAILCALL(nextfn) return (jump_addr)nextfn

#define BJUMP(num,label)  STMT(if (rsprof_active) rsprof_mt_bjumps(); \
			       arg_count_reg = num; \
			       return (jump_addr)label;)

#if defined(PROFILE_MONOTONES)
#define JUMP(num,label)	  BJUMP(num,label)
#else
#if PROFILING_HOOKS
#define JUMP(num,label)   STMT(if (rsprof_active) rsprof_mt_jumps(); \
			       arg_count_reg = num; \
			       return (jump_addr)label;)
#else
#define JUMP(num,label)   return label()
#endif
#endif

#define APPLY(num,expr)	       STMT(arg_count_reg = num; \
				    return apply(CHECK_FUNCTION(expr));)
#define APPLYF(num,expr)       STMT(arg_count_reg = num; \
				    return apply(expr);)
#define APPLYG(num,expr)       STMT(arg_count_reg = num; \
				    return applyg(expr);)
#define APPLY_TMPL(num,expr,t) STMT(arg_count_reg = num; \
				    return apply_tmpl(CHECK_FUNCTION(expr),t);)

#define RETURN(num) STMT(arg_count_reg = num; TAILCALL(half_restore());)

#define LABEL_TO_JUMP_ADDR(label) ((jump_addr)label)

#endif /* GNU_VINSNS */

#define RETURN0() STMT(REG0=FALSE_OBJ; RETURN(0);)
#define RETURN1() RETURN(1);

void quasi_interp_spin( jump_addr first_m );

CIH_DECL jump_addr apply( obj closure );
CIH_DECL jump_addr apply_tmpl( obj closure, obj tmplate );
CIH_DECL jump_addr half_restore( void );
jump_addr rs_gf_dispatch( obj gf );
jump_addr applyg( obj gf );
obj rs_gf_find_method( obj gf, obj rcvr );

/************************ Miscellaneous ************************/

#define LITERAL(k) gvec_read( literals_reg, SLOT((k)+3) )
#define CLOSURE(k) make_closure(envt_reg,LITERAL(k))
#define TLREF(k) tlv_value( LITERAL(k) )
#define TLREFB(k) gvec_ref( LITERAL(k), SLOT(1) ) /* ref a bound var */
#define TLSET(k,x) tlv_set_value( LITERAL(k), (x) )

_rs_volatile void failed_type_check( obj place, obj var, obj val, obj expect );
_rs_volatile void apply_error( obj a_non_function );
_rs_volatile void done_w_call_scheme( void );

/************************ Binding Environments ************************/

obj nth_enclosing_envt( unsigned n );

CIH_DECL obj enclosing_envt( obj envt );

/* one of USE_FUNCTION_ENVT or USE_EMPTY_ENVT *MUST* be used
 * if the function expects to have an environment in it's <function>
 * object, or is going to bind arguments onto the heap.
 *
 * THIS_FUNCTION() may be used only *BEFORE* the use of one of those
 * vinsns in the entry monotone
 */

#define USE_FUNCTION_ENVT()  envt_reg = gvec_ref(envt_reg,SLOT(1))
#define USE_EMPTY_ENVT()     envt_reg = NIL_OBJ

#define THIS_FUNCTION()     envt_reg


#define LEXREF(frame,i) (rs_profile2("lexref",frame,i),\
			 gvec_read(nth_enclosing_envt(frame),SLOT((i)+1)))

#ifdef RS_PROFILE
#define LEXREF0(i) LEXREF(0,i)
#define LEXREF1(i) LEXREF(1,i)
#define LEXREF2(i) LEXREF(2,i)
#define LEXREF3(i) LEXREF(3,i)
#else
#define LEXREF0(i) gvec_read(envt_reg,SLOT((i)+1))
#define LEXREF1(i) gvec_read(enclosing_envt(envt_reg),SLOT((i)+1))
#define LEXREF2(i) gvec_read(enclosing_envt(\
				enclosing_envt(envt_reg)),SLOT((i)+1))
#define LEXREF3(i) gvec_read(enclosing_envt(\
				enclosing_envt(\
				    enclosing_envt(envt_reg))),SLOT((i)+1))
#endif

#define LEXREF4(i) LEXREF(4,i)
#define LEXREF5(i) LEXREF(5,i)
#define LEXREF6(i) LEXREF(6,i)
#define LEXREF7(i) LEXREF(7,i)
#define LEXREF8(i) LEXREF(8,i)
#define LEXREF9(i) LEXREF(9,i)

#define LEXSET(frame,i,v) (rs_profile2("lexset",frame,i),\
			   gvec_write(nth_enclosing_envt(frame),SLOT((i)+1),v))

#ifdef RS_PROFILE
#define LEXSET0(i,v) LEXSET(0,i,v)
#define LEXSET1(i,v) LEXSET(1,i,v)
#define LEXSET2(i,v) LEXSET(2,i,v)
#define LEXSET3(i,v) LEXSET(3,i,v)
#else
#define LEXSET0(i,v) gvec_write(envt_reg,SLOT((i)+1),v)
#define LEXSET1(i,v) gvec_write(enclosing_envt(envt_reg),SLOT((i)+1),v)
#define LEXSET2(i,v) gvec_write(enclosing_envt(\
				    enclosing_envt(envt_reg)),\
				SLOT((i)+1),\
				v)
#define LEXSET3(i,v) gvec_write(enclosing_envt(\
				    enclosing_envt(\
					enclosing_envt(envt_reg))),\
				SLOT((i)+1),\
				v)
#endif
#define LEXSET4(i,v) LEXSET(4,i,v)
#define LEXSET5(i,v) LEXSET(5,i,v)
#define LEXSET6(i,v) LEXSET(6,i,v)
#define LEXSET7(i,v) LEXSET(7,i,v)
#define LEXSET8(i,v) LEXSET(8,i,v)
#define LEXSET9(i,v) LEXSET(9,i,v)

#define POPENVT()	envt_reg = enclosing_envt( envt_reg )

/*
    Some of the code in ctcprims (ie, bind_regs())
    depends on this definition of BEGIN_BIND_
    because it fakes the use of BEGIN_BIND_
*/

#define BEGIN_BIND_(c)	{ obj new_envt = c; \
			  gvec_write_init( new_envt, SLOT(0), envt_reg );
#define BEGIN_BIND(num)	BEGIN_BIND_(alloc(SLOT((num)+1),binding_envt_class))
#define BEGIN_BIND0()	BEGIN_BIND_(alloc1(binding_envt_class))
#define BEGIN_BIND1()	BEGIN_BIND_(alloc2(binding_envt_class))
#define BEGIN_BIND2()	BEGIN_BIND_(alloc3(binding_envt_class))
#define BEGIN_BIND3()	BEGIN_BIND_(alloc4(binding_envt_class))
#define BEGIN_BIND4()	BEGIN_BIND_(alloc5(binding_envt_class))
#define BEGIN_BIND5()	BEGIN_BIND_(alloc6(binding_envt_class))
#define BEGIN_BIND6()	BEGIN_BIND_(alloc7(binding_envt_class))
#define BEGIN_BIND7()	BEGIN_BIND_(alloc8(binding_envt_class))
#define BEGIN_BIND8()	BEGIN_BIND_(alloc9(binding_envt_class))
#define BEGIN_BIND9()	BEGIN_BIND(9)
#define BIND_ARG(i,e)	gvec_write_init( new_envt, SLOT((i)+1), e )
#define END_BIND	envt_reg = new_envt; }

#define BIND1(e1)	envt_reg = make2( binding_envt_class, envt_reg, \
				   e1 )
#define BIND2(e1,e2)	envt_reg = make3( binding_envt_class, envt_reg, \
				   e1, e2 )
#define BIND3(e1,e2,e3)	envt_reg = make4( binding_envt_class, envt_reg, \
				   e1, e2, e3 )

/************************ Argument Checking ************************/

_rs_volatile void wrong_num_args( const char *fn, unsigned num_required );
_rs_volatile void wrong_num_args_range( const char *fn, 
				    unsigned min_rqd,
				    unsigned max_accepted );
_rs_volatile void too_few_args( const char *fn, unsigned min_required );

#define COUNT_ARGS(num) STMT( if (arg_count_reg != (num)) \
				wrong_num_args( FUNCTION, num ); )
				
#define COUNT_ARGS_AT_LEAST(num) STMT( if (arg_count_reg < (num)) \
				too_few_args( FUNCTION, num ); )

_rs_volatile void type_check_failed( const char *fn );

#define assert_type(expr) STMT( if (!(expr)) type_check_failed(FUNCTION); )

void pad_with_false( unsigned limit_reg );
unsigned expand_last( void );

obj collect_top( unsigned first_reg );
void collectn( unsigned first_reg ); /* reg(n) = collect_top(n); */

#define COLLECT_TOP() (arg_count_reg > 10)?collect_top(10):NIL_OBJ
#include <rscheme/collectn.h>

#define COLLECT(num) collectn(num)
#define PAD_WITH_FALSE0()  /* nothing */
#define PAD_WITH_FALSE1()  STMT( if (arg_count_reg == 0) REG0 = FALSE_OBJ; )
#define PAD_WITH_FALSE2()  STMT( switch (arg_count_reg) { \
				case 0: REG0 = FALSE_OBJ; \
				case 1: REG1 = FALSE_OBJ; })
#define PAD_WITH_FALSE3()  STMT( switch (arg_count_reg) { \
				case 0: REG0 = FALSE_OBJ; \
				case 1: REG1 = FALSE_OBJ; \
				case 2: REG2 = FALSE_OBJ; })
#define PAD_WITH_FALSE4()  STMT( switch (arg_count_reg) { \
				case 0: REG0 = FALSE_OBJ; \
				case 1: REG1 = FALSE_OBJ; \
				case 2: REG2 = FALSE_OBJ; \
				case 3: REG3 = FALSE_OBJ; })

#define PAD_WITH_FALSE(num) STMT(if (arg_count_reg < num) pad_with_false(num);)

/************************ Continuations ************************/

#define CONT_FIXED  (4)

void save_cont( unsigned num, jump_addr label );
void restore_cont( unsigned num );

#ifdef STACK_CACHE
#include <rscheme/stakcach.h>
#else
#include <rscheme/stack.h>
#endif

#define PUSH_PARTCONT(label,space) PUSH_PARTCONT_ADDR(\
					LABEL_TO_JUMP_ADDR(label),space)

#define SAVE_CONT0(label) push_cont(LABEL_TO_JUMP_ADDR(label),0)
#define SAVE_CONT1(label) STMT( PUSH_PARTCONT(label,1); \
				SET_PARTCONT_REG(0,REG0); )
#define SAVE_CONT2(label) STMT( PUSH_PARTCONT(label,2); \
				SET_PARTCONT_REG(0,REG0); \
				SET_PARTCONT_REG(1,REG1); )
#define SAVE_CONT3(label) STMT( PUSH_PARTCONT(label,3); \
				SET_PARTCONT_REG(0,REG0); \
				SET_PARTCONT_REG(1,REG1); \
				SET_PARTCONT_REG(2,REG2); )
#define SAVE_CONT4(label) STMT( PUSH_PARTCONT(label,4); \
				SET_PARTCONT_REG(0,REG0); \
				SET_PARTCONT_REG(1,REG1); \
				SET_PARTCONT_REG(2,REG2); \
				SET_PARTCONT_REG(3,REG3); )
#define SAVE_CONT5(label) save_cont(5,LABEL_TO_JUMP_ADDR(label))
#define SAVE_CONT6(label) save_cont(6,LABEL_TO_JUMP_ADDR(label))
#define SAVE_CONT7(label) save_cont(7,LABEL_TO_JUMP_ADDR(label))
#define SAVE_CONT8(label) save_cont(8,LABEL_TO_JUMP_ADDR(label))
#define SAVE_CONT9(label) save_cont(9,LABEL_TO_JUMP_ADDR(label))
#define SAVE_CONT(num,label) save_cont(num,LABEL_TO_JUMP_ADDR(label))

#define RESTORE_CONT_REG() continuation_reg = PARTCONT_REF(3)
#define PARTCONT_REG(i) PARTCONT_REF((i)+CONT_FIXED)

#define RESTORE_CONT0() RESTORE_CONT_REG()
#define RESTORE_CONT1() STMT( REG0 = PARTCONT_REG(0); RESTORE_CONT_REG(); )
#define RESTORE_CONT2() STMT( REG0 = PARTCONT_REG(0); \
			      REG1 = PARTCONT_REG(1); \
			      RESTORE_CONT_REG(); )
#define RESTORE_CONT3() STMT( REG0 = PARTCONT_REG(0); \
			      REG1 = PARTCONT_REG(1); \
			      REG2 = PARTCONT_REG(2); \
			      RESTORE_CONT_REG(); )
#define RESTORE_CONT4() STMT( REG0 = PARTCONT_REG(0); \
			      REG1 = PARTCONT_REG(1); \
			      REG2 = PARTCONT_REG(2); \
			      REG3 = PARTCONT_REG(3); \
			      RESTORE_CONT_REG(); )
#define RESTORE_CONT5() restore_cont(5)
#define RESTORE_CONT6() restore_cont(6)
#define RESTORE_CONT7() restore_cont(7)
#define RESTORE_CONT8() restore_cont(8)
#define RESTORE_CONT9() restore_cont(9)
#define RESTORE_CONT(num)  restore_cont(num)

/***************************************************************/

#ifdef INLINES
#include <rscheme/vinsns.ci>
#endif /* INLINES */

/********* remapping C operators to function-looking things ****/

#define FLT_ADD(x,y) ((x)+(y))
#define FLT_SUB(x,y) ((x)-(y))
#define FLT_MUL(x,y) ((x)*(y))
#define FLT_DIV(x,y) ((x)/(y))
#define FLT_NEG(x)   (-(x))

#define FLT_GT(x,y) ((x)>(y))
#define FLT_GE(x,y) ((x)>=(y))
#define FLT_LT(x,y) ((x)<(y))
#define FLT_LE(x,y) ((x)<=(y))
#define FLT_EQ(x,y) ((x)==(y))

#define FLT_TRUNC(x) ((int)(x))
#define FX_FLOAT(x)  ((IEEE_64)(x))

/********* similar for register access ***/

#define GET_DYNAMIC_STATE_REG()  dynamic_state_reg
#define SET_DYNAMIC_STATE_REG(v)  (dynamic_state_reg = (v))

#define GET_THREAD_STATE_REG()  thread_state_reg
#define SET_THREAD_STATE_REG(v)  (thread_state_reg = (v))

#endif /* _H_VINSNS */
