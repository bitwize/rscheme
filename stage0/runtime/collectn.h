/*-----------------------------------------------------------------*-C-*---
 * File:    handc/runtime/collectn.h
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.4
 * File mod date:    1997-11-29 23:10:49
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose:          COLLECTn() macros
 *------------------------------------------------------------------------*/

#define COLLECT0() STMT( obj t = COLLECT_TOP(); \
		 	 switch (arg_count_reg) \
			 { \
			    default: t = cons( REG9, t ); \
			    case 9:  t = cons( REG8, t ); \
			    case 8:  t = cons( REG7, t ); \
			    case 7:  t = cons( REG6, t ); \
			    case 6:  t = cons( REG5, t ); \
			    case 5:  t = cons( REG4, t ); \
			    case 4:  t = cons( REG3, t ); \
			    case 3:  t = cons( REG2, t ); \
			    case 2:  t = cons( REG1, t ); \
			    case 1:  t = cons( REG0, t ); \
			    case 0: ; } REG0 = t; )
#define COLLECT1() STMT( obj t = COLLECT_TOP(); \
		 	 switch (arg_count_reg) \
			 { \
			    default: t = cons( REG9, t ); \
			    case 9:  t = cons( REG8, t ); \
			    case 8:  t = cons( REG7, t ); \
			    case 7:  t = cons( REG6, t ); \
			    case 6:  t = cons( REG5, t ); \
			    case 5:  t = cons( REG4, t ); \
			    case 4:  t = cons( REG3, t ); \
			    case 3:  t = cons( REG2, t ); \
			    case 2:  t = cons( REG1, t ); \
			    case 1: ; \
			    case 0: ; } REG1 = t; )
#define COLLECT2() STMT( obj t = COLLECT_TOP(); \
		 	 switch (arg_count_reg) \
			 { \
			    default: t = cons( REG9, t ); \
			    case 9:  t = cons( REG8, t ); \
			    case 8:  t = cons( REG7, t ); \
			    case 7:  t = cons( REG6, t ); \
			    case 6:  t = cons( REG5, t ); \
			    case 5:  t = cons( REG4, t ); \
			    case 4:  t = cons( REG3, t ); \
			    case 3:  t = cons( REG2, t ); \
			    case 2:; case 1:;  \
			    case 0:; } REG2 = t; )
#define COLLECT3() STMT( obj t = COLLECT_TOP(); \
		 	 switch (arg_count_reg) \
			 { \
			    default: t = cons( REG9, t ); \
			    case 9:  t = cons( REG8, t ); \
			    case 8:  t = cons( REG7, t ); \
			    case 7:  t = cons( REG6, t ); \
			    case 6:  t = cons( REG5, t ); \
			    case 5:  t = cons( REG4, t ); \
			    case 4:  t = cons( REG3, t ); \
			    case 3:; case 2:; case 1:;  \
			    case 0:; } REG3 = t; )
#define COLLECT4() STMT( obj t = COLLECT_TOP(); \
		 	 switch (arg_count_reg) \
			 { \
			    default: t = cons( REG9, t ); \
			    case 9:  t = cons( REG8, t ); \
			    case 8:  t = cons( REG7, t ); \
			    case 7:  t = cons( REG6, t ); \
			    case 6:  t = cons( REG5, t ); \
			    case 5:  t = cons( REG4, t ); \
			    case 4:;  \
			    case 3:; case 2:; case 1:;  \
			    case 0:; } REG4 = t; )
#define COLLECT5() STMT( obj t = COLLECT_TOP(); \
		 	 switch (arg_count_reg) \
			 { \
			    default: t = cons( REG9, t ); \
			    case 9:  t = cons( REG8, t ); \
			    case 8:  t = cons( REG7, t ); \
			    case 7:  t = cons( REG6, t ); \
			    case 6:  t = cons( REG5, t ); \
			    case 5:; case 4:;  \
			    case 3:; case 2:; case 1:;  \
			    case 0:; } REG5 = t; )
#define COLLECT6() STMT( obj t = COLLECT_TOP(); \
		 	 switch (arg_count_reg) \
			 { \
			    default: t = cons( REG9, t ); \
			    case 9:  t = cons( REG8, t ); \
			    case 8:  t = cons( REG7, t ); \
			    case 7:  t = cons( REG6, t ); \
			    case 6:; case 5:; case 4:;  \
			    case 3:; case 2:; case 1:;  \
			    case 0:; } REG6 = t; )
#define COLLECT7() STMT( obj t = COLLECT_TOP(); \
		 	 switch (arg_count_reg) \
			 { \
			    default: t = cons( REG9, t ); \
			    case 9:  t = cons( REG8, t ); \
			    case 8:  t = cons( REG7, t ); \
			    case 7:; case 6:; case 5:; case 4:;  \
			    case 3:; case 2:; case 1:;  \
			    case 0:; } REG7 = t; )
#define COLLECT8() STMT( obj t = COLLECT_TOP(); \
		 	 switch (arg_count_reg) \
			 { \
			    default: t = cons( REG9, t ); \
			    case 9:  t = cons( REG8, t ); \
			    case 8:; case 7:; case 6:; case 5:; case 4:;  \
			    case 3:; case 2:; case 1:;  \
			    case 0:; } REG8 = t; )
#define COLLECT9() STMT( obj t = COLLECT_TOP(); \
		 	 if (arg_count_reg >= 10) \
				t = cons( REG9, t ); \
			 REG9 = t; )
