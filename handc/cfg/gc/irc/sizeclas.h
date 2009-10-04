/*-----------------------------------------------------------------*-C-*---
 * File:    handc/cfg/gc/irc/sizeclas.h
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.4
 * File mod date:    1997-11-29 23:10:46
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 *------------------------------------------------------------------------*/

#ifndef _H_SIZECLAS
#define _H_SIZECLAS

/*  there are three sets of size classes
    fine granular size classes:
    	128 of these, at double-word allocation resolution
	0
    coarse granular size classes
    	15 of these, at 64 word (256-byte) allocation resolution
		starting at 256 words
    large object size class
    	1 of these, using the underyling library's storage mgmt
	(ie, malloc() and free())
*/

/*
(define (sc n b)
  (lambda (i)
    (list (+ b (* (- i 1) n) 1)
          (+ b (* i n)))))

(define fine (sc 8 0))
(define coarse (sc 256 (* 8 128)))

*/

#define NUM_FINE_SIZE_CLASSES		(128)
#define FINE_SIZE_CLASS_RESOLUTION	(8)	/* bytes */
#define FINE_SIZE_CLASS_NUM(bytes)	\
		((bytes+(FINE_SIZE_CLASS_RESOLUTION-1))\
		 /FINE_SIZE_CLASS_RESOLUTION)

#define FINE_SIZE_LIMIT			(FINE_SIZE_CLASS_RESOLUTION  \
					 *(NUM_FINE_SIZE_CLASSES-1))

#define NUM_COARSE_SIZE_CLASSES		(15)
#define COARSE_SIZE_CLASS_RESOLUTION	(64*4)	/* bytes */
#define COARSE_SIZE_CLASS_NUM(bytes)	(NUM_FINE_SIZE_CLASSES + \
					    (bytes-(FINE_SIZE_LIMIT+1)) \
					    /COARSE_SIZE_CLASS_RESOLUTION)

#define COARSE_SIZE_LIMIT		(FINE_SIZE_LIMIT + \
					 + (COARSE_SIZE_CLASS_RESOLUTION  \
					    *NUM_COARSE_SIZE_CLASSES))

#define LARGE_OBJ_SIZE_CLASS_NUM 	(NUM_FINE_SIZE_CLASSES +\
					 NUM_COARSE_SIZE_CLASSES)
#define NUM_LOGICAL_SIZE_CLASSES	(LARGE_OBJ_SIZE_CLASS_NUM+1)

#define LOGICAL_SIZE_CLASS_OF(bytes) \
    ((((bytes)<=FINE_SIZE_LIMIT)\
    ? FINE_SIZE_CLASS_NUM(bytes) \
    : (((bytes)<=COARSE_SIZE_LIMIT) \
	? COARSE_SIZE_CLASS_NUM(bytes)\
	: LARGE_OBJ_SIZE_CLASS_NUM)))

#endif /* _H_SIZECLAS */
