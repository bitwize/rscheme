/**********************************************
THIS FILE WAS AUTOMATICALLY COPIED FROM THE
RSCHEME SOURCE TREE, AND THE ORIGINAL MAY CHANGE.
HENCE, DO NOT MODIFY THIS FILE BY HAND!
RScheme Build (v0.7.1M, 97.04.03)
**********************************************/

#include <stdlib.h>

#define ALLOC(t) ALLOCN(t,1)
#define ALLOCN(t,n) ((t *)malloc( sizeof(t) * (n) ))
#define REALLOCN(v,t,n) ((v) = (t *)realloc( (v), sizeof(t) * (n) ))

