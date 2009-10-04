#include <rtgc/sizclass.h>

static int table_0[BASE*BASE] = {0,0,1,2,2,3,3,3,3,4,4,4,4,4,4,4,4,
			         5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,
			         6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
			         6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
			         7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
			         7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
			         7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
			         7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
			         8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
			         8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
			         8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
			         8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
			         8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
			         8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
			         8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
			         8,8,8,8,8,8,8,8,8,8,8,8,8,8,8};




/* static int table_1[BASE] = {8,9,9,10,10,10,10,10,11,11,11,11,11,11,11,11,
			    12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,
			    13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,
			    13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,
			    14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,
			    14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,
			    14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,
			    14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,
			    15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,
			    15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,
			    15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,
			    15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,
			    15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,
			    15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,
			    15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,
			    15,15,15,15,15,15,15,15,15,15,15,15,15,15,15};
*/
 
/* static int table_1[BASE] = {4,5,6,6,7,7,7,7,8,8,8,8,8,8,8,8}; */
static int table_2[BASE] = {8,9,10,10,11,11,11,11,12,12,12,12,12,12,12,12};
static int table_3[BASE] = {12,13,14,14,15,15,15,15,16,16,16,16,16,16,16,16};
static int table_4[BASE] = {16,17,18,18,19,19,19,19,20,20,20,20,20,20,20,20};
static int table_5[BASE] = {20,21,22,22,23,23,23,23,24,24,24,24,24,24,24,24};
static int table_6[BASE] = {24,25,26,26,27,27,27,27,28,28,28,28,28,28,28,28};
static int table_7[BASE] = {28,29,30,30,31,31,31,31,32,32,32,32,32,32,32,32};

/*
  get_size_class

  Find the size class of the given size. The size class of `n' is
  basically given by calculating ceil (log2 (n)). However, since
  calling log and ceil is expensive, we do table lookups in
  conjunction with bit shifts to calculate the values.

 Note: get_size_class and table_0 were originally defined as inlined static 
      object, but now it is called only in the initialization, so we made
      it external. 
*/

int get_size_class (size_t n)
{
    /* If the size is less than the base size, the size class can be */
    /* read off directly by indexing into the first table.           */
    if (n < BASE*BASE)
      return table_0 [ n ];
    else 
      return get_size_class_non_base_case(n);
}

int get_size_class_non_base_case(size_t n)
{
    /* If the size is greater than the base size but less than the next    */
    /* power of the base size, we shift right by the number of bits in the */
    /* base size (to chop off the lower bits) and read off the size        */
    /* class by indexing into the second table. The index is (n-1)         */
    /* instead of n as in the previous case, in order to take care of      */
    /* the boundary conditions i.e. exact powers of 2 which fall at the    */
    /* boundaries for each table. The zeroth entry in table_1 through      */
    /* table_7 are for such powers of 2.                                   */
    
/* The next case has been folded into the base case in the file: */
/* sizeclass.ci                                                  */
/*    if (n < (BASE * BASE))
      return table_1 [ (n - 1) >> BASE_BITS ]; */
    
    /* The above procedure repeats for one higher power of base size   */
    /* until the max size class has been reached.                      */
    
    if (n < (BASE * BASE * BASE))
      return table_2 [ (n - 1) >> (BASE_BITS * 2) ];
    
    if (n < (BASE * BASE * BASE * BASE))
      return table_3 [ (n - 1) >> (BASE_BITS * 3) ];
    
    if (n < (BASE * BASE * BASE * BASE * BASE))
      return table_4 [ (n - 1) >> (BASE_BITS * 4) ];
    
    if (n < (BASE * BASE * BASE * BASE * BASE * BASE))
      return table_5 [ (n - 1) >> (BASE_BITS * 5) ];
    
    if (n < (BASE * BASE * BASE * BASE * BASE * BASE * BASE))
      return table_6 [ (n - 1) >> (BASE_BITS * 6) ];
    
/* This last case is slightly different than the rest because BASE (16)  */
/* ^ 8 is 0 for an unsigned integer.  So, we compare against BASE^8-1    */
/* and use <= to make up for it.                                         */

    if (n <= ((1U<<31) + ((1U<<31) - 1U)))

/*    if (n <= (((BASE * BASE * BASE * BASE * BASE * BASE * BASE * BASE)/2) -1)
	      +
	      (BASE * BASE * BASE * BASE * BASE * BASE * BASE * BASE)/2) */
      return table_7 [ (n - 1) >> (BASE_BITS * 7) ];
    
    return 0;
}
