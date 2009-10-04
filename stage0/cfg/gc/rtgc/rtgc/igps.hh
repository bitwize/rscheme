#ifndef IGPS_HH
#define IGPS_HH

// This file contains the declaration of igps maintaining class.

#include <rtgc/config.hh>
#include <rtgc/langtype.h>
#include <rtgc/scheduler.hh>

class igp_manager{
#ifdef GENERATIONAL
    friend double scheduling_manager::get_max_table_occupancy_ratio(int);
    friend void scheduling_manager::reset_at_gc_flip(void);
#endif

// A list of intergenerational pointers.  intergenerational pointers
// (or igp's) are pointers from an old generation to a younger
// generation. igp_list contains the location of the intergenerational
// pointers, not the pointer itself. The reason is the integeneration
// pointer may be overwritten by the mutator. add_igp() adds a
// pointer to an intergenerational point to the list. switch_igp()
// resets the scanner at the end of the GC cycle of the younger generation.
    
    // ipg_list[older][younger][i] is an array of igps from the
    // generation (older + 1) to the generation (younger). (Since
    // there is no igp form the youngest generation, we omit the
    // entry. That's why we must add 1 to the older generation index.)
    // We have a pair of igp lists for each combination of generations.
    // One is for igp from an old white object, the other is for those
    // from an old shaded object. i represents which we are looking
    // at. This pair of lists alternate every old GC cycle, so we
    // can't say which is which. (We call the igp from an old white
    // object as white igp, one from an old shaded object as black igp.)
#ifdef GENERATIONAL
    pos_ptr_addr
	igp_list[NUMBER_OF_GENERATIONS-1][NUMBER_OF_GENERATIONS-1][2][NUM_IGP];
#endif
    // black_igps[i][j] points to the igp list of black igps from the
    // generation i to the generation j.  This is designed to point to
    // igp_list[i-1][j][0] or igp_list[i-1][j][1], whichever contains
    // black igps. Here you notice that you don't have to substract 1
    // from the first index when you access black_igps. We only have
    // to worry about it in the constructor and the switch_igp_list().
    // white_igps[i][j] points to the igp list containing white igps,
    // and must point to one of the lists igp_list[i-1][j][0] and
    // igp_list[i-1][j][1] whichever old_igps[i] does not point to. At
    // the end of the GC cycle of the older generation, the values
    // of od_gps[i] and new_igps[i] are exchanged, in order to switch
    // the role of the IGPS lists.
    
    pos_ptr_addr *black_igps[NUMBER_OF_GENERATIONS][NUMBER_OF_GENERATIONS];
    pos_ptr_addr *white_igps[NUMBER_OF_GENERATIONS][NUMBER_OF_GENERATIONS];

    // black_igp_num[i][j] is the number of entries in the igp_list
    // pointed by black_igps[i][j]. white_igp_num[i][j] is the number
    // of entries in white_igps[i][j].  black_igp_next[i][j] indexes the
    // next entry to be scaned in black_igps[i][j] for tracing the
    // generation j.  The same is applied to white_igp_next[i][j].
    // black_igp_store[i][j] pointes to the location where igp is
    // stored after used in tracing. This is for compacting.


    //    ----     
    //   |****| 	         ** is the pointer used for tracing
    //   |****|  	   	    and stored into the compaction space.
    //   |&&&&|<- igp_store      && is the pointer which is used for
    //   |&&&&|  	            tracing, but its copy is in the compaction
    //   |++++|<- igp_next          space if necessary.
    //   |++++|  	         ++ is the pointer which is not used for 
    //   |    |<- igp_bndry[i]      tracing yet.
    //   |    |                    (i is NUM_OF_STEPS )
    //   |    |  
    //   |    |  
    //    ----   


    //	Suppose NUMBER_OF_STEPS is 3
    // 	  ----			    *** is the igps detected 2 cycles before
    //	 |****|			        of the younger generation.
    //	 |****|			        	 	     
    //	 |****|			    &&& is the igps detected 1 cycle before
    //   |&&&&| <- igp_bndry[0]		of the younger generation
    //	 |&&&&|		    			 	     
    //   |&&&&|		    			 	     
    //	 |++++|	<- igp_bndry[1]	    +++ is the igps detected in this 
    //	 |++++|	                        younger generation cycle
    //	 |++++|	               			 
    //	 |    |	<- igp_bndry[2]
    //	 |    |
    //   |    |				      
    //	  ----				      

    int	black_igp_bndry[NUMBER_OF_GENERATIONS]
	[NUMBER_OF_GENERATIONS][NUMBER_OF_STEPS+1] ;
    int white_igp_bndry[NUMBER_OF_GENERATIONS]
	[NUMBER_OF_GENERATIONS][NUMBER_OF_STEPS+1];
    int black_igp_next[NUMBER_OF_GENERATIONS][NUMBER_OF_GENERATIONS];  
    int white_igp_next[NUMBER_OF_GENERATIONS][NUMBER_OF_GENERATIONS];	
    int black_igp_store[NUMBER_OF_GENERATIONS][NUMBER_OF_GENERATIONS];  
    int white_igp_store[NUMBER_OF_GENERATIONS][NUMBER_OF_GENERATIONS];	
  public:
    // 
    // add_black_igp() adds the argument to the black_igp_list. The
    // first argument should be the location of an IGP in a shaded
    // object. This function add the pointer as an igp from the
    // generation old to the generation young.
    void add_black_igp(pos_ptr_addr,int old, int young);

    // add_white_igp() adds the argument to the white_igp, which is thrown
    // away at the end of the GC cycle. The argument should be the location of
    // an IGP in a white object, the index of the older generation and
    // the index of the younger generation.
    void add_white_igp(pos_ptr_addr, int old, int young);

    // Return an IGP pointing into the generation young.
    int scan_igp_list(int young);

    // test each igp and gray it if it is white. This function also promotes
    // the igp and compacts igp list.
    int test_and_gray(pos_ptr_addr igp,int young, int old, int black_flag);

    // switch_igp_lists should be called at the end of a GC cycle of the
    // older generation.
    void switch_igp_lists(int old);

    // reset the indexes at the end of a GC cycle of the younger generation,
    // to get ready for the next GC cycle. This also reduces the
    // number of IGP's.
    void reset_igp_lists(int young);

    // Contructor resets the black_igps and white_igps. 
    igp_manager(void);
};
#endif // IGPS_HH
