#include <rtgc/config.hh>

#ifdef GENERATIONAL

#include <rtgc/igps.hh>
#include <rtgc/inlines.hh>

igp_manager::igp_manager(void){
    // Set the primary and auxiliary igp lists.
    
    for(int j = 0; j < NUMBER_OF_GENERATIONS; j++){
	// We don't use them, but initialize, especially for debugging purpose.
	black_igps[0][j] = NULL;
	white_igps[0][j] = NULL;
	for(int k = 0; k < NUMBER_OF_STEPS + 1; k++){
	    black_igp_bndry[0][j][k] = 0;
	    white_igp_bndry[0][j][k] = 0;
	}
	black_igp_next[0][j] = 0;
	white_igp_next[0][j] = 0;
	black_igp_store[0][j] = 0;
	white_igp_store[0][j] = 0;
    }


    for(int i = 1; i < NUMBER_OF_GENERATIONS; i++){
	for(int j = 0; j < NUMBER_OF_GENERATIONS - 1; j++){
	    // Order doesn't matter, as far as we don't access igp_list
	    // directly.
	    // Caution, see i - 1 in the first index of igp_list.
	    black_igps[i][j] = igp_list[i - 1][j][0];
	    white_igps[i][j] = igp_list[i - 1][j][1];

	    // Everything is initialized to 0.
	    for(int k = 0; k < NUMBER_OF_STEPS + 1; k++){
		black_igp_bndry[i][j][k] = 0;
		white_igp_bndry[i][j][k] = 0;
	    }
	    black_igp_next[i][j] = 0;
	    white_igp_next[i][j] = 0;
	    black_igp_store[i][j] = 0;
	    white_igp_store[i][j] = 0;
	}
	// We don't use them, but initialize. 
	black_igps[NUMBER_OF_GENERATIONS - 1][NUMBER_OF_GENERATIONS - 1] 
	   = NULL;
	white_igps[NUMBER_OF_GENERATIONS - 1][NUMBER_OF_GENERATIONS - 1] 
	   = NULL;
	for(int k = 0; k < NUMBER_OF_STEPS + 1; k++){
	    black_igp_bndry[NUMBER_OF_GENERATIONS - 1]
	                   [NUMBER_OF_GENERATIONS - 1][k] = 0;
	    white_igp_bndry[NUMBER_OF_GENERATIONS - 1]
	                   [NUMBER_OF_GENERATIONS - 1][k] = 0;
	}
	black_igp_next[NUMBER_OF_GENERATIONS - 1]
	              [NUMBER_OF_GENERATIONS - 1] = 0;
	white_igp_next[NUMBER_OF_GENERATIONS - 1]
	              [NUMBER_OF_GENERATIONS - 1] = 0;
	black_igp_store[NUMBER_OF_GENERATIONS - 1]
	               [NUMBER_OF_GENERATIONS - 1] = 0;
	white_igp_store[NUMBER_OF_GENERATIONS - 1]
	               [NUMBER_OF_GENERATIONS - 1] = 0;
    }
}

#ifndef INLINES
#include <rtgc/igps1.ci>
#include <rtgc/igps2.ci>
#endif

#endif // GENERATIONAL
