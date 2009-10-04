#include <iostream.h>
#include <assert.h>
#include <limits.h>
#include <rtgc/scheduler.hh>
#include <rtgc/colorset.hh>
#include <rtgc/gc.hh>
#ifdef INLINES
#include <rtgc/inlines.hh>
#endif

scheduling_manager gc_scheduler;

scheduling_manager::scheduling_manager(void){
    throttle_ratio = 1.5;
    full_throttle = 0;
#ifdef GENERATIONAL
    for(int i = 0; i < NUMBER_OF_GENERATIONS; i++){
	wait_time[i] = WAIT_TIME;
	initial_amount[i] = 0;
	prev_alloc_time[i] = 0;
    }
#endif
}

#ifdef GENERATIONAL
// This scheduling is a allocation time driven, two-phase scheduler,
// considering the occupancy ratio of the ipgs lists and pos lists.
// 
// First of all, work amount is not calculated by the real time. It is
// calculated based on the allocation time. So, if the GC user want to
// guarantee that some realtimeness, he must calculate what amount of
// memory should be required to accomplish the task, and the derive
// the maximum amount of work which might have to be done by the
// collector for that amount of allocation, and add the time required
// for the job itself. Theoretically, that'll be the realtime deadline
// which can be guaranteed by the system.
//
// In the generational version of collector, the definition of
// allocation time is not clear, especially for the older generation.
// Since we want to use the amount of objects in the older generation
// to calculate the amount of work for it, we will use promotion time,
// that is, how much objects are promoted into this generation. This
// is a simple anology of allocation time, which is roughly how much
// objects has been allocated.
//
// The calculation of allocation and promotion time is, simetimes, not
// easy,  because of the promoting objects out of the generation. So
// we define the allocation(promotion) time as: 
//    T = (amount of live objects in this generation)
//         + (amount of live objects in the older generations)
//         - (initial amount of live objects in the older generations)
// (The live object means non-free objects.) 
// 
// The allocation policy is simple, in this scheduler. We have two
// phases. In the first phase, we do no job. The work amount is equal
// to 0. This phase lasts until T becomes larger than the wait time,
// expressed by W here.  In the second phase, we trace objects at a
// certain ratio, usually specified by the user. If we express the
// tracing ratio as R. Then the work amount A is expressed as:
//    
//    A = 0		if T < W        
//      = dT * R	if T >= W
// 
// If this work is done, the GC cycle is guranteed to terminate by
// the allocation time (W * R) / (R - 1), by solving the equation
// T = (T - W) * R assuming all objects are alive. If the R is smaller
// than 1, there is no guarantee in termination. Of course, we know
// that the thing does not work just like this, because some time we
// cannot find gray objects until the tracing in the younger
// generation advances, but in general, we believe this formula is a
// good approximation.
// (The formula is not always true, especially when the initial amount
// of live objects in the target generation is larger than the wait
// time. In that case, we begin the tracing immediately, but we are
// still behind the schedule. In that case, the tracing terminates by
// the allocation time ((Initial amount of live) * R) / (R - 1)
// 
// We also have to worry about the IGP and POS. If the table is, say
// 60% full, then the 60% of tracing must have been done. So if one of
// the highest occupacy ratio of the tables is r, then
// 	(W * R * r)/(R - 1) - (the amount of traced objects)  
// where
// (the amount of traced objects)
// = (the amount of objects in the generation)
//    - (the amount of non black objects in this generation)
//    + (the amount of the objects in the older generations)	
//    - (the initial amount of the object in the older generations.
// 

INT_32 scheduling_manager::get_work_per_increment(int generation){
    INT_32 amount_in_larger_gens = 0;
    INT_32 live_in_current_generation = 0;
    INT_32 black_in_current_generation = 0;
    INT_32 alloc_time, work_to_be_done, amount_traced;
    
    // For full collect, full_throttle should be non zero.
    if(full_throttle){
	return(INT_MAX);
    }
    // get the amount of objects in the generaions larger than the
    // current generation.
    for(int i = generation + 1; i < NUMBER_OF_GENERATIONS; i++){
	for(int j = 0; j < NUM_SIZE_CLASSES ; j++){
	    color_set *c = gc.get_gen(i)->get_color_set(j);
	    assert(c->get_num_of_objects() >= c->get_num_of_free());
	    amount_in_larger_gens
		+= (c->get_num_of_objects() - c->get_num_of_free()) << j;
	}
    }
    
    // Get the number of live objects and black objects in this generation.
    for(int j = 0; j < NUM_SIZE_CLASSES; j++){
	color_set *c = gc.get_gen(generation)->get_color_set(j);
	assert(c->get_num_of_objects() >= c->get_num_of_free());
	assert(c->get_num_of_objects() >= c->get_num_of_non_black());
	live_in_current_generation
	    += (c->get_num_of_objects() - c->get_num_of_free()) << j;
	black_in_current_generation
	    += (c->get_num_of_objects() - c->get_num_of_non_black()) << j;
    }    
    
    // Get the allocation time. Allocation time for the generaion
    // is the amount of live objects in this generation plus the amount of
    // objects which have migrated out of this generation.
    assert(amount_in_larger_gens >= initial_amount_larger_gens[generation]);
    alloc_time
	= amount_in_larger_gens - initial_amount_larger_gens[generation]
	    + live_in_current_generation;
    amount_traced
	= amount_in_larger_gens - initial_amount_larger_gens[generation]
	    + black_in_current_generation;

    // Calculate the work amount with respect to the allocation time.
    if(alloc_time < wait_time[generation]){
	work_to_be_done = 0;
    }else{
	assert(alloc_time >= prev_alloc_time[generation]);
	work_to_be_done =
	    int((alloc_time - prev_alloc_time[generation]) * throttle_ratio);
    }
    prev_alloc_time[generation] = alloc_time;
    
    // Next, adjust the work_to_be_done by the occupancy ratio of the table.

    // Get the maximum number of occupancy ratios in relation with this
    // generation.
    double table_ratio = (get_max_table_occupancy_ratio(generation)
			  - table_ratio_bias) * table_ratio_steep ;
    
    INT_32 work_tmp; 
    
    // We adopt different formula to get the throttle from the table
    // occupancy depending on whether we waited tracing until enough amount 
    // of object was allocated or we started tracing immediately. If the 
    // initial amount of objects in the generation is larger than wait_time
    // we start tracing immediately after the tracing begins.
    if(initial_amount[generation] < wait_time[generation])
	work_tmp
	    = int(((wait_time[generation]*throttle_ratio* table_ratio)
		   / (throttle_ratio - 1.0))
		  - amount_traced);
    else
	work_tmp
	    = int(((initial_amount[generation]*throttle_ratio*table_ratio)
		   / (throttle_ratio - 1.0))
		  - amount_traced);

    if(work_tmp > work_to_be_done)
	return(work_tmp);
    else
	return(work_to_be_done);
}
#else
INT_32 scheduling_manager::get_work_per_increment(int){
    // For full collect, full_throttle should be non zero.
    if(full_throttle){
	return(INT_MAX);
    }
    return(work_per_increment);
}
#endif GENERATIONAL

#ifdef GENERATIONAL
void scheduling_manager::set_throttle_setting(double new_setting)
{
    if(new_setting < 1.0){
	cout << "No termination is guaranteed.\n";
	cout << "IGP pointer list may overflow.\n";
    }
    if(new_setting != 1.0)
	throttle_ratio = new_setting;
    else
	// To avoid devided by zero error.
	throttle_ratio = 1.01;
}
#else GENERATIONAL
//***************************************************************************
// scheduling_manager::set_throttle_setting
//
// This routine changes the throttle setting to a new value.
// Since all the garbage collector is worried about is the amount of
// work it needs to do for the next increment, this routine simply
// calculates the work_per_increment, which is the product of the
// throttle setting and the number of bytes that can be allocated per
// collection increment.

void scheduling_manager::set_throttle_setting(double new_setting)
{
    work_per_increment = int(new_setting *AMOUNT_TO_ALLOCATE_PER_INCREMENT);
    throttle_ratio = new_setting;
}
#endif  GENERATIONAL

double scheduling_manager::get_throttle_setting(void){
    return(throttle_ratio);
}

void scheduling_manager::reset_at_gc_flip(void){
#ifdef  GENERATIONAL
    INT_32 amount,k;

    for(int i = 0; i < NUMBER_OF_GENERATIONS; i++){
	if(!gc.get_gen(i)->tracing_is_done()){
	    continue;
	}

	// calc initial amount.
	amount = 0; 
	for(int j = 0; j < NUM_SIZE_CLASSES; j++){
	    color_set *c = gc.get_gen(i)->get_color_set(j);
	    assert(c->get_num_of_objects() >= c->get_num_of_free());
	    amount += (c->get_num_of_objects() - c->get_num_of_free()) << j;
	}
	initial_amount[i] = amount;
	prev_alloc_time[i] = amount;
	// calc initial amount in the larger generations.
	amount = 0;
	for(k = i + 1; k < NUMBER_OF_GENERATIONS; k++){
	    for(int j = 0; j < NUM_SIZE_CLASSES; j++){
		color_set *c = gc.get_gen(k)->get_color_set(j);
		assert(c->get_num_of_objects() >= c->get_num_of_free());
		amount += (c->get_num_of_objects()-c->get_num_of_free()) << j;
	    }
	}
	initial_amount_larger_gens[i] = amount;

	// record the initial amount of table occupance ratio.
	for(k = i + 1; k < NUMBER_OF_GENERATIONS; k++){
	    initial_black_igp_ratio[k][i]
		= double(gc.get_igpm()->black_igp_bndry[k][i][NUMBER_OF_STEPS])
		    / NUM_IGP;
	    initial_white_igp_ratio[k][i]
		= double(gc.get_igpm()->white_igp_bndry[k][i][NUMBER_OF_STEPS])
		    / NUM_IGP;
	}
    }
#endif
}

#ifdef  GENERATIONAL
double scheduling_manager::get_max_table_occupancy_ratio(int gen){
    double max,tmp;

    generation *g = gc.get_gen(gen);
    if(g->stored_into_list_head > g->stored_into_list_tail){
	max = double(g->stored_into_list_tail + STORED_INTO_LIST_SIZE
		     - g->stored_into_list_head)
	    / STORED_INTO_LIST_SIZE;
    }else{
	max = double(g->stored_into_list_tail - g->stored_into_list_head)
	    / STORED_INTO_LIST_SIZE;
    }
    
    // Get the igp list occupancy ratio. We can consume only 
    // 1/(NUMBER_OF_STEPS+1) space in this gc cycle because it takes 
    // (NUMBER_OF_STEPS+1) GC cycles before throwing away the igps.
    for(int i = gen + 1; i < NUMBER_OF_GENERATIONS; i++){
	tmp = (double(gc.get_igpm()->black_igp_bndry[i][gen][NUMBER_OF_STEPS])
	         / NUM_IGP
	       - initial_black_igp_ratio[i][gen]) * double(NUMBER_OF_STEPS+1);

	if(tmp > max) max = tmp;

	tmp = (double(gc.get_igpm()->white_igp_bndry[i][gen][NUMBER_OF_STEPS])
	         / NUM_IGP
	       - initial_white_igp_ratio[i][gen]) * double(NUMBER_OF_STEPS+1);

	if(tmp > max) max = tmp;
    }
    return(max);
}
#endif  GENERATIONAL
