#ifndef SCHEDULER_HH
#define SCHEDULER_HH
#include <rtgc/config.hh>
#include <rtgc/langtype.h>
#define WAIT_TIME 500000



// This class calculates the amount of work to be done in an increment. 
// Each generation askes this class for the amount of work, and do the
// specified amount of work. The bookkeeping of the work done in each
// generaion is done by each generation.
class scheduling_manager{
    float throttle_ratio;
    int full_throttle;

#ifdef GENERATIONAL
    // Wait time before begining the tracing.
    INT_32 wait_time[NUMBER_OF_GENERATIONS];
    // Initial live objects in this generation.
    INT_32 initial_amount[NUMBER_OF_GENERATIONS];
    // Initial live objects in the larger generations at the moment
    // the GC cycle of the current generation begins.
    INT_32 initial_amount_larger_gens[NUMBER_OF_GENERATIONS];
    // Allocation time at the previous increment.
    INT_32 prev_alloc_time[NUMBER_OF_GENERATIONS];
    // ratio of ipg lists.
    // e.g. initial_blakc_igp_ratio[old][young] stores the initial ratio of the
    // black igp list for the igps from the generation old to the generation
    // young.
    double
	initial_black_igp_ratio[NUMBER_OF_GENERATIONS][NUMBER_OF_GENERATIONS];
    double
	initial_white_igp_ratio[NUMBER_OF_GENERATIONS][NUMBER_OF_GENERATIONS];
    //The bias and steep to control the throttle against table occupancy.
    const double table_ratio_bias = 0.3;
    const double table_ratio_steep = 1.5;
#else
    INT_32 work_per_increment;
#endif
  public:
    scheduling_manager(void);
    INT_32 get_work_per_increment(int generaion);
    void set_throttle_setting(double new_setting);
    double get_throttle_setting(void);
    void reset_at_gc_flip(void);
    void set_full_throttle(void){full_throttle = 1;}
    void unset_full_throttle(void){full_throttle = 0;}
    
#ifdef  GENERATIONAL
    double get_max_table_occupancy_ratio(int generaion);
#endif
};

extern scheduling_manager gc_scheduler;
#endif SCHEDULER_HH
