#ifndef scanroot_hh
#define scanroot_hh
// This file contains the declaration for the abstract root set class.
// Currently, the interface with the client program to get root sets 
// is very simple and we can only get the next root value or reset the
// pointer. This is not appropriate for multiple generations, so we implement
// a wrapping interface. This interface will be replaced bye new functions
// served by the clients.

#include <rtgc/config.hh>
#include <rtgc/colorset.hh>

// The class rootset abstracts multiple interfaces of the client root set
// examination. We have static variables to indicate the instance from
// which the last function call was issued to the client, and simulate the
// function calls as if we have multiple client call interfaces.
class abstract_root_set{
    // This indicates this instance's ID. Usually, we set the generation
    // number to this, and compare it with static variables below. If
    // they are equal, the last call of the corresponding client function is
    // from this instance. Otherwise, we have to adjust the scanning pointer
    // of client function by calling *_root_next appropriate times.
    int my_id;

    // These variable shows from which instance the corresponding root set was
    // examined last time.
    static int stable_id,quasistable_id,unstable_id;

    // The current position of the abstract scanning pointer of the root set.
    int stable_root_index,quasistable_root_index,unstable_root_index;

public:    
    void init(int my_id);

    void abstract_stable_root_reset(void);
    gc_obj_addr abstract_stable_root_next(void);

    void abstract_quasistable_root_reset( void );
    gc_obj_addr abstract_quasistable_root_next( void );

    void abstract_unstable_root_reset( void );
    gc_obj_addr abstract_unstable_root_next( void );
};
#endif // scanroot_hh
