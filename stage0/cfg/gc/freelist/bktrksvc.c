/*-----------------------------------------------------------------*-C-*---
 * File:    handc/cfg/gc/freelist/bktrksvc.c
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.4
 * File mod date:    1997-11-29 23:10:47
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose:          freelist GC back-pointer service
 *------------------------------------------------------------------------*/

#import <mach/mach.h>
#import <mach/message.h>
#import <servers/netname.h>
#import <mach/mach_error.h>
#import <ansi/stdlib.h>
#import "backtrk.h"
#import <mach/cthreads.h>

#define MAX_PTR_LOCATIONS (100)

static ptr_location *ptr_location_ptr;
static ptr_location *ptr_location_limit;

int find_live_ptr_in_sc( MEMSizeClass *c, UINT_32 item )
{
MEMHeader *block;
UINT_32 *p, i, n;

    block = c->alloced;
    while (block)
    {
        p = (UINT_32 *)(block + 1);
	n = block->actual_size;
	for (i=0; i<n; i+=sizeof(UINT_32))
	{
	    if (*p++ == item)
	    {
		ptr_location_ptr->gc_ptr = (UINT_32)(block+1);
		ptr_location_ptr->offset = i;
		ptr_location_ptr++;
	        if (ptr_location_ptr >= ptr_location_limit)
		    return 1;
	    }
	}
	block = block->next;
    }
    return 0;
}

kern_return_t find_live_pointers( port_t heap_server, 
				  UINT_32 word, 
				  ptr_locations_list locations, 
				  unsigned int *locationsCnt )
{
unsigned i;

    printf( "backtracking: %#x...", word );
    fflush( stdout );
    ptr_location_ptr = locations;
    ptr_location_limit = ptr_location_ptr + MAX_PTR_LOCATIONS;
    
    for (i=0; i<NUM_SIZE_CLASSES; i++)
    {
        if (find_live_ptr_in_sc( &size_classes[i], word ))
	    break;
    }
    find_live_ptr_in_sc( &other_size_class, word );
    *locationsCnt = ptr_location_ptr - locations;
    printf( "%u refs\n", *locationsCnt );
    return KERN_SUCCESS;
}

#define BACKTRACK_MAX_REQUEST_SIZE (64)
#define BACKTRACK_MAX_REPLY_SIZE (1024)

any_t backtracking_server( any_t the_port )
{
kern_return_t rc;
msg_header_t *msg = (msg_header_t *)alloca(BACKTRACK_MAX_REQUEST_SIZE);
msg_header_t *reply = (msg_header_t *)alloca(BACKTRACK_MAX_REPLY_SIZE);

    while (TRUE)
    {        
        /* Receive a request from a client. */
        msg->msg_local_port = (port_t)the_port;
        msg->msg_size = BACKTRACK_MAX_REQUEST_SIZE;
        rc = msg_receive(msg, MSG_OPTION_NONE, 0);
        if (rc != RCV_SUCCESS)
	{
	    /* ignore errors */
	}
	
        /* Feed the request into the server. */
	
        backtrack_server( msg, reply );

        /* Send a reply to the client. */
        reply->msg_local_port = (port_t)the_port;
        rc = msg_send(reply, MSG_OPTION_NONE, 0);
        if (rc != SEND_SUCCESS) 
	{
	    /* ignore errors */
	}
    }
}

void init_backtracking_service( void )
{
port_t server_port;
kern_return_t rc;
char our_name[30];
extern int getpid( void );

    sprintf( our_name, "backtrack-%d", getpid() );
    
    rc = port_allocate( task_self(), &server_port );
    if (rc != KERN_SUCCESS) 
    {
        mach_error( "port_allocate failed", rc );
        exit(1);
    }

    /* Register with the Network Name Server. */
    
    rc = netname_check_in( name_server_port, 
    			   our_name,
			   PORT_NULL,
			   server_port );
    if (rc != KERN_SUCCESS) 
    {
        mach_error( "netname_check_in failed", rc );
        exit(1);
    }

    cthread_fork( backtracking_server, (any_t)server_port );
}
