
/*  returns 0 on success,
    else -ERRNO, i.e.,:

           -EPIPE : mailbox is closed
*/

int ksend_mailbox( obj mbox, obj item );      /* append to queue */
int ksend_mailbox_pre( obj mbox, obj item );  /* prepend onto queue */

int close_mailbox( obj mbox, obj leave );

void krelease_joiners( obj thr );
void kthread_unqueue_suspend( obj thr );

