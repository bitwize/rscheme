#include <unistd.h>
#include <sys/param.h>
#include <limits.h>
#include "rs_sys_threads_manager_p.h"

static char **vec_to_arry( obj vec )
{
  int i, n;
  char **v;
  
  n = SIZEOF_PTR(vec)/SLOT(1);
  v = (char **)malloc( sizeof(char *) * (n + 1) );

  for (i=0; i<n; i++)
    {
      obj ent = gvec_ref( vec, SLOT(i) );
      if (STRING_P(ent))
	v[i] = string_text(ent);
      else
	v[i] = ""; /* somebody else should have caught this */
    }
  v[n] = NULL;
  return v;
}


void rearrange_fds( obj vec )
{
  /*  `currently_at[fd]' contains the file descriptor
   *                     which is going to become `fd' 
   *                     ie, if the app wants the subprocess's FD 1
   *                     to be what is currently FD 9, then
   *                     initially, currently_at[1] = 9
   *  `going_to[fd]' is the file descriptor which `fd' is going
   *                 to become.  ie, if the app wants subprocess FD 1
   *                 to be the app's FD 9, then going_to[9] = 1
   *
   *  HENCE, for all x being manipulated, 
   *         currently_at[ going_to[x] ] = x
   */

  int *currently_at, *going_to;
  int i, fd, num_fd, max_fd;
  FILE *debug_port;

  if (DEBUG_SUBPROCESS)
    debug_port = fopen( "/tmp/debug.fd", "w" );

  num_fd = SIZEOF_PTR(vec)/SLOT(1);

  /* set up the transfer arrays */

  max_fd = 1024;
  currently_at = (int *) malloc (max_fd * sizeof (int));
  going_to = (int *) malloc (max_fd * sizeof (int));
  if (currently_at == NULL || going_to == NULL)
    /* Something we cannot recover from.
       XXX What is the normal procedure?  */
    abort ();

  for (i=0; i<max_fd; i++)
    {
      going_to[i] = -1;      /* by default, an FD isn't going anywhere */
      currently_at[i] = -1;  /* and we don't need it anywhere either */
    }

  for (i=0; i<num_fd; i++)
    {
      int fd;

      fd = fx2int( gvec_ref( vec, SLOT(i) ) );
      if (fd >= 0)
	{
	  if (fd >= max_fd)
	    {
	      int new_max_fd = max_fd + 128;
	      currently_at = (int *) realloc (currently_at,
					      new_max_fd * sizeof (int));
	      going_to = (int *) realloc (going_to, new_max_fd * sizeof (int));
	      if (currently_at == NULL || going_to == NULL)
		/* XXX Again, what is the procedure here?  */
		abort ();

	      while (max_fd < new_max_fd)
		{
		  currently_at[max_fd] = -1;
		  going_to[max_fd] = -1;
		  ++max_fd;
		}
	    }

	  if (going_to[fd] >= 0) /* already going elsewhere */
	    {
	      int fd2;

	      fd2 = dup(fd);
	      if (DEBUG_SUBPROCESS)
		fprintf( debug_port, "dup(%d) -> %d\n", fd, fd2 );
	      fd = fd2;
	    }
	  currently_at[i] = fd;
	  going_to[fd] = i;
	}
    }

  /* close fd's we don't want */

  if (DEBUG_SUBPROCESS)
    going_to[fileno(debug_port)] = 99;

  for (i=0; i<max_fd; i++)
    if (going_to[i] < 0)
      {
	close(i);
	if (DEBUG_SUBPROCESS)
	  fprintf( debug_port, "close(%d)\n", i );
      }

  if (DEBUG_SUBPROCESS)
    going_to[fileno(debug_port)] = -1;

  /* move them all into place */
  
  for (i=0; i<max_fd; i++)
    {
      if ((currently_at[i] >= 0) && (currently_at[i] != i))
	{
	  if (going_to[i] >= 0)
	    {
	      /* have to move this one out of the way */
	      fd = dup( i );
	      if (DEBUG_SUBPROCESS)
		fprintf( debug_port, "dup(%d) -> %d\n", i, fd );
	      going_to[fd] = going_to[i];
	      currently_at[going_to[i]] = fd;
	      close( going_to[i] );
	      if (DEBUG_SUBPROCESS)
		fprintf( debug_port, "close(%d)\n", going_to[i] );
	    }
	  dup2( currently_at[i], i );
	  if (DEBUG_SUBPROCESS)
	    fprintf( debug_port, "dup2(%d) -> %d\n", currently_at[i], i );
	  close( currently_at[i] );
	  if (DEBUG_SUBPROCESS)
	    fprintf( debug_port, "close(%d)\n", currently_at[i] );
	  going_to[i] = i;
	  currently_at[i] = i;
	}
    }
  free (currently_at);
  free (going_to);
  if (DEBUG_SUBPROCESS)
    fclose( debug_port );
}


obj rs_fork_and_exec( obj process_obj, obj path, obj argv, obj envv, obj fdv,
                      obj dir, obj new_pgrp )
{
  int pid;
  char **(dst[2]);
  int i, k;
  char temp[300];

  pid = fork();

  if (pid < 0)
    {
      /* there was an error */
      os_error( "fork", 0 );
    }

  if (pid > 0)
    {
      /* we are the parent */
      if (DEBUG_SUBPROCESS)
	printf( "  subprocess id is %d\n", pid );
      return int2fx(pid);
    }

  /* we are the child */

  rearrange_fds( fdv );

  if (truish( new_pgrp )) {
    setpgid(0,0);
  }

  if (truish(dir)) {
    if( chdir(string_text(dir)) ) { os_error( "chdir", 0 ); }
  }

  /* exec the program */

  execve( string_text(path), vec_to_arry(argv), vec_to_arry(envv) );
  sprintf( temp, "execve: %s\n", strerror(errno) );
  write( 2, temp, strlen(temp) );
  exit(2);
}
