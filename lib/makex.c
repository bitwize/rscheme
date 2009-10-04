#include <stdio.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <unistd.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

int verbose = 1;

int believe_caller = 0;         /* no indirection, doesn't depend on
                                   existence of the interpreter */

/* do indirections through other makex'd images */

char *indirect_interpreter( char *name )
{
  FILE *f = fopen( name, "r" );
  int c1, c2;
  if (!f)
    {
      perror( name );
      return name;
    }

  c1 = fgetc( f );
  c2 = fgetc( f );
  if ((c1 == '#') && (c2 == '!'))
    {
      int i;
      char tmp[1024];

      /* skip seperating whitespace */ 
      while ((c1 = fgetc(f)) == ' ');

      for (i=0; !isspace(c1); i++)
	{
	  tmp[i] = c1;
	  c1 = fgetc( f );
	}
      tmp[i] = 0;
      if (verbose)
	printf( "indirected to: %s\n", tmp );
      return strdup(tmp);
    }
  else
    return name;
}

char *search_path_for_interp( char *name )
{
  if (name[0] == '/')
    {
      return name;
    }
  else
    {
      char *p = getenv( "PATH" );
      char temp[1024];
      while (p)
	{
	  struct stat sb;
	  char *x = strchr( p, ':' );
	  if (x)
	    {
	      memcpy( temp, p, x-p );
	      temp[x-p] = 0;
	      p = x+1;
	    }
	  else
	    {
	      strcpy( temp, p );
	      p = NULL;
	    }
	  if (temp[strlen(temp)-1] != '/')
	    strcat( temp, "/" );
	  strcat( temp, name );
	  if (stat( temp, &sb ) == 0)
	    {
	      if (verbose)
		printf( "found in PATH at: %s\n", temp );
	      return strdup( temp );
	    }
	}
      fprintf( stderr, "%s: not found in PATH\n", name );
      exit(1);
    }
}


int main( int argc, const char **argv )
{
  int fd, i;
  char *opt;

  if (argc < 3)
    {
      fprintf( stderr, "usage: %s [-q] [-v] [-f] file interpreter\n", argv[0] );
      fprintf( stderr, "  (-v causes executable to be invoked with `-image' instead of `-qimage',\n" );
      fprintf( stderr, "   if `interpreter' does not contain a slash, PATH is searched,\n" );
      fprintf( stderr, "   if `interpreter' is itself the result of makex, then *its*\n" );
      fprintf( stderr, "   interpreter is used)\n" );
      fprintf( stderr, "   -f forces use the interpreter as given on the command line.\n" );
      return 1;
    }
  i = 1;

  if ((i < argc) && (strcmp(argv[i],"-q") == 0))
    {
      i++;
      verbose = 0;
    }

  if ((i < argc) && (strcmp(argv[i],"-v") == 0))
    {
      i++;
      opt = " -image\n";
    }
  else
    {
      opt = " -qimage\n";
    }

  if ((i < argc) && (strcmp(argv[i],"-f") == 0))
    {
      believe_caller = 1;
      i++;
    }

  if (1) {
    struct stat sb;
    
    if (stat(argv[i],&sb) < 0)
      {
	perror( argv[i] );
	exit(1);
      }

    /* make it also executable by everybody that can read it */
    chmod( argv[i], sb.st_mode | ((sb.st_mode & 0444) >> 2));
  }

  {
    char *interp =
      believe_caller ? (char *)argv[i+1] 
      : indirect_interpreter( search_path_for_interp( (char *)argv[i+1] ) );

    fd = open( argv[i], O_WRONLY );
    write( fd, "#!", 2 );
    write( fd, interp, strlen(interp) );
    write( fd, opt, strlen(opt) );
    close( fd );
    return 0;
  }
}
