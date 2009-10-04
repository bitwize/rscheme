#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <sys/wait.h>

void do_open_defect( void );
void do_view_defect( void );
void sb_gateway( char *as_stdin, ... );

#define MAX_ENTRIES 1000

typedef struct {
    char *name;
    char *val;
} entry;

char *makeword(char *line, char stop);
char *fmakeword(FILE *f, char stop, int *len);
char x2c(char *what);
void unescape_url(char *url);
void plustospace(char *str);

entry entries[MAX_ENTRIES];
unsigned num_entries;

char *get_value( const char *name )
{
  unsigned i;

  for (i=0; i<num_entries; i++)
    if (strcmp( entries[i].name, name ) == 0)
      return entries[i].val;
  return NULL;
}

char *kill_crs( char *dst )
{
  char *s = dst, *d = dst;

  while (*s)
    {
      if (*s != '\r')
	*d++ = *s;
      s++;
    }
  *d++ = 0;
  return dst;
}

main(int argc, char *argv[]) {
    register int x,m=0;
    int cl;
    char *op;

    printf("Content-type: text/html%c%c",10,10);

    if(strcmp(getenv("REQUEST_METHOD"),"POST")) {
        printf("This script should be referenced with a METHOD of POST.\n");
        printf("If you don't understand this, see this ");
        printf("<A HREF=\"http://www.ncsa.uiuc.edu/SDG/Software/Mosaic/Docs/fill-out-forms/overview.html\">forms overview</A>.%c",10);
        exit(1);
    }
    if(strcmp(getenv("CONTENT_TYPE"),"application/x-www-form-urlencoded")) {
        printf("This script can only be used to decode form results. \n");
        exit(1);
    }
    cl = atoi(getenv("CONTENT_LENGTH"));

    for(x=0;cl && (!feof(stdin));x++) {
        m=x;
        entries[x].val = fmakeword(stdin,'&',&cl);
        plustospace(entries[x].val);
        unescape_url(entries[x].val);
        entries[x].name = makeword(entries[x].val,'=');
    }
    num_entries = m+1;
    
    op = get_value( "operation" );

#if 0
    printf( "<H1>Basic Query</H1>");
    printf( "<ul>\n" );
    for (x=0; x<num_entries; x++)
      {
	printf( "<li> [%d] %s = '%s'\n", x, entries[x].name, entries[x].val );
      }
    printf( "</ul>\n" );
#endif

    if (!op)
      {
	printf( "<H1>Query Results</H1>");
	printf( "Missing operation\n" );
      }
    else if (strcmp(op,"open-defect") == 0)
      {
	do_open_defect();
      }
    else if (strcmp(op,"view-defect") == 0)
      {
	do_view_defect();
      }
    else
      {
	printf( "<h1>Query Results</h1>\n" );
	printf( "Invalid operation '%s'\n", op );
      }
}

char temp[30000];

void do_open_defect( void )
{
  FILE *f;
  char sev[4];

  if (!get_value("title") || strlen(get_value("title")) < 10)
    {
      printf( "Please provide a better title\n" );
      return;
    }
  if (!get_value("summary") || strlen(get_value("summary")) < 10)
    {
      printf( "Please provide a better summary\n" );
      return;
    }
  if (!get_value("remarks") || strlen(get_value("remarks")) < 10)
    {
      printf( "Please provide some decent remarks\n" );
      return;
    }
  if (!get_value("sev") || !strchr("1234", get_value("sev")[0]))
    {
      printf( "Please use an appropriate severity level\n" );
      return;
    }

  printf( "<title>Submit Change Request</title>\n");
  printf( "<h1>Submit Change Request</h1>\n");
  printf( "<h2>Query Results</h2>\n<hr>\n<pre>\n" );

  sev[0] = get_value("sev")[0];
  sev[1] = 0;

  fflush( stdout );
  sb_gateway( get_value("remarks"),
	     "--changereq", 
	     "--group", get_value("group"),
	     "--title", kill_crs( get_value("title") ),
	      "--severity", sev,
	     "--summary", kill_crs( get_value("summary") ),
	     "--remarks", "-",
	     NULL );
  printf( "\n</pre><hr>\n<p>\nOperation submitted\n" );
}

void do_view_defect( void )
{
  FILE *f;
  char *cr, sev[4];

  cr = get_value("changereq");
  if (!cr)
    {
      printf( "Please provide a change request number\n" );
      return;
    }

  printf( "<title>Change Request %s</title>\n", cr );
  printf( "<h1>Change Request %s</h1>\n", cr);
  printf( "<h2>Query Results</h2>\n<hr>\n<pre>\n" );

  fflush( stdout );
  sb_gateway( "", "--changereq", cr, NULL );
  printf( "\n</pre><hr>\n<p>\nok\n" );
}

void sb_gateway( char *as_stdin, ... )
{
  char *(argv[100]);
  char **p, *a;
  va_list va;
  int pid,s, fd[2];

  if (pipe( fd ) < 0)
    perror( "pipe" );
  
  va_start( va, as_stdin );

  setenv( "SB_LOGIN", get_value("login"), 1 );
  setenv( "SB_SERVER", get_value("server"), 1 );
  p = argv;

  *p++ = "sb";
  
  while (a = va_arg( va, char * ))
    {
      *p++ = a;
    }
  *p++ = NULL;

  pid = fork();
  if (pid == 0)
    {
      close( fd[1] );
      close( 0 );
      dup2( fd[0], 0 );
      close( fd[0] );

close(2);
dup2(1,2);

      /* we're the child */
      execv( "/u/rscheme/tools/sb", argv );
      perror( "execv" );
      exit(1);
    }
printf( "child pid %d\n", pid );
  /* we're the parent */

  close( fd[0] );
  kill_crs( as_stdin );
  write( fd[1], as_stdin, strlen(as_stdin) );
  close( fd[1] );
  
  if (waitpid( pid, &s, 0 ) < 0)
    {
      perror( "waitpid" );
    }
  if (WIFEXITED(s))
    {
      if (WEXITSTATUS(s))
	{
	  fprintf( stderr, "child exited with rc %d\n", WEXITSTATUS(s) );
	}
    }
  else
    fprintf( stderr, "child died unexpectedly\n" );
}
