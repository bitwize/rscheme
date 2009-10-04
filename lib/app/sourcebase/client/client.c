#define _USE_GNU
#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <unistd.h>
#include <pwd.h>

#include <sys/stat.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>

FILE *server_r, *server_w;
int server_fd;

const char *client_top;
const char *client_pwd;
int verbose = 0;

#define CLIENT_TYPE  "unix"
#define DEFAULT_SERVICE_PORT 2059
char *program = "a.out";

struct argv_list {
  const char *head;
  const char **values;
  int num_values;
};

void protocol_error( void )
{
    fprintf( stderr, "protocol error\n" );
    exit(1);
}

union cmd_arg {
    struct {
       void *addr;
       size_t len;
    } str;		/* s=8-bit len, S=16-bit len, F=32-bit len */
    int  val;		/* i=16 bit signed int, I=32-bit signed int */
};


struct cmd_table {
   char cmd;
   void (*proc)( union cmd_arg *argv );
   char *args;
};   

void exit_cmd( union cmd_arg *argv )
{
   fclose( server_w );
   fclose( server_r );
   close( server_fd );
   exit( argv[0].val );
}

void std_print_cmd( union cmd_arg *argv )
{
    fwrite( argv[0].str.addr, 1, argv[0].str.len, stdout );
}

void err_print_cmd( union cmd_arg *argv )
{
    fwrite( argv[0].str.addr, 1, argv[0].str.len, stderr );
}

void upload_cmd( union cmd_arg *argv );
void download_cmd( union cmd_arg *argv );
void download_tar_cmd( union cmd_arg *argv );
void snarf_stdin_cmd( union cmd_arg *argv );
void chmod_cmd( union cmd_arg *argv );

/*  type codes:
      s = string w/8-bit length
      S = string w/16-bit length
      F = string w/32-bit length
      i = 16-bit int
      I = 32-bit int
*/

static struct cmd_table cmds[] = {
   { 'x', exit_cmd, "i" },
   { 'p', std_print_cmd, "F" },
   { 'P', err_print_cmd, "F" },
   { 'u', upload_cmd, "s" },
   { 'd', download_cmd, "siF" },
   { 'm', chmod_cmd, "si" },
   { '-', snarf_stdin_cmd, "" },
   { 'T', download_tar_cmd, "F" },
   { 0 } };

const char *check_head( const char *str )
{
    if (str[0] == '-' && str[1] == '-')
       return str+2;
    return NULL;
}

struct argv_list pargs[150];
struct argv_list *p_limit;

void free_argv_list( struct argv_list *a )
{
  if (a >= pargs && a < p_limit)
    {
      p_limit--;
      while (a < p_limit)
	{
	  a[0] = a[1];
	  a++;
	}
    }
  else
    {
      fprintf( stderr, "illegal argv_list delete!\n" );
      abort();
    }
}

struct argv_list *alloc_argv_list( const char *head, const char **lst )
{
   p_limit->head = head;
   p_limit->num_values = 0;
   p_limit->values = lst;
   return p_limit++;
}

struct argv_list *defined( const char *head )
{
struct argv_list *p;

   for (p=pargs; p<p_limit; p++)
   {
      if (strcmp( p->head, head ) == 0)
         return p;
    }
    return NULL;
}

void connect_to_server( void )
{
  struct argv_list *c = defined( "server" );
  const char *h;
  char host[100];
  int port;
  int fd, rc;
  struct sockaddr_in s;
  struct hostent *h_ent;
  
  assert(c);
  if (c->num_values != 1)
    {
      fprintf( stderr, "`--server' has %d values, expected exactly 1\n", 
	      c->num_values );
      exit(1);
    }
  if (strchr( c->values[0], ':' ))
    {
      char *p;
      
      strcpy( host, c->values[0] );
      p = strrchr( host, ':' );
      *p++ = 0;
      h = host;
      port = atoi( p );
    }
  else
    {
      h = c->values[0];
      port = DEFAULT_SERVICE_PORT;
    }
  
  
  h_ent = gethostbyname( h );
  if (!h_ent)
    {
      fprintf( stderr, "host `%s' not found\n", h );
      exit(1);
    }

  bzero( &s, sizeof s );
	
  s.sin_family = AF_INET;
  s.sin_port = htons(port);
  memcpy( &s.sin_addr.s_addr, 
	  h_ent->h_addr_list[0], 
	  h_ent->h_length );

#if 0
  printf( "Contacting server on %s (%s), port %d\n", 
	 h,
	 inet_ntoa( s.sin_addr.s_addr ),
	 port );
#endif
  fd = socket( PF_INET, SOCK_STREAM, 0 );
  if (fd < 0)
    {
      perror( "socket" ); 
      exit(1); 
    }
  
  rc = connect( fd, (struct sockaddr *)&s, sizeof s );
  if (rc < 0)
    { 
      perror( "connect" ); 
      exit(1); 
    }
  
  server_w = fdopen( fd, "wb" );
  server_r = fdopen( fd, "rb" );
}


void missing( const char *head, const char *env )
{
   fprintf( stderr, "required option `--%s' is missing\n", head );
   fprintf( stderr, "   (not in environment variable %s, either)\n", env );
   exit(1);
}

void put_lit( const char *lit, size_t len )
{
    fwrite( lit, 1, len, server_w );
}

unsigned char get_8( void )
{
int x;

    x = fgetc( server_r );
    if (x == EOF)
    {
       protocol_error();
    }
    /*printf( "RECV %02x\n", x );*/
    return x;
}

unsigned short get_16( void )
{
unsigned short x;

    x = get_8() << 8;
    x += get_8();
    return x;
}

unsigned long get_32( void )
{
unsigned long x;

    x = get_8() << 24;
    x += get_8() << 16;
    x += get_8() << 8;
    x += get_8();
    return x;
}

signed char get_s8( void )
{
    return get_8();
}

signed short get_s16( void )
{
    return get_16();
}

signed long get_s32( void )
{
    return get_32();
}

char *get_n( size_t len )
{
   char *b = malloc( len + 1 );
   fread( b, 1, len, server_r );
   b[len] = 0;
   return b;
}


void put_8( unsigned char val )
{
    fputc( val, server_w );
}

void put_16( unsigned short val )
{
    fputc( val >> 8, server_w );
    fputc( val & 0xFF, server_w );
}

void put_32( unsigned long val )
{
    fputc( (val >> 26) & 0xFF, server_w );
    fputc( (val >> 16) & 0xFF, server_w );
    fputc( (val >> 8) & 0xFF, server_w );
    fputc( val & 0xFF, server_w );
}

void put_str( const char *str )
{
    put_16( strlen(str) );
    put_lit( str, strlen(str) );
}

void greet_server( void )
{
  char *temp = "CLI-1 (" CLIENT_TYPE ")\n";

  put_lit( temp, strlen(temp) );
}

void fix_top( void )
{
  struct argv_list *c = defined( "top" );
  static const char *wd;
  int max, max_is, i, j;

  if (!c)
    return;

  /* delete from pargs list */

  /* strip off the prefix top from pwd */

  wd = client_pwd;

  max = 0;
  for (i=0; i<c->num_values; i++)
    {
      for (j=0; wd[j] && c->values[i][j] && wd[j] == c->values[i][j]; j++);
      /* printf( "match '%s' to %d chars\n", c->values[i], j ); */
      if (j > max)
	{
	  max = j;
	  max_is = i;
	}
    }

  if (max > 1)
    {
      client_top = c->values[max_is];
      free_argv_list( c );
      wd += max;
      c = alloc_argv_list( "pwd", &wd );
      c->num_values = 1;
      /*  printf( "client top = '%s'\n", client_top ); */
    }
  else
    {
      client_top = NULL;
      free_argv_list( c );
    }
}

void import_arg_from_env( const char *name, const char *env_name, int req )
{
  static const char *(temps[20]);
  static int num_temps = 0;

  if (!defined(name))
    {
      const char *s;
      struct argv_list *c;

      s = getenv( env_name );
      if (s)
	{
	  temps[num_temps] = s;
	  c = alloc_argv_list( name, &temps[num_temps] );
	  c->num_values = 1;
	  num_temps++;
	}
      else if (req)
	missing( name, env_name );
    }
}

#define required_arg(n,e) import_arg_from_env(n,e,1)
#define optional_arg(n,e) import_arg_from_env(n,e,0)

int main( int argc, const char **argv )
{
  int i, j;
  struct argv_list *c;
  const char *logname;
  struct passwd *pw;
  char temp[1024];

   p_limit = pargs;
   
   getwd( temp );
   client_pwd = temp;

   /* parse the arguments */
   
  pw = getpwuid( getuid() );
  logname = pw->pw_name;

   c = alloc_argv_list( "logname", &logname );
   c->num_values = 1;

   c = alloc_argv_list( "program", argv );
   
   for (i=0; i<argc; i++)
   {
   const char *h = check_head(argv[i]);
   
      if (h)
      {
	  c = alloc_argv_list( h, argv+i+1 );
      }
      else
      {
          if (i == 0)
	  {
	   char *r = strrchr( argv[0], '/' );
	      if (r)
	         argv[i] = r + 1;
	    }
          c->num_values++;
      }
    }
    
    /* install some environment info */

    required_arg( "login", "SB_LOGIN" );
    required_arg( "server", "SB_SERVER" );

    optional_arg( "top", "SB_TOP" );
    optional_arg( "filespace", "SB_FILESPACE" );
    optional_arg( "group", "SB_GROUP" );

    fix_top();

#if 0
    for (c=pargs; c<p_limit; c++)
    {
       printf( "%s: %d => {", c->head, c->num_values );
       for (j=0; j<c->num_values; j++)
       {
          printf( "%s\"%s\"", j?", ":"", c->values[j] );
	}
	printf( "}\n" );
    }
#endif

    connect_to_server();
    free_argv_list( defined("server") );

    /*printf( "connected\n" );*/
    
    /* send the greeting */

    greet_server();
    
    /* send the initial request */
    
    put_8( p_limit - pargs );
    
    for (c=pargs; c<p_limit; c++)
    {
       put_str( c->head );
       put_16( c->num_values );

       for (j=0; j<c->num_values; j++)
       {
          put_str( c->values[j] );
	}
    }
    fflush( server_w );
    
    /* become a slave to the server */
    
    {
    char greet[100];
    
        /*printf( "getting server's response...\n" );*/
	fgets( greet, 100, server_r );
	/*printf( "server: %s\n", greet );*/
    }
    
    while (1)
    {
    char cmd;
    union cmd_arg carg[10];
    int i, j;
    
       cmd = get_8();
       /*printf( "Command %02x ('%c')\n", cmd, cmd );*/
       for (i=0; cmds[i].cmd; i++)
       {
          if (cmds[i].cmd == cmd)
	  {
	     for (j=0; cmds[i].args[j]; j++)
	     {
	        switch (cmds[i].args[j])
		{
		   case 's':  
			carg[j].str.len = get_8();
			carg[j].str.addr = get_n( carg[j].str.len );
			break;
		   case 'S':
			carg[j].str.len = get_16();
			carg[j].str.addr = get_n( carg[j].str.len );
			break;
		   case 'F':
			carg[j].str.len = get_32();
			carg[j].str.addr = get_n( carg[j].str.len );
			break;
		   case 'i':
		   	carg[j].val = get_s16();
			break;
		   case 'I':
		   	carg[j].val = get_s32();
			break;
		   default: protocol_error();
		}
	    }
	     if (get_8() != '.')
	       protocol_error();

	    cmds[i].proc( &carg[0] );
	    break;
	  }
	}
	if (!cmds[i].cmd)
	   protocol_error();
    }
    return 0;
}

#define BLOCKLEN (500)

void download_tar_cmd( union cmd_arg *argv )
{
  char temp[1200];
  FILE *f;

  printf( "tar: gzip'ed input is %u bytes\n", argv[0].str.len );

  if (client_top)
    sprintf( temp, "cd %s ; gunzip -c | tar -xvf -", client_top );
  else
    sprintf( temp, "gunzip -c | tar -xvf -" );

  f = popen( temp, "w" );

  if (!f)
    {
      perror( "gunzip|tar" );
    }
  else
    {
      int rc;

      fwrite( argv[0].str.addr, 1, argv[0].str.len, f );
      rc = pclose(f);
      if (rc != 0)
        {
          fprintf( stderr, "untar failed (exit code %d)\n", rc );
          exit(1);
        }
    }
}

void chmod_cmd( union cmd_arg *argv )
{
  char temp[1024];
  sprintf( temp, "%s/%s", client_top, argv[0].str.addr );
  if (chmod( temp, argv[1].val ))
    perror( temp );
}

void download_cmd( union cmd_arg *argv )
{
  char *path = argv[0].str.addr;
  char temp[1024];
  int mode;
  FILE *f;
  struct stat sb;

  mode = argv[1].val;

  if (path[0] == '/')
    path++;
  else
    protocol_error(); /* all paths from the server should be absolute */

  if (verbose)
    printf( "downloading: %s / %s\n", client_top, path );
  sprintf( temp, "%s/%s", client_top, path );

  if (stat(temp,&sb) == 0)
    {
      char bakup[1024];
      strcpy( bakup, temp );

      /*
       *  Check to make sure we have write access to the directory.
       *  Otherwise, it may be hard for the user to figure out why
       *  things aren't working!
       */
      strrchr( bakup, '/' )[0] = 0;
      if (access( bakup, W_OK ) != 0) {
        perror( bakup );
        goto download_failed;
      }

      strcat( bakup, "/." );
      strcat( bakup, strrchr( temp, '/' ) + 1 );

      if (verbose)
	printf( "renaming %s => %s\n", temp, bakup );
      if (rename( temp, bakup )) {
        perror( bakup );
	goto download_failed;
      }
    }

  f = fopen( temp, "w" );
  if (!f)
    goto download_failed;

  fwrite( argv[2].str.addr, 1, argv[2].str.len, f );

  if (fclose(f))
    goto download_failed;
  if (verbose)
    printf( "chmod %s: %o\n", temp, mode );
  chmod( temp, mode );

  put_8( 'd' );
  fflush( server_w );
  return;

 download_failed:
  {
    char *e = strerror(errno);
    fprintf( stderr, "%s failed: %s\n", temp, e );
    perror( temp );
    put_8( 'e' );
    put_str( e );
  }
  fflush( server_w );
}

void snarf_stdin_cmd( union cmd_arg *argv )
{
  char *buf = malloc(1000);
  size_t max = 1000, n = 0;
  int ch;

  while ((ch = getchar()) != EOF)
    {
      if (n >= max)
	{
	  max *= 2;
	  buf = realloc( buf, max );
	}
      buf[n++] = ch;
    }
  put_8( '-' );
  put_32( n );
  put_lit( buf, n );
  free(buf);
  fflush( server_w );
}

void upload_cmd( union cmd_arg *argv )
{
  char *path = argv[0].str.addr;
  FILE *f;
  size_t i, n, x;
  char temp[1024];
  struct stat sb;
  int rc;

  if (path[0] == '/')
    path++;
  else
    protocol_error(); /* all paths from the server should be absolute */

  if (verbose)
    printf( "uploading: %s / %s\n", client_top, path );
  sprintf( temp, "%s/%s", client_top, path );
  path = temp;

  f = NULL;
  rc = stat( path, &sb );
  if (rc != 0)
    {
      char *e = strerror(errno);
      perror( path );
      put_8( 'e' );
      put_str( e );
    }
  else if (S_ISDIR(sb.st_mode))
    {
      fprintf( stderr, "%s: is a directory\n", path );
      put_8( 'e' );
      put_str( "is a directory" );
    }
  else if ((f = fopen( path, "r" )) != NULL)
    {
      char temp[BLOCKLEN];

      fseek( f, 0, SEEK_END );
      n = ftell(f);
      if (verbose)
	printf( "uploading: %s (%u bytes)\n", path, n );
      fseek( f, 0, SEEK_SET );
      put_8( 'f' );
      put_16( sb.st_mode & 0777 );
      put_32( n );
      for (i=0; i<n; i+=x)
	{
	  x = fread( temp, 1, BLOCKLEN, f );
	  if (x == 0)
            {
	      fprintf( stderr, "%s: changed size during upload\n", path );
	      exit(2);
	    }
	  put_lit( temp, x );
	}
    }
  else
    {
      char *e = strerror(errno);
      perror( path );
      put_8( 'e' );
      put_str( e );
    }
    fflush( server_w );
}
