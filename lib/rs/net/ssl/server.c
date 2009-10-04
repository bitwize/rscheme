#include <assert.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <ctype.h>
#include <openssl/ssl.h>
#include <openssl/rand.h>
#include <openssl/err.h>
#include <netdb.h>

#include <sys/capability.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>

#include <arpa/inet.h>          /* depreciated inet_aton only! */

#define SEED_SIZE  (512)

static int framing_mode = 0;
static int status_fd = 2;
static int verbose = 0;

static unsigned read_frame( int fd );

#define CONTROL_TAG     (0xCC000000)
#define DATA_TAG        (0xDD000000)

#define TAG_MASK        (0xFF000000)
#define LENGTH_MASK     (0x0003FFFF)    /* 256K-1 max block size */

#define CTL_SHUTDOWN    ('c')
#define CTL_PASSWORD    ('p')

static unsigned char buf[LENGTH_MASK+1];

typedef unsigned u32;
 
void status( const char *msg, ... )
{
  char *bufptr, *buf, small[8192];
  va_list va;
  size_t n;
  int rc;

  buf = &small[0];
  n = sizeof(small);
  bufptr = buf;

  if (framing_mode) {
    bufptr += sizeof(u32);
    n -= sizeof(u32);         /* room for the header */
  } else {
    memcpy( &buf[0], "%[", 2 );
    bufptr = &buf[2];
    n -= 5;                             /* room for the "%[]%\n" */
  }

  va_start( va, msg );
  rc = vsnprintf( bufptr, n, msg, va );
  va_end( va );

  if (rc < 0) {
    perror( "vsnprintf" );
    exit( 2 );
  }
  bufptr += rc;

  if (rc > n) {
    rc = sprintf( buf, "status-overflow %d", rc );
  }

  if (status_fd >= 0) {

    if (framing_mode) {
      ((u32 *)buf)[0] = CONTROL_TAG | rc;
      write(status_fd, buf, rc + sizeof( u32 ));
    } else {
      memcpy( bufptr, "]%\n", 3 );
      write(status_fd, buf, rc + 5);
    }
  }
}

void status_errno( const char *msg )
{
  status( "error: %s: %s", msg, strerror(errno) );
}

void status_cert( const char *tag, X509 *cert )
{
  if (cert) {
    char tmp[1024];
    
    status( "%s %s", 
            tag,
            X509_NAME_oneline( X509_get_subject_name( cert ), 
                               tmp, sizeof(tmp) ) );
  } else {
    status( "%s -none-", tag );
  }
}

volatile void ssl_choke( int rc )
{
  char buf[200];

  while (1) {
    unsigned long err = ERR_get_error();
    if (!err) {
      break;
    }
    status( "ssl-err %d %s", rc, ERR_error_string( err, buf ) );
  }
  exit(rc);
}



static void loadrand( void )
{
  unsigned char buf[SEED_SIZE];
  int fd;

  fd = open( "/dev/urandom", O_RDONLY );
  read( fd, buf, SEED_SIZE );

  RAND_seed( &buf[0], SEED_SIZE );
  memset( &buf[0], 0, SEED_SIZE );
  close( fd );
}


#define BUFLEN 10

static void socks_setup( int fd, char *hf, int pf )
{
  char buffer[BUFLEN+1], *buf=buffer;
  int len = snprintf(buf, BUFLEN + 1,
                     "\x4\x1%c%c00010%s",
                     (char) ((pf & 0xff00) >> 8), /* port MSB */
                     (char) (pf & 0xff), /* port LSB */
                     hf);
  if( len >= BUFLEN ) {
    buf = alloca( len + 1 );
    snprintf(buf, len + 1,
             "\x4\x1%c%c00010%s",
             (char) ((pf & 0xff00) >> 8), /* port MSB */
             (char) (pf & 0xff), /* port LSB */
             hf);
  }
  buf[4]='\0';
  buf[5]='\0';
  buf[6]='\0';
  buf[7]='\1';
  buf[8]='\0';
  if (write( fd, buf, len+1 ) != len+1) {
    status_errno( "socks server connect request" );
    exit( 3 );
  }
  len = read( fd, buf, 8 );
  if( len != 8 || buf[0] != 0 ) {
    status_errno( "socks server connect" );
    exit( 3 );
  }
  if( buf[1] != (char) 90 ) {
    char *msg = "unknown code";
    switch( (int) buf[1] ) {
    case 91: msg = "request rejected or failed"; break;
    case 92: msg = "rejected; server cannot connect to identd on the client";
      break;
    case 93: msg = "rejected because the client program and identd report different user-ids";
      break;
    }
    status( "socks connect %s:%d %s", hf, pf, msg );
    exit( 3 );
  }
}

#define SMODE_CLIENT   (1)
#define SMODE_SERVER   (2)

int get_socket( const char *socks,      /* socks proxy spec */
                const char *addr,       /* local addr/port spec */
                const char *port,       /* remote addr/port spec (local in server mode) */
                int *mode )
{
  int rc;

  if (strncmp( port, "fdsrv:", 6 ) == 0) {
    *mode = SMODE_SERVER;
    return atoi( port+6 );
  } else if (strncmp( port, "fdclient:", 9 ) == 0) {
    *mode = SMODE_CLIENT;
    return atoi( port+9 );
  } else if (strncmp( port, "listen:", 7 ) == 0) {
    int fd = socket( PF_INET, SOCK_STREAM, 0 );
    struct sockaddr_in sa, peer;
    socklen_t peer_len;

    *mode = SMODE_SERVER;

    memset( &sa, 0, sizeof(sa) );
    sa.sin_family = AF_INET;
    sa.sin_port = htons( atoi( port+7 ) );
    sa.sin_addr.s_addr = INADDR_ANY;

    if (verbose) {
      status( "note: server on port <%d>", ntohs( sa.sin_port ) );
    }

    rc = bind( fd, (struct sockaddr *)&sa, sizeof(sa) );
    if (rc < 0) {
      status_errno( "bind" );
      exit(3);
    }

    rc = listen( fd, 3 );
    if (rc < 0) {
      status_errno( "listen" );
      exit(3);
    }

    peer_len = sizeof( peer );
    rc = accept( fd, (struct sockaddr *)&peer, &peer_len );
    if (rc < 0) {
      status_errno( "accept" );
      exit(3);
    }
    return rc;
  } else if (strncmp( port, "connect:", 8 ) == 0) {
    int fd = socket( PF_INET, SOCK_STREAM, 0 );
    struct sockaddr_in sa;
    struct in_addr in;
    /* socklen_t peer_len; */
    const char *x = strrchr( port + 8, ':' );
    char *h, *hf=NULL;
    struct hostent *e;
    int p, pf=0;

    *mode = SMODE_CLIENT;

    if (x) {
      h = alloca( x-(port+8) + 1 );
      memcpy( h, port+8, x-(port+8) );
      h[x-(port+8)] = 0;
      x++;
    } else {
      x = "443";        /* https */
      h = (char *)port+8;
    }
    if (verbose) {
      status( "note: client to <%s> port <%s>", h, x );
    }

    p = atoi( x );
    if ((p < 1) || (p > 65535)) {
      status( "error: bad port number <%s>", x );
      exit( 1 );
    }

    if (socks) {
      hf = h;
      pf = p;
      x = strrchr( socks, ':' );
      if (x) {
	h = alloca( x-socks + 1 );
	memcpy( h, socks, x-socks );
	h[x-socks] = 0;
	x++;
	p = atoi( x );
      } else {
	p = 9050;        /* socks */
	h = (char *)socks;
      }
    }

    memset( &sa, 0, sizeof(sa) );
    sa.sin_family = AF_INET;

    if(inet_aton(h, &in)) {
      sa.sin_addr = in;
    } else {
      e = gethostbyname( h );
      if (!e) {
	status( "error: host name resolution error on <%s>", h );
	exit( 3 );
      }

      if (!e->h_addr_list[0]) {
	status( "error: host name lookup did not provide address for <%s>", h );
	exit( 1 );
      }
      sa.sin_addr = *(struct in_addr *)e->h_addr_list[0];
    }


    sa.sin_port = htons( p );
    
    rc = connect( fd, (struct sockaddr *)&sa, sizeof(sa) );
    if (rc < 0) {
      status_errno( "connect" );
      exit( 3 );
    }

    if (socks != NULL) {
      socks_setup( fd, hf, pf );
    }

    return fd;
  } else {
    fprintf( stderr, "bad port spec: %s\n", port );
    exit( 1 );
  }
}

void ssl_loop( SSL *cnx, int sock, int in, int out );

static int passwd_cb( char *pass, int size, int rwflag, void *userdata )
{
  u32 hdr;
  unsigned i, n;

  if (rwflag) {
    return 0;
  }
  status( "passwd:%s", userdata );

  if (getenv( "PASS" )) {
    strcpy( pass, getenv( "PASS" ) );
    return strlen( getenv( "PASS" ) );
  }

  if (!framing_mode) {
    status( "error: can't get password in non-framing mode" );
    exit(2);
  }

  hdr = read_frame( 0 );
  
  if ((hdr & TAG_MASK) != CONTROL_TAG) {
    status( "error: can't do data while getting password" );
    exit(2);
  }

  n = hdr & LENGTH_MASK;

  if (n == 0) {
    status( "error: needed password" );
    exit(2);
  }

  if (buf[0] != CTL_PASSWORD) {
    status( "error: can't process non-CTL_PASSWORD now" );
    exit(2);
  }

  n--;
  if (n > size) {
    status( "error: actual password length %d > max length %d", n, size );
    exit(2);
  }
  
  buf[1+n] = 0;
  /*status( "password <%s>", buf+1 );*/
  for (i=0; i<n; i++) {
    unsigned char ch = buf[i+1];
    if (isupper( ch )) {
      pass[i] = 'A' + ('Z' - ch);
    } else if (islower( ch )) {
      pass[i] = 'a' + ('z' - ch);
    } else {
      pass[i] = ch;
    }
  }
  return n;
}


int main( int argc, char * const*argv )
{
  SSL_CTX *ctx;
  SSL *cnx;

  int rc, fd;
  const char *certfile = "server.pem";
  const char *pkeyfile = "server.pkey";
  const char *useport = "listen:443";
  const char *usesocks = NULL;
  const char *useaddr = NULL;

  int peer_cert_mode = 0;
  int op_mode;
  const char *ca = NULL;
  const char *trust = NULL;
  const char *jail = NULL;

  assert( sizeof(u32) == 4 );

  while (1) {
    switch (getopt( argc, argv, "c:p:k:FeEA:T:vqJ:i:s:" )) {
    case 'q':
      status_fd = -1;
      break;
    case 'J':
      jail = optarg;
      break;
    case 'v':
      verbose++;
      break;
    case 'c':
      certfile = optarg;
      break;
    case 'A':
      ca = optarg;
      break;
    case 'T':
      trust = optarg;
      break;
    case 'e':
      peer_cert_mode = 1;
      break;
    case 'E':
      peer_cert_mode = 2;
      break;
    case 'F':
      framing_mode = 1;
      break;
    case 'k':
      pkeyfile = optarg;
      break;
    case 'p':
      useport = optarg;
      break;
    case 'i':
      useaddr = optarg;
      break;
    case 's':
      usesocks = optarg;
      break;
    case '?':
      fprintf( stderr, "usage: %s [-c cert] [-k key] [-p port] [-i ip-addr] [-s socks4a-proxy]\n", argv[0] );
      exit( 1 );
    case -1:
      goto done;
    }
  }
 done:

  if (framing_mode) {
    status_fd = 1;
  }

  status( "sslmgr 1.0 pid=%d", getpid() );

  fd = get_socket( usesocks, useaddr, useport, &op_mode );

  SSL_load_error_strings();
  SSL_library_init();
  loadrand();

  if (op_mode == SMODE_CLIENT) {
    ctx = SSL_CTX_new( SSLv23_client_method() );
  } else {
    ctx = SSL_CTX_new( SSLv23_server_method() );
  }

  if (!ctx) {
    status( "error: SSL_CTX creation failed" );
    exit( 2 );
  }

  SSL_CTX_set_default_passwd_cb( ctx, passwd_cb );
  SSL_CTX_set_default_passwd_cb_userdata( ctx, "?!" );

  if (ca) {
    X509 *cert = NULL;
    FILE *f = fopen( ca, "r" );
    char tmp[1024];

    SSL_CTX_set_default_passwd_cb_userdata( ctx, "ca" );

    cert = PEM_read_X509( f, NULL, NULL, NULL );
    if (!cert) {
      ssl_choke( 23 );
    }

    if (verbose) {
      status( "note: ca is %s",
              X509_NAME_oneline( X509_get_subject_name( cert ), 
                                 tmp, sizeof(tmp) ) );
    }

    if (!SSL_CTX_add_client_CA( ctx, cert )) {
      ssl_choke( 22 );
    }
  }

  if (trust) {
    if (!SSL_CTX_load_verify_locations( ctx, trust, NULL )) {
      ssl_choke( 28 );
    }
  }

  SSL_CTX_set_default_passwd_cb_userdata( ctx, "client" );
  if (!SSL_CTX_use_certificate_chain_file( ctx, certfile )) {
    ssl_choke( 25 );
  }

  SSL_CTX_set_default_passwd_cb_userdata( ctx, "private" );
  if (!SSL_CTX_use_PrivateKey_file( ctx, pkeyfile, SSL_FILETYPE_PEM )) {
    ssl_choke( 26 );
  }

  SSL_CTX_set_default_passwd_cb_userdata( ctx, "?" );
  if (!SSL_CTX_check_private_key( ctx )) {
    ssl_choke( 27 );
  }

  if (peer_cert_mode) {
    /*
     *  Need to get my CAs figured out in order for this to work...
     */
    SSL_CTX_set_verify( ctx,
                        SSL_VERIFY_PEER |
                        (peer_cert_mode > 1 ? SSL_VERIFY_FAIL_IF_NO_PEER_CERT
                         : SSL_VERIFY_CLIENT_ONCE),
                        NULL );
  }

  cnx = SSL_new( ctx );
  if (!cnx) {
    status( "error: SSL creation failed" );
    exit( 2 );
  }

  rc = SSL_set_fd( cnx, fd );
  if (rc != 1) {
    ssl_choke( 2 );
  }

  /*
   *  We're about to start talking to the peer.  Drop
   *  all capabilities.
   */
  if (jail) {
    cap_t c;
    int rc;

    rc = chdir( jail );
    if (rc < 0) {
      perror( jail );
      exit(1);
    }

    if (getuid() == 0) {
      rc = chroot( jail );
      if (rc < 0) {
        perror( jail );
        exit(1);
      }
    }

    c = cap_init();
    rc = cap_clear( c );
    if (rc < 0) {
      perror( "cap_clear" );
      exit(1);
    }

    /* I'm not sure this does anything, since we don't have
     * any of the mentioned capabilities anyway...
     */
    rc = cap_set_proc( c );
    if (rc < 0) {
      perror( "cap_set_proc" );
      exit(1);
    }
    cap_free( c );
  }

  if (op_mode == SMODE_SERVER) {
    if (verbose) {
      status( "note: accepting" );
    }
    rc = SSL_accept( cnx );
  } else {
    if (verbose) {
      status( "note: connecting" );
    }
    rc = SSL_connect( cnx );
  }

  if (rc != 1) {
    int code = SSL_get_error( cnx, rc );
    status( "error: RC %d Error Code 0x%08x", rc, code );
    ssl_choke( 3 );
  }

  if (verbose) {
    if (framing_mode) {
      status( "note: ready (framing)" );
    } else {
      status( "note: ready (passthru)" );
    }
  }

  ssl_loop( cnx, fd, 0, 1 );

  if (verbose) {
    status( "note: bye" );
  }

  exit(0);
}

struct WriteBuffer {
  char *start, *end;
  char data[32768];
};


#if 0
static void request_client_cert( SSL *cnx )
{
  int rc;

  SSL_set_verify( cnx, SSL_VERIFY_PEER | SSL_VERIFY_CLIENT_ONCE, NULL );
  SSL_renegotiate( cnx );
  rc = SSL_do_handshake( cnx );
  if (verbose) {
    status( "note: renegotiate %d", rc );
  }
}
#endif

static unsigned read_frame( int fd )
{
  u32 hdr;
  int rc;
  unsigned size, offset;

  rc = read( fd, &hdr, sizeof(hdr));
  if (rc != sizeof(hdr)) {
    if (rc < 0) {
      status_errno( "frame header read" );
      exit(2);
    } else if (rc == 0) {
      status( "error: frame channel EOF" );
      exit(0);
    } else {
      status( "error: frame header short read" );
      exit(2);
    }
  }

  if (!(((hdr & TAG_MASK) == CONTROL_TAG) 
        || ((hdr & TAG_MASK) == DATA_TAG))) {
    status( "illegal frame header %08lx", hdr );
    exit(2);
  }

  size = hdr & LENGTH_MASK;
  offset = 0;

  while (offset < size) {
    rc = read( fd, buf + offset, size - offset );
    if (rc < 0) {
      status_errno( "frame data read" );
      exit(2);
    } else if (rc == 0) {
      status( "error: frame channel EOF (after header)" );
      exit(2);
    } else {
      offset += rc;
    }
  }
  return hdr;
}

static void report_peer_cert( SSL *cnx )
{
  static X509 *pc, *statused_cert = NULL;

  /*
   *  Report the new peer certificate if it's different than
   *  what we last reported
   */
  pc = SSL_get_peer_certificate( cnx );
  if (statused_cert != pc
      && SSL_get_verify_result( cnx ) == X509_V_OK) {
    status_cert( "peer-cert", pc );
    statused_cert = pc;
  }
  if (pc) {
    X509_free( pc );
  }
}

void ssl_close( SSL *cnx )
{
  int rc;

  rc = SSL_shutdown( cnx );

#if 0
  if (rc == 0) {
    /* we only sent the "close notify" alert... 
       haven't received theirs yet though
    */
    rc = SSL_shutdown( cnx );
    if (rc < 0) {
      ssl_choke( 5 );
    } else if (rc == 0) {
      status( "error: incomplete shutdown" );
    } else {
      status( "clean-shutdown" );
      exit( 0 );
    }
  } else if (rc == 1) {
    status( "clean-shutdown" );
    exit( 0 );
  } else {
    ssl_choke( 5 );
  }
#endif
}

void ssl_loop( SSL *cnx, int sock, int in, int out )
{
  int num_fd;

  num_fd = in;
  if (out > num_fd) {
    num_fd = out;
  }
  if (sock > num_fd) {
    num_fd = sock;
  }
  num_fd++;

  report_peer_cert( cnx );

  while (1) {
    char *p;
    int n;
    fd_set r, w;

    FD_ZERO( &r );
    FD_ZERO( &w );

    FD_SET( sock, &r );
    FD_SET( in, &r );

    if (SSL_want_write( cnx )) {
      FD_SET( sock, &w );
    }

    n = select( num_fd, &r, &w, NULL, NULL );

    if (n < 0) {
      status( "error: select errno %d", errno );
      exit(31);
    }

    if (verbose) {
      p = &buf[0];
      if (FD_ISSET( sock, &r )) {
        *p++ = '<';
      }
      if (FD_ISSET( sock, &w )) {
        *p++ = '>';
      }
      if (FD_ISSET( in, &r )) {
        *p++ = 'i';
      }
      if (FD_ISSET( out, &r )) {
        *p++ = 'o';
      }
      *p = 0;
      status( "note: select returns %d (%s)", n, buf );
    }

    if (FD_ISSET( sock, &r )) {
      n = SSL_read( cnx, buf, LENGTH_MASK );
      if (n > 0) {

        if (verbose) {
          status( "note: read chunk %d", n );
        }

        report_peer_cert( cnx );

        if (framing_mode) {
          u32 hdr = DATA_TAG | n;
          write( out, &hdr, sizeof(hdr) );
          write( out, buf, n );
        } else {
          write( out, buf, n );
        }
      } else if (n == 0) {
        if (SSL_get_shutdown( cnx ) & SSL_RECEIVED_SHUTDOWN) {
          status( "clean-shutdown" );
          exit( 0 );
        } else {
          int code = SSL_get_error( cnx, n );
          status( "error: read shutdown error (%d)", code );
          ssl_choke( 4 );
        }
      } else {
        status( "error: read error %d", n );
        ssl_choke( 4 );
      }
    }

    if (FD_ISSET( in, &r )) {
      int write_rc = 0;
      unsigned write_n = 0;

      if (framing_mode) {
        u32 hdr;

        hdr = read_frame( in );
        if ((hdr & TAG_MASK) == CONTROL_TAG) {
          unsigned i;

          for (i=0; i<(hdr & LENGTH_MASK); i++) {
            unsigned char ch = buf[i];

            switch (ch) {
            case CTL_SHUTDOWN:
              ssl_close( cnx );
              break;
            default:
              status( "error: illegal control code 0x%02x", ch );
              exit(2);
            }
          }
        } else {
          assert( (hdr & TAG_MASK) == DATA_TAG );
          write_n = hdr & LENGTH_MASK;
          write_rc = SSL_write( cnx, buf, write_n );
        }
      } else {
        n = read( in, buf, sizeof(buf) );
        if (n > 0) {
          write_n = n;
          write_rc = SSL_write( cnx, buf, write_n );
        } else if (n == 0) {
          if (verbose) {
            status( "note: EOF local" );
          }
          exit(0);
        }
      }
      
      if (write_rc < 0) {
        int code = SSL_get_error( cnx, write_rc );
        status( "error: read shutdown error (%d)", code );
        ssl_choke( 4 );
      }

      if (write_rc != write_n) {
        status( "error: only wrote %d of %u", write_rc, write_n );
      }
    }
  }
}

