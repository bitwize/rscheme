
(define-safe-glue (get-permit-blocking-reads?)
{
  REG0 = PERMIT_BLOCKING_READS ? TRUE_OBJ : FALSE_OBJ;
  RETURN1();
})

(define-safe-glue (fd-set-blocking (fd <raw-int>) (blocking_q <boolean>))
{
  int rc, flags;

  flags = fcntl(fd, F_GETFL);
  if (flags < 0) 
    {
      os_error( "fcntl/F_GETFL", 1, raw_fd );
    }

  if(EQ(blocking_q, FALSE_OBJ))
    {
      rc = fcntl(fd, F_SETFL, flags | O_NONBLOCK);
    } 
  else 
    {
      rc = fcntl(fd, F_SETFL, flags & ~O_NONBLOCK);
    }
  if (rc < 0)
    os_error( "fcntl/F_SETFL", 1, raw_fd );
  RETURN0();
})

;;;
;;;  this code is taken from Rob's `files.scm'
;;;


(define-safe-glue (tcdrain* (fd <raw-int>) (flush_which <raw-int>))
{ 
  static int q_flag[4] = { 0, TCIFLUSH, TCOFLUSH, TCIOFLUSH };
  obj ok = TRUE_OBJ;
  
  if (flush_which)	 
    {
      if (tcflush( fd, q_flag[flush_which] ) < 0) {
        if (errno == EINTR) {
          ok = FALSE_OBJ;
        } else {
          os_error( "tcflush", 2, raw_fd, raw_flush_which );
        }
      }
    }
  else
    {
      if (tcdrain( fd ) < 0) {
        if (errno == EINTR) {
          ok = FALSE_OBJ;
        } else {
          os_error( "tcdrain", 1, raw_fd );
        }
      }
    }
  REG0 = ok;
  RETURN1();
})

(define (tcdrain fd which)
  (let loop ()
    (if (not (tcdrain* fd which))
        (loop)
        (values))))
        

(define-class <terminal-state> (<object>) :bvec)

(define-safe-glue (terminal-set-attr (fd <raw-int>) (state <terminal-state>))
{
  struct termios *tiop = PTR_TO_DATAPTR( state );

  if (tcsetattr( fd, TCSANOW, tiop ) < 0)
    os_error( "tcsetattr", 1, raw_fd );
  RETURN0();
})

(define-safe-glue (terminal-get-attr (fd <raw-int>))
  literals: ((& <terminal-state>))
{
  obj attrs = alloc( sizeof( struct termios ), TLREF(0) );
  struct termios *tiop = PTR_TO_DATAPTR( attrs );

  if (tcgetattr( fd, tiop ) < 0)
    os_error( "tcgetattr", 1, raw_fd );
  REG0 = attrs;
  RETURN1();
})

(define-safe-glue (terminal-state-make-raw (based_on <terminal-state>))
{ 
  obj new_state = clone( based_on );
  struct termios *tiop = PTR_TO_DATAPTR( new_state );
  
  tiop->c_iflag &= ~(IGNBRK|BRKINT|PARMRK|ISTRIP|INLCR|IGNCR|ICRNL|IXON);
  tiop->c_oflag &= ~OPOST;
  tiop->c_lflag &= ~(ECHO|ECHONL|ICANON|ISIG|IEXTEN);
  tiop->c_cflag &= ~(CSIZE|PARENB);
  tiop->c_cflag |= CS8;

  /* is this AIX specific...? */

  tiop->c_cc[VMIN] = 1;   /* MIN characters to read before returning data   */
  tiop->c_cc[VTIME] = 0;  /* max TIME to wait before returning data (*0.1s) */

  REG0 = new_state;
  RETURN1();
})

(define (tcflush-flag flag)
  (case flag
    ((drain) 0)
    ((input) 1)
    ((output) 2)
    ((input&output) 3)
    (else (error "tcflush-flag: unrecognized value: ~s" flag))))

;;;

;;;  bits_sw is a key/value vectors mapping bit names (symbols or strings)
;;;  to #t, #f, or some other value

(define-class <termios-mask> (<object>) :bvec)

(define-safe-glue (compile-termios-mask (settings <vector>))
;  properties: ((other-hfiles "<termios.h>"  "<string.h>"))
  literals: ((& <termios-mask>))
{ 
  static struct {
     char *name;
     int   value;
     int   mask;
   } termios_intro[]
   = { { "ib:IGNBRK", IGNBRK, 0 },
       { "ib:BRKINT", BRKINT, 0 },
       { "ib:IGNPAR", IGNPAR, 0 },
       { "ib:PARMRK", PARMRK, 0 },
       { "ib:INPCK", INPCK, 0 },   /* input parity checking */
       { "ib:ISTRIP", ISTRIP, 0 }, /* strip bit 7 */
       { "ib:INLCR", INLCR, 0 },   /* xlate NL->CR on input */
       { "ib:IGNCR", IGNCR, 0 },   /* ignore CR on input */
       { "ib:ICRNL", ICRNL, 0 },   /* xlate CR->NL on input (unless IGNCR) */
       { "ib:IXON", IXON, 0 },     /* XON/XOFF on output */
       { "ib:IXOFF", IXOFF, 0 },   /* XON/XOFF on input */
       { "ob:OPOST", OPOST, 0 },   /* output-processing */
       { "ob:ONLCR", ONLCR, 0 },   /* NL->CR/NL on output */
#ifdef OCRNL
       { "ob:OCRNL", OCRNL, 0 },   /* CR->NL on output */
#endif
#ifdef ONOCR
       { "ob:ONOCR", ONOCR, 0 },   /* no CR in column 0 */
#endif
#ifdef ONLRET
       { "ob:ONLRET", ONLRET, 0 }, /* no CR */
#endif
       { "lb:ECHO", ECHO, 0 },     /* echo input */
       { "lb:ECHONL", ECHONL, 0 }, /* if ICANON, echo NL even if not ECHO */
       { "lb:ICANON", ICANON, 0 }, /* canonical mode (EOF, ERASE, KILL, etc) */
       { "lb:ISIG", ISIG, 0 },
       { "lb:IEXTEN", IEXTEN, 0 },
       { "cb:PARENB", PARENB, 0 },
       { "cb:CS8", CS8, CSIZE },
       { "cb:CS7", CS7, CSIZE },
       { "cb:CS6", CS6, CSIZE },
       { "cb:CS5", CS5, CSIZE },
       { "Cn:VMIN", VMIN, 0 },
       { "Cn:VTIME", VTIME, 0 },
       { NULL, 0, 0 } };

  obj mask = bvec_alloc( sizeof( struct termios ) * 2, TLREFB(0) );
  struct termios *tiop = PTR_TO_DATAPTR( mask );
  unsigned i, j;

  memset( tiop, 0, sizeof( struct termios ) * 2 );

  /* turn on bits */

  for (i=0; i<SIZEOF_PTR(settings); i+=SLOT(2))
    {
      obj key = gvec_ref( settings, i );
      obj val = gvec_ref( settings, i+SLOT(1) );
      const char *key_str = NULL;
      int tval = 0, tmask = 0;

      if (SYMBOL_P( key ))
	key_str = symbol_text( key );
      else if (STRING_P( key ))
	key_str = string_text( key );
      else
	scheme_error( "compile-termios-mask: wrong type key ~s", 1, key );

      for (j=0; termios_intro[j].name; j++)
	{
	  if (strcmp( termios_intro[j].name + 3, key_str ) == 0)
	    {
	      break;
	    }
	}
      if (!termios_intro[j].name)
	{
	  scheme_error( "compile-termios-mask: unrecognized bit ~s", 1, key );
	}
      tval = termios_intro[j].value;
      tmask = termios_intro[j].mask;
      if (!tmask)
        tmask = tval;

      switch (termios_intro[j].name[0])
	{
	case 'i':
	  if (truish( val ))
	    tiop[0].c_iflag |= tval;
	  tiop[1].c_iflag |= tmask;
	  break;
	case 'o':
	  if (truish( val ))
	    tiop[0].c_oflag |= tval;
	  tiop[1].c_oflag |= tmask;
	  break;
	case 'l':
	  if (truish( val ))
	    tiop[0].c_lflag |= tval;
	  tiop[1].c_lflag |= tmask;
	  break;
	case 'c':
	  if (truish( val ))
	    tiop[0].c_cflag |= tval;
	  tiop[1].c_cflag |= tmask;
	  break;
	case 'C':
	  tiop[0].c_cc[tval] = fx2int( val );
	  tiop[1].c_cc[tval] = ~0;
	  break;
	}
    }

  REG0 = mask;
  RETURN1();
})


(define-safe-glue (termios-apply-changes (based_on <terminal-state>)
					 (using_mask <termios-mask>))
;  properties: ((other-hfiles "<termios.h>"))
{
  obj new_state = clone( based_on );
  struct termios *using = PTR_TO_DATAPTR( using_mask );
  UINT_8 *src, *dest, *mask;
  unsigned i;

  dest = PTR_TO_DATAPTR( new_state );
  src = (UINT_8 *)(using + 0);
  mask = (UINT_8 *)(using + 1);

  for (i=0; i<sizeof( struct termios ); i++)
   {
     dest[i] = (dest[i] & ~mask[i]) + src[i];
   }

  REG0 = new_state;
  RETURN1();
})

(define (make-termios-raw-mask)
  (compile-termios-mask
   '#("IGNBRK" #f
      "BRKINT" #f
      "PARMRK" #f
      "ISTRIP" #f
      "INLCR" #f
      "IGNCR" #f
      "ICRNL" #f
      "IXON" #f
      "OPOST" #f
      "ECHO" #f
      "ECHONL" #f
      "ICANON" #f
      "ISIG" #f
      "IEXTEN" #f
      "PARENB" #f
      "CS8" #t
      "VMIN" 1
      "VTIME" 0)))

