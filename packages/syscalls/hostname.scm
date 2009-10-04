#|------------------------------------------------------------*-Scheme-*--|
 | File:	    packages/syscalls/hostname.scm
 |
 |          Copyright (C)1998 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |	    See <http://www.rscheme.org/> for the latest info.
 |
 | File version:     1.4
 | File mod date:    2003-11-17 08:59:10
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  syscalls
 |
 | Purpose:          Provide host name services; lookup, etc
 `------------------------------------------------------------------------|#

(define-class <resolver-error> (<condition>)
  (resolver-query type: <object> #|<string> or <inet-addr>|#)
  (error-code type: <fixnum>))

(define (gethostbyname (hostname <string>))
  (bind ((addrs names (gethostinfo hostname)))
    (car addrs)))

(define (gethostbyaddr (hostaddr <inet-addr>))
  (bind ((addrs names (gethostinfo hostaddr)))
    (car names)))

;; takes host names and IP addresses

(define (string->hostaddr (hostspec <string>))
  (if (char-numeric? (string-ref hostspec 0))
      (string->inet-addr hostspec)
      (gethostbyname hostspec)))

(define-syscall-glue (gethostinfo query)
  literals: ((& <resolver-error>) (& <inet-addr>))
{
  struct hostent *h = NULL;

  if (STRING_P(query))
    {
      h = gethostbyname( (char *)string_text(query) );
    }
  else if (OBJ_ISA_PTR(query) && (EQ(CLASSOF_PTR(query),TLREF(1))))
    {
      h = gethostbyaddr( PTR_TO_DATAPTR(query), SIZEOF_PTR(query), AF_INET );
    }
  else
    {
      scheme_error( "gethostinfo: ~s invalid", 1, query );
    }

  if (h)
    {
      obj name_list, last_name_pair;
      obj addr_list, last_addr_pair;
      char **alias, **addr;

      name_list = cons( make_string( h->h_name ), NIL_OBJ );
      last_name_pair = name_list;
      for (alias=h->h_aliases; *alias; alias++)
	{
	  obj n;
	  
	  n = cons( make_string( *alias ), NIL_OBJ );
	  gvec_write_fresh_ptr( last_name_pair, SLOT(1), n );
	  last_name_pair = n;
	}

      addr_list = NIL_OBJ;
      last_addr_pair = NIL_OBJ;
      for (addr=h->h_addr_list; *addr; addr++)
	{
	  obj a = bvec_alloc( h->h_length, TLREF(1) );
	  obj n = cons( a, NIL_OBJ );
	  memcpy( PTR_TO_DATAPTR(a), *addr, h->h_length );

	  if (EQ(last_addr_pair,NIL_OBJ))
	    addr_list = n;
	  else
	    gvec_write_fresh_ptr( last_addr_pair, SLOT(1), n );
	  last_addr_pair = n;
	}

      REG0 = addr_list;
      REG1 = name_list;
    }
  else
    {
      obj hcode;

      switch (h_errno)
	{
	case HOST_NOT_FOUND: hcode = int2fx(1); break;
	case NO_ADDRESS:     hcode = int2fx(2); break;
	case NO_RECOVERY:    hcode = int2fx(3); break;
	case TRY_AGAIN:      hcode = int2fx(4); break;
	default:             hcode = int2fx(0); break;
	}
      raise_error( make3( TLREF(0), NIL_OBJ, query, hcode ) );
    }
  RETURN(2);
})

(define-method display-object ((self <resolver-error>) port)
  (format port "resolver error for ~s: ~a\n"
	  (resolver-query self)
	  (vector-ref '#("(unknown)"
			 "Host not found"
			 "No address"
			 "No recovery"
			 "Try again")
		      (error-code self))))
