import socket, types, struct, string, cStringIO

class BXConnection:

   def __init__( self, server ):
      self.sock = socket.socket( socket.AF_INET, socket.SOCK_STREAM )
      self.sock.connect( server )

      hello = self.sock.recv(1024)
      
      print "HELLO => %s" % `hello`
      self.sock.send( "CLI-1 (python)\n" )

      self.slave_method_index = { 'P': (self.slave_err_print, "F"),
                                  'x': (self.slave_exit, "i"),
                                  'p': (self.slave_std_print, "F"),
                                  '-': (self.slave_snarf, "") }
      self._stderr = []
      self._stdout = []
      self._exitcode = None
      self._bulk = None

   def stdout( self ):
       return string.join( self._stdout, '' )

   def stderr( self ):
       return string.join( self._stderr, '' )

   def exitcode( self ):
       return self._exitcode

   def setbulk( self, text ):
       self._bulk = text

   def close( self ):
       self.sock.close()

   def slave_loop( self ):
       while 1:
           if not self.slave():
               return

   def slave( self ):
       cmd = self.get_8()
       print "COMMAND => 0x%02x (%c)" % (cmd, cmd)
       sm = self.slave_method_index[ chr(cmd) ]
       # collect the arguments
       arglist = []
       for argtype in sm[1]:
           if argtype in ('F', 's', 'S'):
               if argtype == 'F':
                   n = self.get_32()
               elif argtype == 's':
                   n = self.get_8()
               elif argtype == 'S':
                   n = self.get_16()
               print "  ARG[%s] (length %d)" % (argtype,n)
               arglist.append( self.get_n(n) )
           elif argtype == 'i':
               arglist.append( self.get_s16() )
               print "  ARG[%s] (value %d)" % (argtype,arglist[-1])
           elif argtype == 'I':
               arglist.append( self.get_s32() )
               print "  ARG[%s] (value %d)" % (argtype,arglist[-1])
           else:
               raise RuntimeError, "Protocol failure (bad type '%s')" % argtype
       tail = self.get_8()
       if tail != ord('.'):
           raise RuntimeError, "Protocol failure (tail is 0x%02x)" % tail
       return apply( sm[0], arglist )

   #------------------------------------------------------------

   def slave_exit( self, rc ):
       print "EXIT rc <%d>" % rc
       self._exitcode = rc
       return 0

   def slave_snarf( self ):
       print "SNARF"
       return 1

   def slave_std_print( self, text ):
       print "STD <%s>" % text
       self._stdout.append( text )
       return 1
       
   def slave_err_print( self, text ):
       print "ERROR <%s>" % text
       self._stderr.append( text )
       return 1

   #------------------------------------------------------------
   
   def get_n( self, n ):
      if n < 5000:
         a = self.sock.recv(n)
         while len(a) < n:
            more = self.sock.recv( n - len(a) )
            if not more:
               raise "Protocol error (socket closed)"
            a = a + more
         return a
      else:
         buf = cStringIO.StringIO()
         while n > 0:
            more = self.sock.recv( n )
            if not more:
               raise "Protocol error (socket closed)"
            buf.write( more )
            n = n - len(more)
         a = buf.getvalue()
         buf.close()
         return a

   def get_s8( self ):
       return struct.unpack( "b", self.sock.recv( 1 ) )[0]

   def get_8( self ):
       return ord( self.sock.recv( 1 ) )

   def get_16( self ):
       return struct.unpack( "!H", self.sock.recv(2) )[0]
       
   def get_32( self ):
       return struct.unpack( "!I", self.sock.recv(4) )[0]
   
   def get_s16( self ):
       return struct.unpack( "!h", self.sock.recv(2) )[0]
       
   def get_s32( self ):
       return struct.unpack( "!i", self.sock.recv(4) )[0]
   
   def put_lit( self, str ):
      self.sock.send( str )

   def put_8( self, n ):
      assert ((n >= 0) and (n < 256))
      self.sock.send( chr(n) )

   def put_16( self, n ):
      assert ((n >= 0) and (n < 65536))
      self.sock.send( chr(n >> 8) + chr(n & 0xFF) )

   def put_32( self, n ):
      self.sock.send( chr( (n >> 26) & 0xFF )
                      + chr( (n >> 16) & 0xFF )
                      + chr( (n >> 8) & 0xFF )
                      + chr( n & 0xFF ) )

   def put_str( self, str ):
      self.put_16( len(str) )
      self.put_lit( str )

   def put_keylist( self, dict ):
      self.put_8( len(dict) )
      for k, v in dict.items():
         self.put_str( k )
         if isinstance( v, types.ListType ):
            self.put_16( len(v) )
            for i in v:
               self.put_str( str(i) )
         else:
            self.put_16( 1 )
            self.put_str( str(v) )

def t():
    s = BXConnection( ('localhost', 5150) )
    s.put_keylist( { '*logname': 'donovan',
                     'view': 66,
                     'html': [] } )
    s.slave_loop()
    return s
