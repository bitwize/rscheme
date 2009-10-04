import socket, struct, sys, os
from cStringIO import StringIO

class ExitRequest(Exception):
    def __init__( self, rc ):
        self.code = rc

    def __str__( self ):
        return "ExitRequest (rc %d)" % self.code

class DebugReader:
    def __init__( self, f ):
        self.__raw = f

    def read( self, n ):
        data = self.__raw.read(n)
        print "(read %d => %r)" % (n,data)
        return data

    def readline( self ):
        data = self.__raw.readline()
        print "(readline => %r)" % (data,)
        return data
        
class SourceBase:
    def __init__( self, host="127.0.0.1", port=2134,
                  login=None,
                  localuser=None ):
        sock = socket.socket( socket.AF_INET, socket.SOCK_STREAM )
        sock.connect( (host,port) )
        self.__socket = sock
        rcv = sock.makefile('r')
        #rcv = DebugReader( rcv )
        self.__rcv = rcv
        
        self.__greetServer()
        self.login = login or os.environ['USER']
        self.localuser = localuser or os.environ['USER']
        
    def __greetServer( self ):
        self.__putlit( "CLI-2 (python)\n" )

    def __receiveGreeting( self ):
        msg = self.__rcv.readline()
        print "SB Server => %r" % msg

    def __putlit( self, str ):
        self.__socket.send( str )

    def sendRequest( self, dict ):
        a = StringIO()
        a.write( struct.pack( "B", len(dict) ) )
        for key, values in dict.items():
            _putstr( a, key )
            a.write( struct.pack( "!H", len( values ) ) )
            for val in values:
                _putstr( a, val )
        pdu = a.getvalue()
        #print "pdu %r" % pdu
        self.__socket.send( pdu )
        self.__receiveGreeting()

    def __rcvDict( self ):
        # The received dict has the same structure
        # as the sent request
        r = self.__rcv
        (numKeys,) = struct.unpack( "!B", r.read(1) )
        dict = {}
        for i in range( numKeys ):
            key = self.__rcvArg( 'S' )
            dict[key] = self.__rcvArg( '*' )
        return dict

    def __rcvArg( self, spec ):
        r = self.__rcv
        if spec == '*':
            spec = r.read(1)

        if spec == 'i':
            return struct.unpack( "!H", r.read(2) )[0]
        elif spec == 'I':
            return struct.unpack( "!L", r.read(4) )[0]
        elif spec == 'R':
            return self.__rcvDict()
        elif spec == 'L':
            result = []
            (n,) = struct.unpack( "!H", r.read(2) )
            for i in range(n):
                result.append( self.__rcvArg('*') )
            return result
        
        if spec == 'F':
            (len,) = struct.unpack( "!L", r.read(4) )
        elif spec == 'S':
            (len,) = struct.unpack( "!H", r.read(2) )
        elif spec == 's':
            (len,) = struct.unpack( "!B", r.read(1) )
        else:
            raise RuntimeError, "Unknown arg spec %r" % spec
        return r.read( len )
                
    def processServer( self ):
        self.response = None
        while 1:
            cmd = self.__rcv.read(1)
            if self.CMD_DICT.has_key( cmd ):
                (methodName, argSpec) = self.CMD_DICT[ cmd ]
                args = []
                for a in argSpec:
                    args.append( self.__rcvArg( a ) )
                if self.__rcv.read(1) != '.':
                    raise RuntimeError, "Protocol Error"
                
                method = getattr( self, methodName )
                apply( method, args )
            else:
                raise RuntimeError, "Unknown cmd %r" % cmd

    #
    
    CMD_DICT = {
        'P': ('_err_print_cmd', "F"),
        'p': ('_std_print_cmd', "F"),
        'x': ('_exit_cmd', "i"),
        'R': ('_struct_response', "*")
        }

    def _exit_cmd( self, rc ):
        raise ExitRequest( rc )

    def _err_print_cmd( self, msg ):
        sys.stderr.write( "*** " )
        sys.stderr.write( msg ) 

    def _std_print_cmd( self, msg ):
        sys.stdout.write( msg )

    def _struct_response( self, dict ):
        if self.response is None:
            self.response = []
        self.response.append( dict )
    #

    def rpc( self, **dict ):
        dict['login'] = [self.login]
        dict['logname'] = [self.localuser]
        
        try:
            self.sendRequest( dict )
            self.processServer()
        except ExitRequest, xr:
            if xr.code == 0:
                return self.response
            else:
                raise

    def filespaceCreate( self, name, owner, group ):
        self.rpc( filesystem=[],
                  create=[],
                  name=[name],
                  owner=[owner],
                  group=[group] )
    
def _putstr( file, str ):
    file.write( struct.pack( "!H", len(str) )) 
    file.write( str )
    
def rpc( host="127.0.0.1", port=2134, **dict ):
    sb = SourceBase( host=host, port=port )
    apply( sb.rpc, (), dict )

def t():
    x=SourceBase( login='superuser' )
    x.filespaceCreate( "foo", "superuser", "world" )

"""
(make-database "/tmp/my.sto"            ; path to storage
               "myfam"                  ; database name
               "superuser"              ; initial user user-name
               "Super User"             ; initial user full-name
               "superuser@domain.foo"   ; initial user e-mail
               "donovan"                ; initial user login username
               "localhost.localdomain") ; initial user login hostname


(database-connect "/tmp/my.sto" 'update)
(server-daemon 2134)
"""

