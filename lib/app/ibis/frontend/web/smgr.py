#

import md5, random, time, string
import shelve

SESSION_INDEX = "/tmp/session_index"

def longhash( str ):
   return string.join( map( lambda x: "%02x" % ord(x),
                            md5.new( str ).digest() ), '' )

LAST_SEED = (int((time.time()*1000)%1000000),
             longhash('smgr-%f-w0llaw0lla' % time.time()))

def crandom():
   global LAST_SEED
   s = "%s(%g,%.3f,%f(foo),)" % (LAST_SEED,
                                 random.random(),
                                 time.time(),
                                 random.random())
   LAST_SEED = (LAST_SEED[0]+1,longhash(s))
   return LAST_SEED[1]

def genid():
   db = shelve.open( SESSION_INDEX )
   while 1:
       h = crandom()
       x = string.atoi( h[0:4], 16 ) % 27
       id = h[x:][:5]
       if not db.has_key( id + '/*' ):
           db.close()
           return id

class Session:
   def __init__( self ):
      self.id = genid()
      self.user = None
      self.lasttime = time.time()
      self.server = ('localhost',5150)
      self._dirty = 1
      self.state = None
      self.peer = ''

   def getServer( self ):
      return "%s:%d" % self.server

   def _writeback( self ):
       if self._dirty:
           del self._dirty
           self.lasttime = time.time()
           db = shelve.open( SESSION_INDEX )
           db[self.id + '/' + self.peer] = self
           db[self.id + '/*'] = self.peer
           db.close()
           self._dirty = 0

   def setUser( self, user ):
      self.user = user
      self._dirty = 1

   def setState( self, state ):
       self.state = state
       self._dirty = 1
       
   def setServer( self, server ):
      h, p = string.split( server, ':' )
      self.server = (h, int(p))
      self._dirty = 1

   def clearUser( self, user ):
      self.user = None
      self._dirty = 1

def new_session( peer ):
    s = Session()
    s.peer = peer
    s._writeback()
    return s

def get_session( id, peer ):
    db = shelve.open( SESSION_INDEX )
    if db.has_key( id + '/' + peer ):
        v = db[id + '/' + peer]
        v._dirty = 0
    else:
        v = None
    db.close()
    return v
