try:
   from mod_python import apache
   from mod_python import util
except:
   print "*** COULD NOT LOAD APACHE MODULE ***"

from xml.xslt.Processor import Processor

from ConfigParser import ConfigParser
cfg = ConfigParser()
cfg.readfp( open( 'ibis.conf' ) )

class Family:
   def __init__( self, server ):
      self.server = server

def getFamily( family ):
   return Family( cfg.get( family, 'server' ) )

FAMILIES = { 'personnel': 'localhost:5151',
             'f117': 'localhost:5150',
             'ft': 'localhost:5160' }

import os.path

import string, time, md5, random, cPickle, base64, os, stat
import ibis
import stylex
from stylex import longhash
reload(ibis)
import smgr
reload(smgr)

nultrans = string.maketrans( '', '' )

def stripcr( str ):
   return string.translate( str, nultrans, '\r' )

P_CACHE = {}

def get_processor( req, name ):
   dir = os.path.split( req.filename )[0]
   fn = os.path.join( dir, name + ".xsl" )
   s = os.stat( fn )
   mtime = s[stat.ST_MTIME]
   if P_CACHE.has_key(fn):
      if mtime == P_CACHE[fn][0]:
         return P_CACHE[fn][1]
   #
   p = Processor()
   p.registerExtensionModules( ["stylex"] )
   p.appendStylesheetUri( fn )
   P_CACHE[fn] = ( mtime, p )
   return p

def right_now():
   return time.strftime( "%A, %B %d -- %H:%M:%S",
                         time.localtime( time.time() ) )

def applyProcessor( session, req, name, input, extras ):
   args = { 'timestamp': right_now(),
            'sid': session.id,
            'user': session.user }
   for k,v in extras.items():
      args[k] = v
   p = get_processor( req, name )
   return p.runString( input, topLevelParams = args )
   
def login_page( session, req, greeting ):
   r = applyProcessor( session, req, "login",
                       greeting,
                       { 'server': session.getServer() } )
   req.write( r )
   session._writeback()
   return apache.OK

def home_page( session, req, args ):
   r = applyProcessor( session, req, "home",
                       "<home><user><fullname>%s</fullname></user></home>" \
                       % session.user,
                       {} )
   req.write( r )
   session._writeback()
   return apache.OK
   
def style_handler( req, item, args ):
   try:
      return style_handler_inner( req, item, args )
   except:
      import sys, traceback
      sys.stderr.write( "*** FAILED IN style_handler() ***\n" )
      traceback.print_exc()
      req.content_type = 'text/html'
      #req.status = apache.HTTP_INTERNAL_SERVER_ERROR
      req.send_http_header()
      req.write( "<h1>Request Failed</h1>\n" )
      req.write( "<pre>\n" )
      traceback.print_exc( 30, req )
      req.write( "</pre>\n" )
      return apache.OK
      #return apache.HTTP_INTERNAL_SERVER_ERROR

def style_handler_inner( req, item, args ):
   peer = req.get_remote_host()
   if args.has_key( 'sid' ):
      s = smgr.get_session( args['sid'], peer )
      if not s:
         del args['sid']
         return style_handler_inner( req, item, args )
      import sys ; sys.stderr.write( "s.state = %s, peer = %s, item = %s\n" % (s.state, peer, item) )
      if s.state == 'unauth':
         if item == 'login':
            fs = util.FieldStorage( req, 1 )
            if (fs[ 'password' ] != 'foo') and (fs[ 'password' ] != 'guest'):
               return login_page( s, req, "<greeting mode='failed'/>" )

            s.setUser( fs[ 'user' ] )
            s.setState( 'ok' )
            if fs.has_key( 'redirect' ):
               x = cPickle.loads( base64.decodestring( fs['redirect'] ) )
               return do_redirect( s, req, "%s?sid=%s" % (x[0], s.id) )
               #return item_page( s, req, x[0], x[1] )
            return do_redirect( s, req, "home?sid=%s" % s.id )
         else:
            return login_page( s, req, "<greeting mode='whassup'/>" )
      elif s.state == 'ok':
         #import sys;sys.stderr.write( "[%s] args => %s\n" % (`item`,`args`) )
         if (item == 'home') or (item == 'login'):
            return home_page( s, req, args )
         elif item == 'logout':
            s.setState( 'unauth' )
            return login_page( s, req, "<greeting mode='bye'/>" )
         else:
            return item_page( s, req, item, args )
      elif s.state == 'anon':
         return issueblock_page( s, req, item, args )
      else:
         raise 'Unknown session state'
   elif item == 'login':
      s = smgr.new_session( peer )
      if args.has_key( 'server' ):
         s.setServer( args['server'] )
      s.setState( 'unauth' )
      return login_page( s, req, "<greeting mode='fresh'/>" )
   else:
      s = smgr.new_session( peer )
      if args.has_key( 'family' ):
         s.setServer( args['family'] )
      s.setState( 'unauth' )
      was = base64.encodestring( cPickle.dumps( (item, args) ) )
      m = "<greeting mode='redirect'><request>%s</request></greeting>" \
          % was
      return login_page( s, req, m )
                         
#      s.setState( 'anon' )
#      s.setUser( 'anon' )
#      return issueblock_page( s, req, item, args )

def redirect_item( session, req, item, args ):
   return do_redirect( session,
                       req,
                       "%s?sid=%s&detail=1&t=%f" % (item,
                                                    session.id,
                                                    time.time()) )
   
def annotate_item( session, req, item, args ):
   if req.method != "POST":
      raise 'Expected POST method'
   fs = util.FieldStorage( req )
   db = ibis.IBISDatabase( session.server )
   db.add_note( item,
                stripcr( fs['title'] ),
                stripcr( fs['desc'] ),
                fs['mime'] )
   return redirect_item( session, req, item, args )

def edit_item( session, req, item, args ):
   if req.method != "POST":
      raise 'Expected POST method'
   fs = util.FieldStorage( req, 1 )

   #import sys
   #fst = fs['title']
   #sys.stderr.write( "title       => %s\n" % `fst` )
   #sys.stderr.write( "H(title)    => %s\n" % longhash(fst) )
   #sys.stderr.write( "H(S(title)) => %s\n" % longhash( stripcr( fst ) ) )
   #sys.stderr.write( "H_saved     => %s\n" % fs['title_hash'] )
   #
   #fst = fs['desc']
   #sys.stderr.write( "title       => %s\n" % `fst` )
   #sys.stderr.write( "H(title)    => %s\n" % longhash(fst) )
   #sys.stderr.write( "H(S(title)) => %s\n" % longhash( stripcr( fst ) ) )
   #sys.stderr.write( "H_saved     => %s\n" % fs['desc_hash'] )

   new_title = None
   new_desc = None
   new_origin = None
   new_domain = None
   new_owner = None
   
   v = stripcr( fs['title'] )
   if longhash( v ) != fs['title_hash']:
      new_title = v

   v = stripcr( fs['desc'] )
   if longhash( v ) != fs['desc_hash']:
      new_desc = v

   if fs['originator'] != fs['originator_orig']:
      new_origin = fs['originator']

   if fs['owner'] != fs['owner_orig']:
      new_owner = fs['owner']

   db = ibis.IBISDatabase( session.server )
   db.edit( item, new_title, new_desc, new_origin, new_owner, new_domain )

   if fs['disp'] != fs['disp_orig']:
      db.mark( item, 'disposition', fs['disp'] )

   return redirect_item( session, req, item, args )

#apache.HTTP_UNAUTHORIZED
#apache.HTTP_MOVED_PERMANENTLY
#req.headers_out[ 'URI' ] =  foo 

def do_redirect( session, req, whereto ):
   loc = os.path.normpath( os.path.join( req.uri, whereto ) )
   req.full_uri = "http://%s%s" % (req.hostname, loc)
   session._writeback()
   req.headers_out[ 'Location' ] = whereto
   req.status = apache.HTTP_MOVED_PERMANENTLY
   req.send_http_header()
   return apache.HTTP_MOVED_PERMANENTLY

def item_page( session, req, item, args ):
   if args.has_key('edit'):
      what = args['edit']
      if what == 'item':
         return edit_item( session, req, item, args )
      else:
         raise 'bad edit type (%s)' % what

   if args.has_key('postnote'):
      return annotate_item( session, req, item, args )

   if args.has_key('fup'):
      return fup_page( session, req, item, args )
   elif args.has_key('detail'):
      return detail_page( session, req, item, args )
   else:
      return issueblock_page( session, req, item, args )

def issueblock_page( session, req, item, args ):
   if '.' in item:
      return do_redirect( session, req,
                          "%s?sid=%s" % (string.split( item, '.' )[0],
                                         session.id) )

   db = ibis.IBISDatabase( session.server )
   db.setSession( session )
   
   z = db.issue_block_xml( item, args.has_key('deep') )
   if args.has_key('deep'):
      depth_uri_flag = '&deep=1'
   else:
      depth_uri_flag = ''

   r = applyProcessor( session, req, "navigate",
                       z,
                       { 'withedit': 'yes',
                         'depth': depth_uri_flag } )
   req.write( r )
   return apache.OK

def fup_page( session, req, item, args ):
   fup = args['fup']
   db = ibis.IBISDatabase( session.server )
   db.setSession( session )
      
   if req.method == 'POST':
      fs = util.FieldStorage( req, 1 )
      if fup == 'note':
         db.add_note( item,
                      stripcr( fs['title'] ),
                      stripcr( fs['desc'] ),
                      'text/plain' )
      else:
         db.followup( item,
                      fup,
                      stripcr( fs['title'] ),
                      stripcr( fs['desc'] ),
                      None )
      iparent = string.split( item, '.' )
      item = iparent[0]
      return do_redirect( session, req,
                          "%s?sid=%s&t=%d" % (item, session.id, time.time()) )
   else:
      p = get_processor( req, "fup-specialize" )
      z = db.item_details_xml( item )
      r = p.runString( z, topLevelParams = { 'timestamp': right_now(),
                                             'sid': session.id,
                                             'user': session.user,
                                             'fuptype': fup } )
      req.write( r )
      return apache.OK

def detail_page( session, req, item, args ):

   p = get_processor( req, "detail" )
   db = ibis.IBISDatabase( session.server )
   db.setSession( session )
   
   z = db.item_details_xml( item )
   r = p.runString( z, topLevelParams = { 'timestamp': right_now(),
                                          'withedit': 'yes',
                                          'sid': session.id } )
   req.write( r )
   return apache.OK

# parse some "GET" parameters

def getparms( req ):            # XXX better parsing?
   options = {}
   if req.args:
      for o in string.split( req.args, "&" ):
         k, v = string.split( o, "=" )
         options[k] = v
   return options

def handler( req ):
   #import sys ; sys.stderr.write( "URI %s\n" % `req.uri` )
   rc = handler_(req)
   #sys.stderr.write( "RC  %s\n" % `rc` )
   return rc

def handler_( req ):
   z = string.split( req.path_info, '/' )
   req.full_uri = "http://%s%s" % (req.hostname, req.uri)
   #req.write( "        z => %s\n" % `z` )

   options = getparms( req )

   if (z[1] == 'xsl'):
      req.family = getFamily( z[2] )
      return style_handler( req, z[3], options )

   if z[1] == "echo":
      req.content_type = 'text/plain'
      req.write( "self => %s\n" % req.full_uri )
      req.write( "------------------\n" )
      for p in dir(req):
         req.write( "  %s => %s\n" % (p, getattr(req,p)) )
      return apache.OK
   
   req.content_type = 'text/html'
   req.send_http_header()


   if (len(z) == 2) and (z[0] == '') and z[1]:
      try:
         if options.has_key('fup'):
            if options.has_key('post'):
               followup_post_handler( req, z[1], options )
            else:
               followup_handler( req, z[1], options )
         elif options.has_key('edit'):
            edit_handler( req, z[1], options )
         elif options.has_key('post'):
            post_handler( req, z[1], options )
         else:
            view_handler( req, z[1], options )

      except RuntimeError, e:
         req.write( '<h1>Request Failed</h1>' )
         req.write( 'Failure:<br>\n' )
         req.write( '<pre>\n' )
         req.write( e.args[0] )
         req.write( '</pre>\n' )

   req.write( postamble() )
   return apache.OK

MAIN_SERVER = ('localhost', 5150)

def parent_links( req, issue ):
   db = ibis.IBISDatabase( MAIN_SERVER )
   n = 0
   req.write( '<table width="100%%" cellspacing="0" cellpadding="0" border="0" bgcolor="#eeeeff">\n' )
   for h in db.issue_heritage( issue ):
      req.write( '<tr>\n' )
      req.write( '<td><code>%s (<a href="%s">%s</a>)</code> ' \
                 % (("&nbsp;" * (n*2)),
                    h[0],
                    h[0]) )
      req.write( '%s</td>' % db.unescape( h[6] ) )
      req.write( '</tr>\n' )
      n = n + 1
   req.write( '</table>' )

def view_handler( req, issue, options ):
   db = ibis.IBISDatabase( MAIN_SERVER )
   deepq = options.has_key('deep')
   verboseq = options.has_key('verbose')
   content = db.issue_block( issue, deepq, verboseq )
   #subtitle = [ "<code>%s</code>" % req.path_info,
   #             "<code>%s</code>" % req.args ]
   subtitle = []
   
   if deepq:
      subtitle.append( '<a href="%s">[shallow view]</a>' % issue )
   else:
      subtitle.append( '<a href="%s?deep=1">[deep view]</a>' % issue )
      
   req.write( preamble( title = "Issue %s" % issue,
                        subtitle = subtitle ) )
   parent_links( req, issue )
   req.write( '<pre>\n' )
   req.write( content )
   req.write( '</pre>\n' )

def post_handler( req, issue, options ):
   db = ibis.IBISDatabase( MAIN_SERVER )
   fs = util.FieldStorage( req, 1 )
   #
   req.write( preamble( title = "Issue %s" % issue,
                        subtitle = [] ) )
   req.write( '<pre>\n' )
   req.write( '  disp = [%s]\n' % fs['disp'] )
   req.write( '  disp_orig = [%s]\n' % fs['disp_orig'] )
   nt = stripcr( fs['title'] )
   req.write( '  new title      = [%s]\n' % html_escape(nt) )
   req.write( '  new title hash = [%s]\n' % longhash(nt) )
   req.write( '  old title hash = [%s]\n' % fs['title_hash'] )
   req.write( '</pre>\n' )

   req.write( 'Processing actions:<br>\n' )
   req.write( '<hr>\n' )
   d = fs['disp']
   if d == "undecided":
      d = ""
   if d != fs['disp_orig']:
      req.write( "Set disposition to <b>%s</b>.<br>\n" % d )
      db.mark( issue, 'disposition', d )
   req.write( '<hr>\n' )
   req.write( '<a href="%s?edit=1">[Back to Edit]</a>&nbsp;&nbsp;' % issue )
   req.write( '<a href="%s">[View]</a><br>' % issue )

def followup_post_handler( req, item, options ):
   db = ibis.IBISDatabase( MAIN_SERVER )
   fs = util.FieldStorage( req, 1 )
   req.write( preamble( title = "Item %s Followup" % item,
                        subtitle = [] ) )
   d = fs['disp']
   if d == "undecided":
      d = ""
   #
   req.write( '<pre>\n' )
   new_title = string.strip( stripcr( fs['title'] ) )
   new_desc = string.strip( stripcr( fs['desc'] ) )
   
   req.write( '  Type = [%s]\n' % options['fup'] )
   req.write( '  Title = [%s]\n' % html_escape( new_title ) )
   req.write( '  Desc = [%s]\n' % html_escape( new_desc ) )
   req.write( '  Owner = [%s]\n' % fs['owner'] )
   req.write( '  Disposition: [%s]\n' % d )
   req.write( '</pre>\n' )
   req.write( '<hr>\n' )
   z = db.followup( item, options['fup'], new_title, new_desc, d )
   req.write( 'Opened new item <b>%s</b>\n' % z )
   req.write( '<hr>\n' )
   req.write( '<a href="%s?edit=1">[Edit New Item]</a>&nbsp;&nbsp;' % z )
   req.write( '<a href="%s">[View]</a><br>' % z )

longfup = { 'specialize': 'Specialization',
            'respond': "Response",
            'support': "Supporting Argument",
            'object-to': "Objecting Argument",
            'question': "Questioning Issue" }

def followup_handler( req, item, options ):
   db = ibis.IBISDatabase( MAIN_SERVER )
   info = db.issue_info( item )
   subtitle = []
   issue_owner = info[1]
   req.write( preamble( title = "Item %s %s" % (item, longfup[options['fup']]),
                        subtitle = subtitle ) )
   parent_links( req, item )

   if followup_text.has_key(options['fup']):
      msg = followup_text[options['fup']]
   else:
      msg = "Preparing a followup to: <code>(%(id)s)</code> %(title)s."
      
   req.write( msg % { 'title': db.unescape( info[3] ),
                      'id': item } )
   
   dir = os.path.split( req.filename )[0]
   form = open( os.path.join( dir, "followupform.html" ) ).read()
   fillin = { 'parent_id': item,
              'followup': options['fup'],
              'owner': info[1] }
   req.write( form % fillin )

def edit_handler( req, issue, options ):                # `issue' is a misnomer
   db = ibis.IBISDatabase( MAIN_SERVER )
   info = db.issue_info( issue )
   subtitle = []
   issue_owner = info[1]
   issue_disp = info[2]
   issue_title = db.unescape( info[3] )
   issue_desc = db.unescape( info[4] )
   subtitle.append( issue_title )
   req.write( preamble( title = "Issue %s" % issue,
                        subtitle = subtitle ) )
   dir = os.path.split( req.filename )[0]
   form = open( os.path.join( dir, "editform.html" ) ).read()
   fillin = { 'issue': issue,
              'title': html_escape( issue_title ),
              'desc': html_escape( issue_desc ),
              'owner': issue_owner,
              'is_undecided': '',
              'is_taken': '',
              'is_declined': '',
              'is_future': '',
              'disp': issue_disp,
              'title_hash': longhash( issue_title ),
              'desc_hash': longhash( issue_desc ) }

   if issue_disp:
      fillin['is_' + issue_disp] = 'SELECTED '
   else:
      fillin['is_undecided'] = 'SELECTED '
   
   req.write( form % fillin )

def html_escape( str ):
   for x,y in [("<","&lt;"), (">","&gt;")]:
      str = string.replace( str, x, y )
   return str

def preamble( **args ):
   subt = ""
   if args.has_key('subtitle'):
      for s in args['subtitle']:
         subt = subt + """<tr>
         <td bgcolor="#CCCCEE" height="1"><b>%s</b>
         </td>
         </tr>""" % s
         
   args['subtitle_rows'] = subt
         
   return """<html>
<head>
  <title>%(title)s</title>
</head>
<body bgcolor="white">
<table width="100%%" cellspacing="0" cellpadding="0" border="0" bgcolor="#333333">
  <tr>
    <td bgcolor="#EEEEEE">
    <font face="verdana" size=4>%(title)s</font>
    </td>
  </tr>
  %(subtitle_rows)s</table>
""" % args


def postamble():
   return """
<table width="100%%" cellspacing="1" cellpadding="0" border="0" bgcolor="#333333">
  <tr>
    <td bgcolor="#EEEEEE">
    <font face="verdana" size=3>Generated: %(time)s</font>
    </td>
  </tr>
</table>
</html>""" % { 'time': right_now()  }

followup_text = { 'object-to': """Preparing an objection to
(<code>%(id)s</code>):<br> %(title)s
<br><br>
An objection should state the kernel of the objection in the title,
and an explanation of the nature of the objection.""" }

