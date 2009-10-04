import string
import bx

underscore_trans = string.maketrans( '_', '-' )

class IBISDatabase:
    def __init__( self, server ):
        self.server = server
        self.session = None

    def setSession( self, session ):
        self.session = session

    def new_klist( self, **opts ):
        if self.session:
            kl = { '*logname': self.session.user }
        else:
            kl = { '*logname': 'donovan' }
        for k, v in opts.items():
            if k[0] == '_':             # hack for python keywords
                k = k[1:]
            if '_' in k:
                k = string.translate( k, underscore_trans )
            kl[k] = v
        return kl
    
    def _rpc( self, klist ):
        s = bx.BXConnection( self.server )
        s.put_keylist( klist )
        s.slave_loop()
        s.close()
        if s.exitcode() == 0:
            return s.stdout()
        else:
            raise RuntimeError( s.stderr() )

    def edit( self, issue,
              new_title, new_desc, new_origin, new_owner, new_domain ):
        klist = self.new_klist( item = [], edit = issue )
        if new_title:
            klist['title'] = new_title
        if new_desc:
            klist['comment'] = new_desc
        if new_origin:
            klist['originator'] = new_origin
        if new_owner:
            klist['owner'] = new_owner
        if new_domain:
            klist['domain'] = new_domain
        import sys ; sys.stderr.write( "edit %s\n" % `klist` )
        self._rpc( klist )

    def mark( self, issue, key, value ):
        klist = self.new_klist( mark = issue )
        klist[key] = value
        self._rpc( klist )
                                
    def issue_info( self, issue ):
        klist = self.new_klist( _print = issue, raw = [] )
        return string.split( self._rpc( klist ), '|' )

    def issue_heritage( self, issue ):
        klist = self.new_klist( start = issue,
                                report = "heritage",
                                raw = [] )
        answer = []
        for l in string.split( self._rpc( klist ), '\n' ):
            if l:
                answer.append( string.split( l, '|' ) )
        return answer

    def issue_block( self, issue, deepq, verboseq ):
        klist = self.new_klist( view = issue, html_browser = [] )
        if deepq:
            klist['deep'] = []
        if not verboseq:
            klist['short'] = []
        return self._rpc( klist )

    def unescape( self, str ):
        return string.replace( str, "\\n", "\n" )

    def add_note( self, item, title, content, type ):
        klist = self.new_klist( note = item,
                                title = title,
                                comment = content,
                                mime_type = type )
        return self._rpc( klist )

    def followup( self, parent, type, title, content, disposition ):
        assert (type in ('specialize',
                         'respond',
                         'support',
                         'object-to',
                         'question'))
        klist = self.new_klist( title = title,
                                comment = content,
                                raw = [] )
        klist[type] = parent
        
        if disposition:
            klist['disposition'] = disposition

        return self._rpc( klist )

    def item_details_xml( self, item ):
        klist = self.new_klist( _print = item, xml = [] )
        return self._rpc( klist )

    def issue_block_xml( self, issue, deepq ):
        klist = self.new_klist( view = issue, xml = [] )
        if deepq:
            klist['deep'] = []
        return self._rpc( klist )
