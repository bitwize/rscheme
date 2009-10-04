from xml.xpath import Conversions
import string, md5

def longhash( str ):
   return string.join( map( lambda x: "%02x" % ord(x),
                            md5.new( str ).digest() ), '' )

def StringMd5( context, str ):
   s = Conversions.StringValue( str )
   return longhash( s )

ExtFunctions = { ('', 'md5'): StringMd5 }
