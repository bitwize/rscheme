<?xml version="1.0"?>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="1.0">

<!-- Top-level parameters -->
<xsl:param name="timestamp">Right now</xsl:param>
<xsl:param name="sid"/>

<xsl:template match="/">

<html>
  <head>
    <meta http-equiv='Content-Type' content='text/html; charset=iso-8859-1'/>
    <title>IBIS Home Page For <xsl:value-of select="/home/user/fullname"/></title>
  </head>
  <body bgcolor='white'>
    <H1>IBIS Home Page For <xsl:value-of select="/home/user/fullname"/></H1>
        <a href="logout">[Logout]</a>
    <hr/>
    <a href="1?sid={$sid}">Root Item</a>
    <hr/>
    <font size='-1'>Generated: <i><xsl:value-of select="$timestamp"/></i></font>
  </body>
</html>

</xsl:template>
</xsl:stylesheet>

