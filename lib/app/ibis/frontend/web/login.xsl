<?xml version="1.0"?>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="1.0">

<!-- Top-level parameters -->
<xsl:param name="timestamp">Right now</xsl:param>
<xsl:param name="sid"></xsl:param>
<xsl:param name="server">localhost:5150</xsl:param>

<xsl:template match="/">

<html>
  <head>
    <meta http-equiv='Content-Type' content='text/html; charset=iso-8859-1'/>
    <title>IBIS Login</title>
  </head>
  <body bgcolor='white'>
    <H1>IBIS Login</H1>
        (mode = <xsl:value-of select="/greeting/@mode"/>)

    <p>If you do not already have an IBIS account, enter your
       user name and password <q><code>guest</code></q></p>

    <xsl:if test="/greeting/@mode = 'failed'">
    <p>I didn't quite catch that... try again, please.</p>
    </xsl:if>

    <xsl:if test="/greeting/@mode = 'bye'">
    <p>Thank you for using the IBIS system.  Please visit again.</p>
    </xsl:if>

    <form method='post'>
      <xsl:attribute name='action'>login?sid=<xsl:value-of select="$sid"/></xsl:attribute>
      <input type="hidden" name="server" value="{$server}"/>
      <table border='0' bgcolor='blue' cellpadding='0' cellspacing='0'>
        <tr>
          <td>
            <table cellspacing='1' cellpadding='2' border='0'>
              <tr>
                <td bgcolor='#eeffee' align='right'>Login Name:</td>
                <td bgcolor='white'>
                  <input name='user' size='20'/>
                </td>
              </tr>
              <tr>
                <td bgcolor='#eeffee' align='right'>Password:</td>
                <td bgcolor='white'>
                  <input name='password' size='20'/>
                </td>
              </tr>
              <tr>
                <td bgcolor='white' colspan='2' align='right'>
                  <input type='submit' value='Log In'/>
                </td>
              </tr>
            </table>
          </td>
        </tr>
      </table>
      <xsl:if test="/greeting/@mode = 'redirect'">
        <input type="hidden" name="redirect" value="{./greeting/request}"/>
      </xsl:if>

    </form>
    <hr/>
    <font size='-1'>Generated: <i><xsl:value-of select="$timestamp"/></i></font>
  </body>
</html>

</xsl:template>
</xsl:stylesheet>

