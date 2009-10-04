<?xml version="1.0"?>
<!DOCTYPE xsl:stylesheet [
  <!ENTITY nbsp "&#160;">
]>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="1.0">

<xsl:template match="/node">

<html>
  <head>
    <title>Listing of <xsl:value-of select="path"/></title>
  </head>

  <body bgcolor='white'>
    <h1>Listing of <xsl:value-of select="path"/>...</h1>
    <table border='1' cellpadding='2'>
        <xsl:for-each select="links/link">
          <tr>
          <xsl:choose>
            <xsl:when test="@type = 'dir'">
              <td>D</td>
              <td>
                <a href="{@name}/"><xsl:value-of select="@name"/>/</a>
              </td>
            </xsl:when>
            <xsl:otherwise>
              <td>L</td>
              <td>
                <a href="{@name}"><xsl:value-of select="@name"/></a>
              </td>
            </xsl:otherwise>
          </xsl:choose>
          <td>
            <xsl:value-of select="title"/>
          </td>
          <td>
            <i><xsl:value-of select="class"/></i>
          </td>
          </tr>
        </xsl:for-each>
    </table>
  </body>

</html>

</xsl:template>

</xsl:stylesheet>
