<?xml version="1.0"?>
<!DOCTYPE xsl:stylesheet [
  <!ENTITY nbsp "&#160;">
]>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="1.0">

<!-- Top-level parameters -->
<xsl:param name="timestamp">Right now</xsl:param>
<xsl:param name="sid"/>
<xsl:param name="user"/>
<xsl:param name="fuptype"/>

<xsl:template match="/*">

<html>
  <head>
    <title>Followup - <xsl:value-of select="$fuptype"/>: <xsl:value-of select="./@id"/></title>
  </head>
<body bgcolor="white">

<table width="100%" 
       cellspacing="0" cellpadding="0" border="0" 
       bgcolor="#CCCCCC">
  <tr>
    <td>
       <a href="logout?sid={$sid}">[Logout]</a>
       <a href="home?sid={$sid}">[Home]</a>
    </td>
  </tr>
</table>

<H1>Followup (<xsl:value-of select="$fuptype"/>)</H1>
(<xsl:value-of select="./@id"/>) <xsl:value-of select="./head/title"/>
<p/>

<form method="post" action="{./@id}?fup={$fuptype}&amp;sid={$sid}">

<table bgcolor="blue" cellpadding="0" cellspacing="0" border="0">
<tr><td>
<table cellpadding="2" cellspacing="1" border="0">
  <tr>
    <td bgcolor="#ffeeee" valign="top" align="right">Title:</td>
    <td bgcolor="white">
      <input name="title" size="55"/>
    </td>
  </tr>

  <tr>
    <td bgcolor="#ffeeee" valign="top" align="right">Description:</td>
    <td bgcolor="white">
      <textarea name="desc" rows="12" cols="55"></textarea>
    </td>
  </tr>

  <tr>
    <td bgcolor="#ffeeee" valign="top" align="right">Owner:</td>
    <td bgcolor="white">
      <input name="owner" size="12">
        <xsl:attribute name="value"><xsl:value-of select="./head/owner/user/login"/></xsl:attribute>
      </input>
    </td>
  </tr>

  <tr>
    <td bgcolor="#ffeeee" valign="top" align="right">Originator:</td>
    <td bgcolor="white">
      <input name="originator" size="12" value="{$user}"/>
    </td>
  </tr>

  <tr>
    <td colspan="2" bgcolor="white" align="right">
      <input type="submit" value="Submit"/>
    </td>
  </tr>
</table>
</td></tr></table>
</form>
</body>
</html>

</xsl:template>


</xsl:stylesheet>

