<?xml version="1.0"?>
<!DOCTYPE xsl:stylesheet [
  <!ENTITY nbsp "&#160;">
]>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="1.0">

<!-- Top-level conversion tables -->

<maps:dispositions
        xmlns:maps="http://www.rscheme.org/xml/app/ibis/disposition">
  <maps:disposition xml:lang="enUS" tag="">Undecided</maps:disposition>
  <maps:disposition xml:lang="enUS" tag="undecided">Undecided</maps:disposition>
  <maps:disposition xml:lang="enUS" tag="taken">Taken</maps:disposition>
  <maps:disposition xml:lang="enUS" tag="declined">Declined</maps:disposition>
  <maps:disposition xml:lang="enUS" tag="future">Future</maps:disposition>
</maps:dispositions>


<!-- Top-level parameters -->
<xsl:param name="timestamp">Right now</xsl:param>
<xsl:param name="sid"/>
<xsl:param name="withedit">no</xsl:param>

<xsl:template match="/issue">
  <xsl:call-template name="details">
    <xsl:with-param name="type">Issue </xsl:with-param>
  </xsl:call-template>
</xsl:template>

<xsl:template match="/position">
  <xsl:call-template name="details">
    <xsl:with-param name="type">Position </xsl:with-param>
  </xsl:call-template>
</xsl:template>

<xsl:template name="details" 
        xmlns:maps="http://www.rscheme.org/xml/app/ibis/disposition">
<xsl:param name="type"/>

<!-- Note, according to Tennison, p.6, and my reading of the XSLT spec
     on document(), we shouldn't need to do the /xsl:stylesheet/ step.
     4suite seems to want it
-->

<xsl:variable name="dispositions"
        select="document('')/xsl:stylesheet/maps:dispositions"/>

<html>
  <head>
    <title><xsl:value-of select="./@id"/>: <xsl:value-of select="./head/title"/></title>
  </head>
<body bgcolor="white">

<table width="100%" 
       cellspacing="0" cellpadding="0" border="0" 
       bgcolor="#CCCCCC">
  <tr>
    <td>
       <a href="logout?sid={$sid}">[Logout]</a>
       <a href="home?sid={$sid}">[Home]</a>
       <a href="{./@id}?sid={$sid}">[go]</a>
    </td>
  </tr>
</table>

<H1><xsl:value-of select="$type"/><xsl:value-of select="./@id"/>: <xsl:value-of select="./head/title"/></H1>

<table bgcolor="#cccccc" cellpadding="0" cellspacing="0" border="0">
<tr><td>
<table cellpadding="2" cellspacing="1" border="0">
  <tr>
    <td bgcolor="#eeffee" align="right">Id:</td>
    <td bgcolor="white"><xsl:value-of select="./@id"/></td>
  </tr>

  <tr>
    <td bgcolor="#eeffee" align="right">Title:</td>
    <td bgcolor="white"><xsl:value-of select="./head/title"/></td>
  </tr>

  <tr>
    <td bgcolor="#eeffee" align="right">Owner:</td>
    <td bgcolor="white"><xsl:apply-templates mode="long" select="./head/owner/user"/></td>
  </tr>

  <tr>
    <td bgcolor="#eeffee" align="right">Originator:</td>
    <td bgcolor="white"><xsl:apply-templates mode="long" select="./head/originator/user"/></td>
  </tr>

  <tr>
    <td bgcolor="#eeffee" align="right">Disposition:</td>
    <td bgcolor="white">
        <xsl:variable name="disp" select="string(./head/disposition)"/>
        <xsl:value-of 
             select="$dispositions/maps:disposition[ @tag = $disp ]"/>
    </td>
  </tr>
</table>
</td></tr></table>

<h3>Description</h3>
<p>
<xsl:apply-templates select="./description"/>
</p>

<h3>Notes</h3>
<xsl:apply-templates select="./notes"/>

<xsl:if test="$withedit = 'yes'">
  <hr/>
    <xsl:call-template name="edit">
      <xsl:with-param name="type" select="$type"/>
    </xsl:call-template>      
</xsl:if>

<hr/>
<h3>Add a Note to <xsl:value-of select="$type"/><xsl:value-of select="./@id"/></h3>

<form method="post" action="{./@id}?postnote=1&amp;sid={$sid}">
<table bgcolor="blue" cellpadding="0" cellspacing="0" border="0">
<tr><td>
<table cellpadding="2" cellspacing="1" border="0">
  <tr>
    <td bgcolor="#ffeeee" valign="top" align="right">Note Title:</td>
    <td bgcolor="white">
      <input name="title" size="55">
        <xsl:attribute name="value"></xsl:attribute>
      </input>
    </td>
  </tr>

  <tr>
    <td bgcolor="#ffeeee" valign="top" align="right">Remarks:<br/>
      <select name="mime">
        <option value="text/docbook">DocBook</option>
        <option value="text/html">HTML</option>
        <option value="text/plain">Plain</option>
      </select>
    </td>
    <td bgcolor="white">
      <textarea name="desc" rows="12" cols="55"></textarea>
    </td>
  </tr>

  <tr>
    <td colspan="2" bgcolor="white" align="right">
      <input type="submit" value="Add Note"/>
    </td>
  </tr>
</table>
</td></tr></table>
</form>
<hr/>
<font size="-1">Generated: <i><xsl:value-of select="$timestamp"/></i></font>

</body>
</html>
</xsl:template>

<xsl:template match="emphasis">
<i><xsl:apply-templates/></i>
</xsl:template>

<xsl:template match="audit">
<table width="100%" cellspacing="1" cellpadding="0" border="0">
  <tr>
    <td bgcolor="#FFFFDD" colspan="2">
      Update <xsl:value-of select="@id"/>
        performed by <b><xsl:apply-templates select="created/user" mode="short"/></b>
        on <i><xsl:value-of select="created/time"/></i>
    </td>
  </tr>
  <xsl:apply-templates select="./auditentry"/>

</table>
</xsl:template>

<xsl:template name="change-audit">
  <xsl:param name="what"/>
    <tr>
      <td colspan="2">
        <i>Changed <xsl:value-of select="$what"/>...</i>
      </td>
    </tr>
    <tr>
      <td width="10%" align="right">From: </td>
      <td><xsl:value-of select="arg[1]"/></td>
    </tr>
    <tr>
      <td width="10%" align="right">To: </td>
      <td><xsl:value-of select="arg[2]"/></td>
    </tr>
</xsl:template>

<xsl:template match="auditentry[@action='change-title']">
  <xsl:call-template name="change-audit">
    <xsl:with-param name="what">title</xsl:with-param>
  </xsl:call-template>
</xsl:template>

<xsl:template match="auditentry[@action='change-content']">
  <xsl:call-template name="change-audit">
    <xsl:with-param name="what">description</xsl:with-param>
  </xsl:call-template>
</xsl:template>

<xsl:template match="auditentry[@action='change-originator']">
  <xsl:call-template name="change-audit">
    <xsl:with-param name="what">originator</xsl:with-param>
  </xsl:call-template>
</xsl:template>

<xsl:template match="auditentry[@action='change-owner']">
  <xsl:call-template name="change-audit">
    <xsl:with-param name="what">owner</xsl:with-param>
  </xsl:call-template>
</xsl:template>

<xsl:template match="auditentry[@action='delete']">
  <tr> <td colspan="2"> <i>Deleted</i> </td> </tr>
</xsl:template>

<xsl:template match="auditentry[@action='recreate']">
  <tr>
    <td colspan="2">
      <i>Recreated</i>
    </td>
  </tr>
</xsl:template>


<xsl:template match="note">

<table width="100%" cellspacing="1" cellpadding="0" border="0">
  <tr>
    <td bgcolor="#FFFFDD">
      Note <xsl:value-of select="@id"/>
        by <b><xsl:apply-templates select="created/user" mode="short"/></b>
        on <i><xsl:value-of select="created/time"/></i>
    </td>
  </tr>
</table>
<xsl:variable name="title" select="title"/>

<xsl:if test="$title != 'untitled'">
<b><xsl:apply-templates select="title"/></b><br></br>
</xsl:if>

<xsl:choose>
  <xsl:when test="(@type = 'text/plain') or ($title = 'untitled')">
    <pre>
      <xsl:apply-templates select="description"/>
    </pre>
  </xsl:when>
  <xsl:otherwise>
    <p>
      <xsl:apply-templates select="description"/>
    </p>
  </xsl:otherwise>
</xsl:choose>
</xsl:template>

<xsl:template name="edit">
<xsl:param name="type"/>

<h3>Modify <xsl:value-of select="$type"/><xsl:value-of select="./@id"/></h3>
<form method="post" action="{./@id}?edit=item&amp;sid={$sid}">
<input type="hidden" name="title_hash" value="{md5(./head/title)}"/>
<input type="hidden" name="desc_hash" value="{md5(./description)}"/>
<input type="hidden" name="disp_orig" value="{./head/disposition}"/>
<input type="hidden" name="owner_orig" value="{./head/owner/user/login}"/>
<input type="hidden" name="originator_orig" value="{./head/originator/user/login}"/>

<table bgcolor="blue" cellpadding="0" cellspacing="0" border="0">
<tr><td>
<table cellpadding="2" cellspacing="1" border="0">
  <tr>
    <td bgcolor="#ffeeee" valign="top" align="right">Title:</td>
    <td bgcolor="white">
      <input name="title" size="55">
        <xsl:attribute name="value"><xsl:value-of select="./head/title"/></xsl:attribute>
      </input>
    </td>
  </tr>

  <tr>
    <td bgcolor="#ffeeee" valign="top" align="right">Description:</td>
    <td bgcolor="white">
      <textarea name="desc" rows="12" cols="55"><xsl:value-of select="./description"/></textarea>
    </td>
  </tr>

  <tr>
    <td bgcolor="#ffeeee" valign="top" align="right">Disposition:</td>
    <td bgcolor="white">
<select name="disp">
<option value="undecided">
  <xsl:if test="(./head/disposition = 'undecided')">
    <xsl:attribute name="SELECTED">1</xsl:attribute>
  </xsl:if>
  Undecided
</option>

<option value="taken">
  <xsl:if test="(./head/disposition = 'taken')">
    <xsl:attribute name="SELECTED">1</xsl:attribute>
  </xsl:if>
Taken
</option>

<option value="declined">
  <xsl:if test="(./head/disposition = 'declined')">
    <xsl:attribute name="SELECTED">1</xsl:attribute>
  </xsl:if>
Declined
</option>

<option value="future">
  <xsl:if test="(./head/disposition = 'future')">
    <xsl:attribute name="SELECTED">1</xsl:attribute>
  </xsl:if>
Future
</option>

</select>
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
      <input name="originator" size="12">
        <xsl:attribute name="value"><xsl:value-of select="./head/originator/user/login"/></xsl:attribute>
      </input>
    </td>
  </tr>

  <tr>
    <td colspan="2" bgcolor="white" align="right">
      <input type="submit" value="Commit"/>
    </td>
  </tr>
</table>
</td></tr></table>
</form>

</xsl:template>

<xsl:template match="user" mode="long">
<b>
<xsl:choose>
  <xsl:when test="./email">
    <a href="mailto:{./email}">
      <xsl:value-of select="./login"/>
    </a>
  </xsl:when>
  <xsl:otherwise>
    <xsl:value-of select="./login"/>
  </xsl:otherwise>
</xsl:choose>
</b> (<xsl:value-of select="./fullname"/>)
</xsl:template>

<xsl:template match="user" mode="short">
<xsl:value-of select="./login"/>
</xsl:template>

</xsl:stylesheet>

