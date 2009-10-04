<?xml version="1.0"?>
<!DOCTYPE xsl:stylesheet [
  <!ENTITY nbsp "&#160;">
]>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="1.0">

<!-- Top-level conversion tables -->

<!-- Top-level parameters -->

<xsl:param name="timestamp">Right now</xsl:param>
<xsl:param name="sid"/>
<xsl:param name="depth"/>
<xsl:param name="user"/>

<xsl:template match="/issue">
  <xsl:call-template name="nav"/>
</xsl:template>

<xsl:template name="nav">

<html>
  <head>
    <title>Issues from: <xsl:value-of select="./@id"/></title>
  </head>
  <body bgcolor="white">

    <table width="100%" 
           cellspacing="0" cellpadding="0" border="0" 
           bgcolor="#CCCCCC">
      <tr>
        <td>
           <a href="logout?sid={$sid}">[Logout <xsl:value-of select="$user"/>]</a>
           <a href="home?sid={$sid}">[Home]</a>
           <a href="{./@id}?sid={$sid}&amp;deep=1">[Deep]</a>
        </td>
      </tr>
      <xsl:for-each select="./ancestors/xref">
        <tr>
          <td>
            <table width="100%" 
                   cellspacing="0" cellpadding="0" border="0" 
                   bgcolor="#EEEEFF">
              <tr>
                <td align="right">
                  <xsl:attribute name="width">
                  <xsl:value-of select="position() * 30"/>
                  </xsl:attribute>
                  (<a href="{./@linkend}?sid={$sid}{$depth}"><xsl:value-of select="./@linkend"/></a>)
                </td>
                <td width="8">--</td>
                <td>
                  <xsl:value-of select="./title"/>
                </td>
              </tr>
            </table>
          </td>
        </tr>
      </xsl:for-each>
    </table>

    <h3>Table of Contents</h3>

    <table width="100%" 
           cellspacing="0" cellpadding="0" border="0">
      <xsl:apply-templates select="." mode="toc"/>
    </table>

    <h3>Tree</h3>
    <table width="100%" 
           cellspacing="0" cellpadding="0" border="0">
      <xsl:apply-templates select="." mode="tree"/>
    </table>
  </body>
</html>
</xsl:template>

<xsl:template name="tag">
  <code><b>
  <xsl:choose>
    <xsl:when test="name() = 'issue'">?</xsl:when>
    <xsl:when test="(name() = 'position') and (./head/disposition = 'taken')">#*</xsl:when>
    <xsl:when test="(name() = 'position') and (./head/disposition = 'declined')">#/</xsl:when>
    <xsl:when test="(name() = 'position')">#</xsl:when>
    <xsl:when test="(name() = 'argument') and (./@type = 'support')">+</xsl:when>
    <xsl:when test="(name() = 'argument') and (./@type = 'object')">-</xsl:when>
  </xsl:choose>
  </b></code>
</xsl:template>

<xsl:template match="issue|position|argument" mode="toc" priority="0">
  <xsl:param name="depth">0</xsl:param>
    <tr>
          <td>
            <table width="100%" 
                   cellspacing="0" cellpadding="0" border="0">
             <tr>
               <td valign="top" align="right">
                  <xsl:attribute name="width">
                    <xsl:value-of select="30 + $depth * 20"/>
                  </xsl:attribute>
                  <xsl:call-template name="tag"/>
                  &nbsp;
               </td>
               <td valign="top">
                  (<a href="#A{./@id}"><xsl:value-of select="./@id"/></a>)
                  &nbsp;
                  <xsl:value-of select="./head/title"/>
                  <xsl:if test="name() = 'issue'">
                    &nbsp;
                    <a href="{./@id}?sid={$sid}">[go]</a>
                  </xsl:if>
               </td>
            </tr>
           </table>
          </td>
    </tr>

  <xsl:apply-templates select="position" mode="toc">
    <xsl:with-param name="depth" select="$depth + 1"/>
  </xsl:apply-templates>

  <xsl:apply-templates select="argument" mode="toc">
    <xsl:with-param name="depth" select="$depth + 1"/>
  </xsl:apply-templates>

  <xsl:apply-templates select="issue" mode="toc">
    <xsl:with-param name="depth" select="$depth + 1"/>
  </xsl:apply-templates>
</xsl:template>

<xsl:template match="*" mode="toc" priority="-0.25">
</xsl:template>

<xsl:template match="issue|position|argument" mode="tree" priority="0">
  <xsl:param name="depth">0</xsl:param>
    <tr>
          <td>
            <table width="100%" 
                   cellspacing="0" cellpadding="0" border="0">
             <tr>
               <td valign="top" align="right">
                  <xsl:attribute name="width">
                    <xsl:value-of select="30 + ./@depth * 20"/>
                  </xsl:attribute>
                  <xsl:call-template name="tag"/>
               </td>
               <td valign="top" width="85">
                  &nbsp;
                  <a name="A{./@id}">
                    (<xsl:value-of select="./@id"/>)
                  </a>
                  &nbsp;
               </td>
               <td align="left">
                  <b><xsl:value-of select="./head/title"/></b>
                  <br/>
                  <xsl:value-of select="./description"/>
                  &nbsp;<a href="{./@id}?sid={$sid}&amp;detail=1">[D]</a>
                  <xsl:if test="name() = 'issue'">
                    <a href="{./@id}?sid={$sid}">[visit]</a>
                  </xsl:if>

                  <xsl:if test="name() = 'issue'">
                    <a href="{./@id}?fup=respond&amp;sid={$sid}">[#]</a>
                    <a href="{./@id}?fup=specialize&amp;sid={$sid}">[?]</a>
                  </xsl:if>

                  <xsl:if test="name() != 'issue'">
                    <a href="{./@id}?fup=question&amp;sid={$sid}">[?]</a>
                  </xsl:if>

                  <xsl:if test="name() = 'position'">
                    <a href="{./@id}?fup=support&amp;sid={$sid}">[+]</a>
                    <a href="{./@id}?fup=object-to&amp;sid={$sid}">[-]</a>
                  </xsl:if>

                  <a href="{./@id}?fup=note&amp;sid={$sid}">[note]</a>
               </td>
            </tr>
           </table>
          </td>
    </tr>
    <tr height="12"><td>&nbsp;</td></tr>

  <xsl:apply-templates select="position" mode="tree">
    <xsl:with-param name="depth" select="$depth + 1"/>
  </xsl:apply-templates>

  <xsl:apply-templates select="argument" mode="tree">
    <xsl:with-param name="depth" select="$depth + 1"/>
  </xsl:apply-templates>

  <xsl:apply-templates select="issue" mode="tree">
    <xsl:with-param name="depth" select="$depth + 1"/>
  </xsl:apply-templates>
</xsl:template>

</xsl:stylesheet>

