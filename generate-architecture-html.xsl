<!-- $Id: generate-architecture-html.xsl,v 3ea8d1ad069e 2001/03/25 09:40:06 simon $ -->
<!-- XSL stylesheet to generate HTML for the Architecture. -->
<!-- Copyright (C) Simon Wright <simon@pushface.org> -->

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="1.0">

  <xsl:strip-space elements="*"/>

  <xsl:output method="html"/>

  <xsl:template match="domain">
    <html>
      <head>
        <title>ColdFrame: <xsl:value-of select="name"/></title>
      </head>
      <body bgcolor="#FFFFFF">
        <h1><xsl:value-of select="name"/></h1>
        <xsl:text>&#10;</xsl:text>
        <xsl:apply-templates select="./documentation"/>
        <h2>Classes</h2>
        <xsl:apply-templates select="classes/class">
          <xsl:sort select="name"/>
        </xsl:apply-templates>
      </body>
    </html>
  </xsl:template>

  <xsl:template match="domain/classes/class">
    <xsl:variable name="name" select="name"/>
    <h3><i><a name="{translate($name, ' ', '_')}">
    <xsl:value-of select="$name"/></a></i></h3>
    <xsl:text>&#10;</xsl:text>
    <xsl:if test="../../relationships/inheritance[child=$name]">
      <xsl:variable
        name="parent"
        select="../../relationships/inheritance[child=$name]/parent"/>
      <xsl:text>Subtype of </xsl:text>
      <i><a href="#{translate($parent, ' ', '_')}">
      <xsl:value-of select="$parent"/></a></i>.
      <br/>
      <xsl:text>&#10;</xsl:text>
    </xsl:if>
    <xsl:apply-templates select="documentation"/>
  </xsl:template>

  <xsl:template match="b">
    <b><xsl:apply-templates/></b>
  </xsl:template>

  <xsl:template match="i">
    <i><xsl:apply-templates/></i>
  </xsl:template>

  <xsl:template match="tt">
    <tt><xsl:apply-templates/></tt>
  </xsl:template>

  <xsl:template match="br">
    <br/>
  </xsl:template>

</xsl:stylesheet>
