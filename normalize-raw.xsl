<?xml version="1.0" encoding="utf-8"?>

<!-- 
     $Id: normalize-raw.xsl,v 743eefbfd117 2008/09/28 19:45:20 simonjwright $

     XSL stylesheet to normalize ColdFrame's .raw format (eg, for
     review comparisons).

     Copyright (C) Simon Wright <simon@pushface.org> 
     -->

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="1.0">

  <xsl:output method="xml" encoding="iso-8859-1" indent="yes"/>
  <xsl:strip-space elements="*"/>
  
  <xsl:template match="classes">
    <xsl:copy>
      <xsl:copy-of select="@*"/>
      <xsl:for-each select="class">
        <xsl:sort select="name"/>
        <xsl:apply-templates select="."/>
      </xsl:for-each>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="documentationxxx"/>

  <xsl:template match="operations">
    <xsl:copy>
      <xsl:copy-of select="@*"/>
      <xsl:for-each select="operation">
        <xsl:sort select="name"/>
        <xsl:apply-templates select="."/>
      </xsl:for-each>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="events">
    <xsl:copy>
      <xsl:copy-of select="@*"/>
      <xsl:for-each select="event">
        <xsl:sort select="name"/>
        <xsl:apply-templates select="."/>
      </xsl:for-each>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="states">
    <xsl:copy>
      <xsl:copy-of select="@*"/>
      <xsl:for-each select="state">
        <xsl:sort select="name"/>
        <xsl:apply-templates select="."/>
      </xsl:for-each>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="relationships">
    <xsl:copy>
      <xsl:copy-of select="@*"/>
      <xsl:for-each select="association">
        <xsl:sort select="name"/>
        <xsl:apply-templates select="."/>
      </xsl:for-each>
      <xsl:for-each select="inheritance">
        <xsl:sort select="name"/>
        <xsl:sort select="child"/>
        <xsl:apply-templates select="."/>
      </xsl:for-each>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="*">
    <xsl:copy>
      <xsl:copy-of select="@*"/>
      <xsl:apply-templates/>
    </xsl:copy>
  </xsl:template>

</xsl:stylesheet>
