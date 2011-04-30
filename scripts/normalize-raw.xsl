<?xml version="1.0" encoding="utf-8"?>

<!-- 
     $Id$

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

  <xsl:template match="attributes">
    <xsl:copy>
      <xsl:copy-of select="@*"/>
      <xsl:for-each select="attribute">
        <xsl:sort select="name"/>
        <xsl:apply-templates select="."/>
      </xsl:for-each>
    </xsl:copy>
  </xsl:template>

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

  <xsl:template match="transitions">
    <xsl:copy>
      <xsl:copy-of select="@*"/>
      <xsl:for-each select="transition">
        <xsl:sort select="source"/>
        <xsl:sort select="target"/>
        <xsl:sort select="event"/>
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
