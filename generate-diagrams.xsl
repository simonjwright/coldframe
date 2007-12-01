<!-- $Id: generate-diagrams.xsl,v 07f537247d38 2007/12/01 21:29:40 simonjwright $ -->

<!-- XSL stylesheet to generate HTML documentation. -->
<!-- Copyright (C) Simon Wright <simon@pushface.org> -->

<!--
     This program is free software; you can redistribute it and/or modify
     it under the terms of the GNU General Public License as published by
     the Free Software Foundation; either version 2, or (at your option)
     any later version.

     This program is distributed in the hope that it will be useful,
     but WITHOUT ANY WARRANTY; without even the implied warranty of
     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
     GNU General Public License for more details.

     You should have received a copy of the GNU General Public License
     along with this program; if not, write to the Free Software
     Foundation, Inc., 59 Temple Place - Suite 330,
     Boston, MA 02111-1307, USA.
     -->

<xsl:stylesheet
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:saxon="http://icl.com/saxon"
  extension-element-prefixes="saxon"
  version="1.1">

  <xsl:strip-space elements="*"/>

  <xsl:output method="text"/>


  <xsl:template match="domain">
    <xsl:for-each select="class[statemachine]">
      <xsl:call-template name="state-diagram"/>
      <xsl:variable name="filename">
        <xsl:value-of select="../name"/>.<xsl:value-of select="name"/>
      </xsl:variable>
      <xsl:text>${DOT:-dot} -Tpng -o</xsl:text>
      <xsl:value-of select="$filename"/>
      <xsl:text>.png </xsl:text>
      <xsl:value-of select="$filename"/>
      <xsl:text>.dot&#10;</xsl:text>
    </xsl:for-each>
  </xsl:template>


  <!-- Called at class to output the class's state diagram. -->
  <xsl:template name="state-diagram">
    <xsl:document href="{../name}.{name}.dot">
      digraph {
      edge [fontsize=10];
      node [shape=record, style=filled, fillcolor=moccasin, fontsize=10];
      <xsl:for-each select="statemachine/state">
        <xsl:sort select="not(@initial)"/>
        <xsl:sort select="name"/>
        <xsl:value-of select="name"/>
        <xsl:choose>
          <xsl:when test="@initial">
            <xsl:text>[shape=point, fillcolor=black, width=0.1];&#10;</xsl:text>
          </xsl:when>
          <xsl:when test="@final">
            <xsl:text>[shape=point, fillcolor=none, width=0.1];&#10;</xsl:text>
          </xsl:when>
          <xsl:otherwise>
            <xsl:text>[label="{</xsl:text>
            <xsl:value-of select="name"/>
            <xsl:if test="action">
              <xsl:text>|</xsl:text>
              <xsl:for-each select="action">
                <xsl:if test="position() &gt; 1">
                  <xsl:text>\n</xsl:text>
                </xsl:if>
                <xsl:value-of select="."/>
              </xsl:for-each>
            </xsl:if>
            <xsl:text>}"];&#10;</xsl:text>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:for-each>
      <xsl:for-each select="statemachine/transition">
        <xsl:value-of select="source"/>
        <xsl:text>-></xsl:text>
        <xsl:value-of select="target"/>
        <xsl:if test="event or action">
          <xsl:text>[label="</xsl:text>
          <xsl:if test="@ignore">
            <xsl:text>&#171;ignore&#187;\n</xsl:text>
          </xsl:if>
          <xsl:if test="@self">
            <xsl:text>&#171;self&#187;\n</xsl:text>
          </xsl:if>
          <xsl:value-of select="event"/>
          <xsl:if test="action">
            <xsl:text>\n/</xsl:text>
            <xsl:value-of select="action"/>
          </xsl:if>
          <xsl:text>"]</xsl:text>
        </xsl:if>
        <xsl:text>;&#10;</xsl:text>
      </xsl:for-each>
      }
    </xsl:document>
  </xsl:template>


</xsl:stylesheet>
