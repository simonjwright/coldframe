<!-- XSL stylesheet to generate Ada code for Classes. -->
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

     As a special exception, when portions of this file are copied by a
     stylesheet processor into an output file, you may use that output
     file without restriction.
     -->

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="1.0">

  <!-- Generate tear-down of the whole Domain, intended to be used
       with AUnit. Called at domain. -->
  <xsl:template name="domain-teardown">

    <xsl:text>procedure </xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>.Tear_Down;&#10;</xsl:text>

    <xsl:for-each select="class">
      <xsl:sort select="name"/>

      <xsl:text>with </xsl:text>
      <xsl:value-of select="../name"/>
      <xsl:text>.</xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text>.Tear_Down;&#10;</xsl:text>

    </xsl:for-each>

    <xsl:text>procedure </xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>.Tear_Down is&#10;</xsl:text>
    <xsl:text>begin&#10;</xsl:text>

    <xsl:for-each select="class">
      <xsl:sort select="name"/>

      <xsl:value-of select="$I"/>
      <xsl:value-of select="name"/>
      <xsl:text>.Tear_Down;&#10;</xsl:text>

    </xsl:for-each>

    <xsl:text>end </xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>.Tear_Down;&#10;</xsl:text>

    <xsl:apply-templates mode="class-teardown-spec"/>
    <xsl:apply-templates mode="class-teardown-body"/>

  </xsl:template>


  <xsl:template mode="class-teardown-spec" match="domain/class">
    <xsl:text>procedure </xsl:text>
    <xsl:value-of select="../name"/>
    <xsl:text>.</xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>.Tear_Down;&#10;</xsl:text>
  </xsl:template>

  <xsl:template mode="class-teardown-spec" match="*"/>


  <xsl:template mode="class-teardown-body" match="domain/class">

    <xsl:text>procedure </xsl:text>
    <xsl:value-of select="../name"/>
    <xsl:text>.</xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>.Tear_Down is&#10;</xsl:text>
    <xsl:text>begin&#10;</xsl:text>

    <xsl:choose>

      <xsl:when test="@singleton">
        <xsl:value-of select="$I"/>
        <xsl:text>null;&#10;</xsl:text>
      </xsl:when>

      <xsl:otherwise>
        <xsl:value-of select="$I"/>
        <xsl:text>Maps.Clear (The_Container);&#10;</xsl:text>        
      </xsl:otherwise>

    </xsl:choose>

    <xsl:text>end </xsl:text>
    <xsl:value-of select="../name"/>
    <xsl:text>.</xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>.Tear_Down;&#10;</xsl:text>

  </xsl:template>

  <xsl:template mode="class-teardown-body" match="*"/>


</xsl:stylesheet>
