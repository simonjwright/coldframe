<!-- $Id: ada-utilities.xsl,v ddc0ed11feb8 2001/04/29 10:39:56 simon $ -->
<!-- XSL stylesheet, utilities to help generate Ada code. -->
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


  <!-- Handle special type name conversions. -->
  <xsl:template name="type-name">
    <xsl:param name="type"/>
    <xsl:choose>

      <!-- Date maps to Time. -->
      <xsl:when test="$type='Date'">
        <xsl:text>Time</xsl:text>
      </xsl:when>

      <!-- Text maps to Unbounded_String. -->
      <xsl:when test="$type='Text'">
        <xsl:text>Unbounded_String</xsl:text>
      </xsl:when>

      <!-- Class -->
      <xsl:when test="/domain/class/name=$type">
        <xsl:value-of select="/domain/name"/>
        <xsl:text>.</xsl:text>
        <xsl:value-of select="$type"/>
        <xsl:text>.Handle</xsl:text>
      </xsl:when>

      <!-- Set (only works for class instances) -->
      <xsl:when test="/domain/type[name=$type]/set">
        <xsl:variable name="type-name" select="/domain/type[name=$type]"/>
        <xsl:if test="$type-name/set">
          <xsl:value-of select="/domain/name"/>
          <xsl:text>.</xsl:text>
          <xsl:value-of select="$type-name/set"/>
          <xsl:text>.Collections.Collection</xsl:text>
        </xsl:if>
      </xsl:when>

      <xsl:otherwise>
        <xsl:value-of select="$type"/>
      </xsl:otherwise>

    </xsl:choose>
  </xsl:template>


</xsl:stylesheet>
