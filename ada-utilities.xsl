<!-- $Id: ada-utilities.xsl,v b3a78855955c 2001/07/07 14:12:52 simon $ -->
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

    <!-- The name of the type to be generated. -->
    <xsl:param name="type"/>

    <!-- The current class. -->
    <xsl:param name="class" select="/.."/>

    <xsl:choose>

      <!-- Autonumber maps to Integer. -->
      <xsl:when test="$type='Autonumber'">
        <xsl:text>Integer</xsl:text>
      </xsl:when>

      <!-- The current Class maps to just Handle. -->
      <xsl:when test="$type=$class/name">
        <xsl:text>Handle</xsl:text>
      </xsl:when>

      <!-- A Class (not the current class) maps to {class}.Handle. -->
      <xsl:when test="/domain/class/name=$type">
        <xsl:value-of select="$type"/>
        <xsl:text>.Handle</xsl:text>
      </xsl:when>

      <!-- Date maps to Time. -->
      <xsl:when test="$type='Date'">
        <xsl:text>Time</xsl:text>
      </xsl:when>

      <!-- Real maps to Float. -->
      <xsl:when test="$type='Real'">
        <xsl:text>Float</xsl:text>
      </xsl:when>

      <!-- Text maps to Unbounded_String. -->
      <xsl:when test="$type='Text'">
        <xsl:text>Unbounded_String</xsl:text>
      </xsl:when>

      <!-- Set (only works for class instances) maps to a Collection. -->
      <xsl:when test="/domain/type[name=$type]/set">
        <xsl:variable name="type-name" select="/domain/type[name=$type]"/>
        <xsl:if test="$type-name/set">
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
