<!-- $Id: generate-ada.xsl,v 1e6e1365bde4 2001/04/23 05:23:16 simon $ -->
<!-- XSL stylesheet to generate Ada code. -->
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

<xsl:stylesheet
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  version="1.0">

  <xsl:import href="ada-association.xsl"/>
  <xsl:import href="ada-attribute.xsl"/>
  <xsl:import href="ada-class.xsl"/>
  <xsl:import href="ada-collection.xsl"/>
  <xsl:import href="ada-operation.xsl"/>
  <xsl:import href="ada-utilities.xsl"/>

  <xsl:strip-space elements="*"/>

  <xsl:output method="text"/>


  <!-- Generate the top-level package for the domain, then all the
       others. -->
  <xsl:template match="domain">

    <!-- Any context clause needed for top-level package .. -->
    <xsl:apply-templates mode="domain-context"/>

    <!-- .. the top-level package spec .. -->
    <xsl:text>package </xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text> is&#10;</xsl:text>

    <!-- .. any specially-declared types .. -->
    <xsl:apply-templates select="type" mode="domain-type"/>

    <!-- .. and close. -->
    <xsl:text>end </xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>;&#10;</xsl:text>

    <!-- Any support packages for specially-declared types. -->
    <xsl:apply-templates select="type" mode="domain-type-support"/>

    <!-- Package specs for individual classes. -->
    <xsl:apply-templates select="class" mode="class-spec"/>

    <!-- Package bodies for individual classes. -->
    <xsl:apply-templates select="class" mode="class-body"/>

    <!-- Collection support packages. -->
    <xsl:apply-templates select="class" mode="collection-support"/>

    <!-- Child subprogram specs for individual operations. -->
    <xsl:apply-templates select="class/operation" mode="operation-spec"/>

    <!-- Child subprogram bodies for individual operations. -->
    <xsl:apply-templates select="class/operation" mode="operation-body"/>

    <!-- Package specs for Associations -->
    <xsl:apply-templates select="association" mode="association-spec"/>

    <!-- Package bodies for Associations -->
    <xsl:apply-templates select="association" mode="association-body"/>

  </xsl:template>


  <!-- Generate domain context clauses. -->
  <xsl:template mode="domain-context" match="domain/type">
    <xsl:choose>

      <xsl:when test="string/max">
      <!-- string/max implies an instantiation of Ada.Strings.Bounded -->
        <xsl:text>with Ada.Strings.Bounded;</xsl:text>
        <xsl:text> use Ada.Strings.Bounded;&#10;</xsl:text>
      </xsl:when>

    </xsl:choose>
  </xsl:template>


  <!-- Generate domain Types entries (not for standard types). -->
  <xsl:template mode="domain-type" match="domain/type">
    <xsl:if test="not(standard)">
      <xsl:choose>

        <xsl:when test="enumeration">
          <xsl:text>  type </xsl:text>
          <xsl:value-of select="name"/>
          <xsl:text> is </xsl:text>
          <xsl:text>&#10;    (</xsl:text>
          <xsl:for-each select="enumeration/literal">
            <xsl:value-of select="."/>
            <xsl:if test="position() &lt; last()">
              <xsl:text>,&#10;     </xsl:text>
            </xsl:if>
          </xsl:for-each>
          <xsl:text>);&#10;</xsl:text>
        </xsl:when>

        <xsl:when test="integer">
          <xsl:text>  type </xsl:text>
          <xsl:value-of select="name"/>
          <xsl:text> is range </xsl:text>
          <xsl:value-of select="integer/lower"/>
          <xsl:text> .. </xsl:text>
          <xsl:value-of select="integer/upper"/>
          <xsl:text>;&#10;</xsl:text>
        </xsl:when>

        <xsl:when test="real">
          <xsl:text>  type </xsl:text>
          <xsl:value-of select="name"/>
          <xsl:text> is digits </xsl:text>
          <xsl:value-of select="real/digits"/>
          <xsl:text> range </xsl:text>
          <xsl:value-of select="real/lower"/>
          <xsl:text> .. </xsl:text>
          <xsl:value-of select="real/upper"/>
          <xsl:text>;&#10;</xsl:text>
        </xsl:when>

        <!-- sets are implemented as class Collections; no action here -->
        <xsl:when test="set"/>

        <xsl:when test="string">
          <xsl:text>  package </xsl:text>
          <xsl:value-of select="name"/>
          <xsl:text>_Package is&#10;</xsl:text>
          <xsl:text>     new Generic_Bounded_Length (Max =&gt; </xsl:text>
          <xsl:value-of select="string/max"/>
          <xsl:text>);&#10;</xsl:text>
          <xsl:text>  subtype </xsl:text>
          <xsl:value-of select="name"/>
          <xsl:text> is </xsl:text>
          <xsl:value-of select="name"/>
          <xsl:text>_Package.Bounded_String;&#10;</xsl:text>
        </xsl:when>

        <xsl:otherwise>
          <xsl:text>  -- Unrecognised type category for </xsl:text>
          <xsl:value-of select="name"/>
          <xsl:text>&#10;</xsl:text>
        </xsl:otherwise>

      </xsl:choose>
    </xsl:if>
  </xsl:template>


  <!-- Generate domain Types support entries (not for standard types).
       We do these as child packages in case they're not actually 
       needed. -->
  <xsl:template mode="domain-type-support" match="domain/type">
    <xsl:if test="not(standard)">
      <xsl:choose>

        <xsl:when test="enumeration"/>

        <xsl:when test="integer"/>

        <xsl:when test="real"/>

        <xsl:when test="set"/>

        <xsl:when test="string">
          <xsl:text>with Architecture.String_Hash;&#10;</xsl:text>
          <xsl:text>function </xsl:text>
          <xsl:value-of select="../name"/>
          <xsl:text>.</xsl:text>
          <xsl:value-of select="name"/>
          <xsl:text>_Hash is&#10;</xsl:text>
          <xsl:text>   new Architecture.String_Hash.Bounded_Hash (</xsl:text>
          <xsl:value-of select="name"/>
          <xsl:text>_Package);&#10;</xsl:text>
        </xsl:when>

        <xsl:otherwise>
          <xsl:text>-- Unrecognised type category for </xsl:text>
          <xsl:value-of select="name"/>
          <xsl:text>&#10;</xsl:text>
        </xsl:otherwise>

      </xsl:choose>
    </xsl:if>
  </xsl:template>


  <!-- Catch unspecified default matches -->
  <xsl:template match="*"/>


  <!-- Catch unspecified mode="xxx" matches -->
  <xsl:template mode="domain-context" match="*"/>
  <xsl:template mode="domain-type" match="*"/>
  <xsl:template mode="domain-type-support" match="*"/>


</xsl:stylesheet>
