<!-- $Id: generate-ada.xsl,v 45d629840042 2001/05/11 19:13:35 simon $ -->
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
  <xsl:import href="ada-callback.xsl"/>
  <xsl:import href="ada-class.xsl"/>
  <xsl:import href="ada-collection.xsl"/>
  <xsl:import href="ada-operation.xsl"/>
  <xsl:import href="ada-type.xsl"/>
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
    <xsl:apply-templates select="type" mode="domain-type">
      <xsl:sort select="number(boolean(@record))"/>
      <xsl:sort select="name"/>
    </xsl:apply-templates>

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

    <!-- Package specs for callbacks. -->
    <xsl:apply-templates select="type[@callback]" mode="callback-spec"/>

    <!-- Package bodies for callbacks. -->
    <xsl:apply-templates select="type[@callback]" mode="callback-body"/>

  </xsl:template>


  <!-- Catch unspecified default matches -->
  <xsl:template match="*"/>


</xsl:stylesheet>
