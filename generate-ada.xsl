<!-- $Id: generate-ada.xsl,v 27e067a835e9 2001/07/13 18:44:32 simon $ -->
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

  <xsl:param name="generate-accessors"/>

  <!-- Generate the top-level package for the domain, then all the
       others. -->
  <xsl:template match="domain">

    <!-- Any context clause needed for top-level package .. -->
    <xsl:message>Generating domain context ..</xsl:message>
    <xsl:apply-templates mode="domain-context"/>

    <!-- .. the top-level package spec .. -->
    <xsl:message>.. top-level package spec ..</xsl:message>
    <xsl:text>package </xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text> is&#10;</xsl:text>

    <!-- .. any specially-declared types .. -->
    <xsl:message>.. any specially-declared types ..</xsl:message>
    <xsl:call-template name="domain-types"/>

    <!-- .. the Initialize procedure .. -->
    <xsl:message>.. the Initialize procedure ..</xsl:message>
    <xsl:text>  procedure Initialize;&#10;</xsl:text>

    <!-- .. and close. -->
    <xsl:text>end </xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>;&#10;</xsl:text>

    <!-- The top-level package body. -->
    <xsl:message>.. the top-level package body ..</xsl:message>
    <xsl:text>package body </xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text> is&#10;</xsl:text>
    <xsl:text>  procedure Initialize is separate;&#10;</xsl:text>
    <xsl:text>end </xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>;&#10;</xsl:text>

    <!-- The separate Initialize procedure body. -->
    <xsl:message>.. the separate Initialize procedure body ..</xsl:message>

    <xsl:variable
      name="initialize-procedures"
      select="class/operation[@initialize]"/>

    <xsl:for-each select="$initialize-procedures[parameter or @return]">
      <xsl:sort select="../name"/>
      <xsl:sort select="name"/>
      <xsl:message>
        <xsl:text>CF: bad "initialize" operation </xsl:text>
        <xsl:value-of select="../name"/>.<xsl:value-of select="name"/>
      </xsl:message>
    </xsl:for-each>

    <xsl:for-each select="$initialize-procedures">
      <xsl:sort select="../name"/>
      <xsl:sort select="name"/>
      <xsl:text>with </xsl:text>
      <xsl:value-of select="../../name"/>.<xsl:value-of select="../name"/>
      <xsl:text>;&#10;</xsl:text>
    </xsl:for-each>

    <xsl:text>separate (</xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>)&#10;</xsl:text>
    <xsl:text>procedure Initialize is&#10;</xsl:text>
    <xsl:text>begin&#10;</xsl:text>

    <xsl:choose>
      
      <xsl:when test="$initialize-procedures">
        <xsl:for-each select="$initialize-procedures">
          <xsl:sort select="../name"/>
          <xsl:sort select="name"/>
          <xsl:text>  </xsl:text>
          <xsl:value-of select="../name"/>
          <xsl:text>.</xsl:text>
          <xsl:value-of select="name"/>
          <xsl:text>;&#10;</xsl:text>
        </xsl:for-each>
      </xsl:when>

      <xsl:otherwise>
        <xsl:text>  null;&#10;</xsl:text>
      </xsl:otherwise>

    </xsl:choose>

    <xsl:text>end Initialize;&#10;</xsl:text>

    <!-- Any support packages for specially-declared types. -->
    <xsl:message>.. any support packages for specially-declared types ..</xsl:message>
    <xsl:apply-templates select="type" mode="domain-type-support"/>

    <!-- Package specs for individual classes. -->
    <xsl:message>.. package specs for individual classes ..</xsl:message>
    <xsl:apply-templates select="class" mode="class-spec">
      <xsl:sort select="name"/>
    </xsl:apply-templates>

    <!-- Package bodies for individual classes. -->
    <xsl:message>.. package bodies for individual classes ..</xsl:message>
    <xsl:apply-templates select="class" mode="class-body">
      <xsl:sort select="name"/>
    </xsl:apply-templates>

    <!-- Collection support packages. -->
    <xsl:message>.. Collection support packages ..</xsl:message>
    <xsl:apply-templates select="class" mode="collection-support">
      <xsl:sort select="name"/>
    </xsl:apply-templates>

    <!-- Package specs for Associations -->
    <xsl:message>.. package specs for Associations ..</xsl:message>
    <xsl:apply-templates select="association" mode="association-spec">
      <xsl:sort select="name"/>
    </xsl:apply-templates>

    <!-- Package bodies for Associations -->
    <xsl:message>.. package bodies for Associations ..</xsl:message>
    <xsl:apply-templates select="association" mode="association-body">
      <xsl:sort select="name"/>
    </xsl:apply-templates>

    <!-- Package specs for callbacks. -->
    <xsl:message>.. package bodies for Callbacks ..</xsl:message>
    <xsl:apply-templates select="type[@callback]" mode="callback-spec">
       <xsl:sort select="name"/>
   </xsl:apply-templates>

    <xsl:message>.. done.</xsl:message>

  </xsl:template>


  <!-- Catch unspecified default matches -->
  <xsl:template match="*"/>


</xsl:stylesheet>
