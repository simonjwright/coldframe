<!-- $Id: generate-ada.xsl,v af1f91f97d0f 2001/10/10 04:49:01 simon $ -->
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
  <xsl:import href="ada-teardown.xsl"/>
  <xsl:import href="ada-type.xsl"/>
  <xsl:import href="ada-utilities.xsl"/>

  <xsl:strip-space elements="*"/>

  <xsl:output method="text"/>

  <!-- For identification info. -->
  <xsl:param name="coldframe-version"/>

  <!-- Controls how attribute accessor functions are generated. -->
  <xsl:param name="generate-accessors"/>

  <!-- Control indentation. -->
  <xsl:param name="standard-indent" select="'   '"/>
  <xsl:param name="continuation-indent" select="'  '"/>

  <!-- Control added blank lines: no or yes.. -->
  <xsl:param name="add-blank-lines" select="'yes'"/>

  <!-- Control verbosity: no or yes. -->
  <xsl:param name="verbose" select="'no'"/>


  <!-- Global shorthands for indentation. -->
  <xsl:param name="I" select="$standard-indent"/>
  <xsl:param name="II" select="concat($I, $I)"/>
  <xsl:param name="III" select="concat($II, $I)"/>
  <xsl:param name="IIII" select="concat($III, $I)"/>
  <xsl:param name="C" select="$continuation-indent"/>
  <xsl:param name="IC" select="concat($I, $C)"/>
  <xsl:param name="IIC" select="concat($II, $C)"/>
  <xsl:param name="IIIC" select="concat($III, $C)"/>
  <xsl:param name="IIIIC" select="concat($IIII, $C)"/>

  <!-- Added blank lines -->
  <xsl:param name="blank-line">
    <xsl:choose>
      <xsl:when test="$add-blank-lines='yes'">
        <xsl:text>&#10;</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:param>


  <!-- Generate the top-level package for the domain, then all the
       others. -->
  <xsl:template match="domain">

    <!-- Identification info -->
    <xsl:call-template name="progress-message">
      <xsl:with-param name="m" select="'Generating identification info ..'"/>
    </xsl:call-template>
    <xsl:call-template name="identification-info"/>
    
    <!-- Any context clause needed for top-level package .. -->
    <xsl:call-template name="progress-message">
      <xsl:with-param name="m" select="'.. domain context ..'"/>
    </xsl:call-template>
    <xsl:apply-templates mode="domain-context"/>

    <!-- .. the top-level package spec .. -->
    <xsl:call-template name="progress-message">
      <xsl:with-param name="m" select="'.. top-level package spec ..'"/>
    </xsl:call-template>
    <xsl:text>package </xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text> is&#10;</xsl:text>
    <xsl:value-of select="$blank-line"/>

    <!-- .. any specially-declared types .. -->
    <xsl:call-template name="progress-message">
      <xsl:with-param name="m" select="'.. any specially-declared types ..'"/>
    </xsl:call-template>
    <xsl:call-template name="domain-types"/>

    <!-- .. the Initialize procedure .. -->
    <xsl:call-template name="progress-message">
      <xsl:with-param name="m" select="'.. the Initialize procedure ..'"/>
    </xsl:call-template>
    <xsl:value-of select="$I"/>
    <xsl:text>procedure Initialize;&#10;</xsl:text>
    <xsl:value-of select="$blank-line"/>

    <!-- .. and close. -->
    <xsl:text>end </xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>;&#10;</xsl:text>

    <!-- The top-level package body. -->
    <xsl:call-template name="progress-message">
      <xsl:with-param name="m" select="'.. the top-level package body ..'"/>
    </xsl:call-template>
    <xsl:text>package body </xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text> is&#10;</xsl:text>
    <xsl:value-of select="$blank-line"/>
    <xsl:value-of select="$I"/>
    <xsl:text>procedure Initialize is separate;&#10;</xsl:text>
    <xsl:value-of select="$blank-line"/>
    <xsl:text>end </xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>;&#10;</xsl:text>

    <!-- The separate Initialize procedure body. -->
    <xsl:call-template name="progress-message">
      <xsl:with-param
        name="m"
        select="'.. the separate Initialize procedure body ..'"/>
    </xsl:call-template>

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
          <xsl:value-of select="$I"/>
          <xsl:value-of select="../name"/>
          <xsl:text>.</xsl:text>
          <xsl:value-of select="name"/>
          <xsl:text>;&#10;</xsl:text>
        </xsl:for-each>
      </xsl:when>

      <xsl:otherwise>
        <xsl:value-of select="$I"/>
        <xsl:text>null;&#10;</xsl:text>
      </xsl:otherwise>

    </xsl:choose>

    <xsl:text>end Initialize;&#10;</xsl:text>

    <!-- Any support packages for specially-declared types. -->
    <xsl:call-template name="progress-message">
      <xsl:with-param
        name="m"
        select="'.. any support packages for specially-declared types ..'"/>
    </xsl:call-template>
    <xsl:apply-templates select="type" mode="domain-type-support"/>

    <!-- Package specs for individual classes. -->
    <xsl:call-template name="progress-message">
      <xsl:with-param
        name="m"
        select="'.. package specs for individual classes ..'"/>
    </xsl:call-template>
    <xsl:apply-templates select="class" mode="class-spec">
      <xsl:sort select="name"/>
    </xsl:apply-templates>

    <!-- Package bodies for individual classes. -->
    <xsl:call-template name="progress-message">
      <xsl:with-param
        name="m"
        select="'.. package bodies for individual classes ..'"/>
    </xsl:call-template>
    <xsl:apply-templates select="class" mode="class-body">
      <xsl:sort select="name"/>
    </xsl:apply-templates>

    <!-- Collection support packages. -->
    <xsl:call-template name="progress-message">
      <xsl:with-param name="m" select="'.. Collection support packages ..'"/>
    </xsl:call-template>
    <xsl:apply-templates select="class" mode="collection-support">
      <xsl:sort select="name"/>
    </xsl:apply-templates>

    <!-- Package specs for Associations -->
    <xsl:call-template name="progress-message">
      <xsl:with-param name="m" select="'.. package specs for Associations ..'"/>
    </xsl:call-template>
    <xsl:apply-templates select="association" mode="association-spec">
      <xsl:sort select="name"/>
    </xsl:apply-templates>

    <!-- Package bodies for Associations -->
    <xsl:call-template name="progress-message">
      <xsl:with-param
        name="m"
        select="'.. package bodies for Associations ..'"/>
    </xsl:call-template>
    <xsl:apply-templates select="association" mode="association-body">
      <xsl:sort select="name"/>
    </xsl:apply-templates>

    <!-- Package specs for callbacks. -->
    <xsl:call-template name="progress-message">
      <xsl:with-param name="m" select="'.. package specs for Callbacks ..'"/>
    </xsl:call-template>
    <xsl:apply-templates select="type[@callback]" mode="callback-spec">
      <xsl:sort select="name"/>
    </xsl:apply-templates>
    
    <!-- Teardown -->
    <xsl:call-template name="progress-message">
      <xsl:with-param name="m" select="'.. Teardown procedures ..'"/>
    </xsl:call-template>
    <xsl:call-template name="domain-teardown"/>

    <xsl:call-template name="progress-message">
      <xsl:with-param name="m" select="'.. done.'"/>
    </xsl:call-template>

  </xsl:template>


  <!-- Called at domain to generate identification information. -->
  <xsl:template name="identification-info">
    <xsl:text>--  Domain revision: </xsl:text>
    <xsl:choose>
      <xsl:when test="revision">
        <xsl:value-of select="revision"/>        
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>unspecified</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:text>&#10;</xsl:text>
    <xsl:text>--  Extraction date: </xsl:text>
    <xsl:value-of select="date/day"/>
    <xsl:text> </xsl:text>
    <xsl:value-of select="date/month"/>
    <xsl:text> </xsl:text>
    <xsl:value-of select="date/year"/>
    <xsl:text>, </xsl:text>
    <xsl:value-of select="date/time"/>
    <xsl:text>&#10;</xsl:text>
    <xsl:text>--  Extractor: </xsl:text>
    <xsl:value-of select="extractor"/>
    <xsl:text>&#10;</xsl:text>
    <xsl:text>--  Normalizer: </xsl:text>
    <xsl:value-of select="normalizer"/>
    <xsl:text>&#10;</xsl:text>
    <xsl:text>--  Generator: </xsl:text>
    <xsl:value-of select="$coldframe-version"/>
    <xsl:text>&#10;</xsl:text>
  </xsl:template>


  <!-- Catch unspecified default matches -->
  <xsl:template match="*"/>


</xsl:stylesheet>
