<!-- $Id: ada-attribute.xsl,v a2be55a7c0f6 2005/02/18 06:08:54 simon $ -->
<!-- XSL stylesheet to generate Ada code for Attributes. -->
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

     As a special exception, when portions of this file are copied by
     a stylesheet processor into an output file, this file does not by
     itself cause the resulting file to be covered by the GNU General
     Public License.  This exception does not however invalidate any
     other reasons why the output file might be covered by the GNU
     Public License.
     -->

<xsl:stylesheet
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:at="http://pushface.org/coldframe/attribute"
  xmlns:ut="http://pushface.org/coldframe/utilities"
  version="1.0">


  <!-- Called from domain/class to generate the actual identifier
       record for the class. -->
  <xsl:template name="at:identifier-record">
    <xsl:choose>

      <!-- Output only identifier attributes. -->
      <xsl:when test="count(attribute[@identifier]) &gt; 0">
        <xsl:value-of select="$I"/>
        <xsl:text>type Identifier is record&#10;</xsl:text>
        <xsl:apply-templates
          mode="at:instance-record-component"
          select="attribute[@identifier]"/>
        <xsl:value-of select="$I"/>
        <xsl:text>end record;&#10;</xsl:text>
      </xsl:when>

      <xsl:otherwise>
        <!-- XXX how can there be an identifier witout attributes? -->
        <xsl:value-of select="$I"/>
        <xsl:text>type Identifier is null record;&#10;</xsl:text>
      </xsl:otherwise>

    </xsl:choose>
  </xsl:template>


  <!-- Called from domain/class to generate the actual instance
       record for the class. -->
  <xsl:template name="at:instance-record">

    <xsl:choose>

      <xsl:when test="attribute[not(@class)] or @active or statemachine">
        <!-- There are instance attributes; output them all. -->

        <xsl:value-of select="$I"/>
        <xsl:choose>
          <xsl:when test="statemachine">
            <xsl:text>type Instance is new ColdFrame.Project.Events.Instance_Base with record&#10;</xsl:text>

          </xsl:when>
          <xsl:otherwise>
            <xsl:text>type Instance is new ColdFrame.Instances.Instance_Base with record&#10;</xsl:text>
          </xsl:otherwise>
        </xsl:choose>

        <xsl:if test="@active">
          <xsl:value-of select="$II"/>
          <xsl:text>The_T : T_P;&#10;</xsl:text>
        </xsl:if>

        <!-- The non-supertype attributes -->
        <xsl:apply-templates
          mode="at:instance-record-component"
          select="attribute"/>
        <xsl:variable name="parent-name" select="name"/>

        <!-- supertype attributes -->
        <xsl:for-each select="../inheritance[parent=$parent-name]">
          <xsl:sort select="name"/>
          <xsl:value-of select="$II"/>
          <xsl:value-of select="name"/>
          <xsl:text>_Current_Child : </xsl:text>
          <xsl:value-of select="name"/>
          <xsl:text>_Child;&#10;</xsl:text>
        </xsl:for-each>

        <!-- state machine status -->
        <xsl:if test="statemachine">
          <xsl:value-of select="$II"/>
          <xsl:text>Old_State_Machine_State : State_Machine_State_T := </xsl:text>
          <xsl:value-of select="statemachine/state[@initial]/name"/>
          <xsl:text>;&#10;</xsl:text>
          <xsl:value-of select="$II"/>
          <xsl:text>State_Machine_State : State_Machine_State_T := </xsl:text>
          <xsl:value-of select="statemachine/state[@initial]/name"/>
          <xsl:text>;&#10;</xsl:text>
        </xsl:if>

        <xsl:value-of select="$I"/>
        <xsl:text>end record;&#10;</xsl:text>

      </xsl:when>

      <xsl:otherwise>
        <xsl:value-of select="$I"/>
        <xsl:text>type Instance is new ColdFrame.Instances.Instance_Base with null record;&#10;</xsl:text>
      </xsl:otherwise>

    </xsl:choose>
  </xsl:template>


  <!-- Generate the individual components of the class identifier
       or instance record. -->
  <xsl:template
    match="attribute[not(@class)]"
    mode="at:instance-record-component">
    <xsl:call-template name="at:single-record-component">
      <xsl:with-param name="indent" select="$II"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template mode="at:instance-record-component" match="*"/>


  <!-- Generate get specs. -->
  <xsl:template
    match="class/attribute[not(@class)]"
    mode="at:attribute-get-spec">

    <xsl:choose>

      <xsl:when test="@refers">

        <!-- We need to avoid generating an accessor that the user has
             specified (only likely to occur for [[formalizes]]
             attributes). -->

        <xsl:variable name="operation-name">
          <xsl:text>Get_</xsl:text>
          <xsl:call-template name="at:attribute-name"/>
        </xsl:variable>

        <xsl:if test="not(../operation[name=$operation-name])">

          <xsl:if test="not(@role='Parent')">
            <xsl:value-of select="$I"/>
            <xsl:text>--  Private use only, use navigation operations&#10;</xsl:text>
          </xsl:if>

          <xsl:call-template name="at:attribute-get-header"/>
          <xsl:text>;&#10;</xsl:text>

          <!--
          <xsl:value-of select="$I"/>
          <xsl:text>pragma Inline_Always (</xsl:text>
          <xsl:value-of select="$operation-name"/>
          <xsl:text>);&#10;</xsl:text>
          -->

          <xsl:value-of select="$blank-line"/>

        </xsl:if>

      </xsl:when>

      <xsl:when test="$generate-accessors='yes'">
        <xsl:call-template name="at:attribute-get-header"/>
        <xsl:text>;&#10;</xsl:text>
        <xsl:value-of select="$blank-line"/>
      </xsl:when>

    </xsl:choose>

  </xsl:template>

  <xsl:template mode="at:attribute-get-spec" match="*"/>


  <!-- Called at class/attribute to generate an attribute "get"
       accessor heading (no semicolon). -->
  <xsl:template name="at:attribute-get-header">

      <xsl:value-of select="$I"/>
      <xsl:text>function Get_</xsl:text>
      <xsl:call-template name="at:attribute-name"/>
      <xsl:text>&#10;</xsl:text>

      <!-- If this isn't a singleton, we need a handle parameter -->
      <xsl:choose>
        <xsl:when test="not(../@singleton)">
          <xsl:value-of select="$IC"/>
          <xsl:text>(This : Handle) return </xsl:text>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="$IC"/>
          <xsl:text>return </xsl:text>
        </xsl:otherwise>
      </xsl:choose>

      <xsl:call-template name="at:attribute-type"/>

  </xsl:template>


  <!-- Generate set specs (non-identifier attributes only). -->
  <xsl:template
    match="class/attribute[not(@identifier) and not(@class)]"
    mode="at:attribute-set-spec">

    <xsl:if test="@refers or $generate-accessors='yes'">

      <!-- Set procedure, with comment for generated associative referential
           attributes -->

      <xsl:if test="@refers">
        <xsl:variable name="rel" select="@relation"/>
        <xsl:if test="/domain/association/name=$rel and not(name)">
          <xsl:value-of select="$I"/>
          <xsl:text>--  Private use only, use Link/Unlink&#10;</xsl:text>
        </xsl:if>
      </xsl:if>

      <xsl:call-template name="at:attribute-set-header"/>
      <xsl:text>;&#10;</xsl:text>
      <!--
      <xsl:value-of select="$I"/>
      <xsl:text>pragma Inline_Always (Set_</xsl:text>
      <xsl:call-template name="attribute-name"/>
      <xsl:text>);&#10;</xsl:text>
      -->
      <xsl:value-of select="$blank-line"/>

    </xsl:if>

  </xsl:template>

  <xsl:template mode="at:attribute-set-spec" match="*"/>


  <!-- Called at class/attribute to generate an attribute "set"
       accessor heading (no semicolon). -->
  <xsl:template name="at:attribute-set-header">

    <xsl:value-of select="$I"/>
    <xsl:text>procedure Set_</xsl:text>
    <xsl:call-template name="at:attribute-name"/>
    <xsl:text>&#10;</xsl:text>
    <xsl:value-of select="$IC"/>
    <xsl:text>(</xsl:text>

    <!-- If this isn't a singleton, we need a handle parameter -->
    <xsl:if test="not(../@singleton)">
      <xsl:text>This : Handle;</xsl:text>
      <xsl:text>&#10;</xsl:text>
      <xsl:value-of select="$IC"/>
      <xsl:text> </xsl:text>
    </xsl:if>

    <xsl:text>To_Be : </xsl:text>
    <xsl:call-template name="at:attribute-type"/>

    <xsl:text>)</xsl:text>

  </xsl:template>


  <!-- Called from domain/class to generate get bodies -->
  <xsl:template
    match="class/attribute[not(@class)]"
    mode="at:attribute-get-body">

    <xsl:choose>

      <xsl:when test="@refers">

        <!-- We need to avoid generating an accessor that the user has
             specified (only likely to occur for [[formalizes]]
             attributes). -->

        <xsl:variable name="operation-name">
          <xsl:text>Get_</xsl:text>
          <xsl:call-template name="at:attribute-name"/>
        </xsl:variable>

        <xsl:if test="not(../operation[name=$operation-name])">
          <xsl:call-template name="at:attribute-get-body"/>
        </xsl:if>

      </xsl:when>

      <xsl:when test="$generate-accessors='yes'">
        <xsl:call-template name="at:attribute-get-body"/>
      </xsl:when>

    </xsl:choose>

  </xsl:template>

  <xsl:template mode="at:attribute-get-body" match="*"/>


  <xsl:template name="at:attribute-get-body">
    <xsl:call-template name="at:attribute-get-header"/>
    <xsl:text> is&#10;</xsl:text>
    <xsl:value-of select="$I"/>
    <xsl:text>begin&#10;</xsl:text>
    <xsl:value-of select="$II"/>
    <xsl:text>return This.</xsl:text>
    <xsl:call-template name="at:attribute-name"/>
    <xsl:text>;&#10;</xsl:text>
    <xsl:value-of select="$I"/>
    <xsl:text>end Get_</xsl:text>
    <xsl:call-template name="at:attribute-name"/>
    <xsl:text>;&#10;</xsl:text>
    <xsl:value-of select="$blank-line"/>
  </xsl:template>


  <!-- Called from domain/class to generate set bodies (non-
       identifier attributes only) -->
  <xsl:template
    match="class/attribute[not(@identifier) and not(@class)]"
    mode="at:attribute-set-body">

    <xsl:if test="@refers or $generate-accessors='yes'">

      <!-- Set procedure -->
      <xsl:call-template name="at:attribute-set-header"/>
      <xsl:text> is&#10;</xsl:text>
      <xsl:value-of select="$I"/>
      <xsl:text>begin&#10;</xsl:text>
      <xsl:value-of select="$II"/>
      <xsl:text>This.</xsl:text>
      <xsl:call-template name="at:attribute-name"/>
      <xsl:text> := To_Be;&#10;</xsl:text>
      <xsl:value-of select="$I"/>
      <xsl:text>end Set_</xsl:text>
      <xsl:call-template name="at:attribute-name"/>
      <xsl:text>;&#10;</xsl:text>
      <xsl:value-of select="$blank-line"/>

    </xsl:if>

  </xsl:template>

  <xsl:template mode="at:attribute-set-body" match="*"/>


  <!-- Called at {class,type}/attribute to generate a record component,
       with optional initializer. -->
  <xsl:template name="at:single-record-component">
    <xsl:param name="indent" select="$II"/>

    <xsl:value-of select="$indent"/>
    <xsl:call-template name="at:attribute-name"/>
    <xsl:text> : </xsl:text>
    <xsl:if test="/domain/type[name=current()/type]/@access">
      <xsl:text>aliased </xsl:text>
    </xsl:if>
    <xsl:call-template name="at:attribute-type"/>
    <xsl:if test="initial">
      <xsl:text> := </xsl:text>
      <xsl:value-of select="initial"/>
    </xsl:if>
    <xsl:text>;&#10;</xsl:text>
    <xsl:if test="@atomic">
      <xsl:value-of select="$indent"/>
      <xsl:text>pragma Atomic (</xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text>);&#10;</xsl:text>
    </xsl:if>
    <xsl:if test="@volatile">
      <xsl:value-of select="$indent"/>
      <xsl:text>pragma Volatile (</xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text>);&#10;</xsl:text>
    </xsl:if>
    <xsl:call-template name="ut:commentary">
      <xsl:with-param name="indent" select="$indent"/>
    </xsl:call-template>
  </xsl:template>


  <!-- Generate attribute name. Normally called at {class,type}/attribute;
       if not, supply parameter "a" as the attribute concerned.
       If this is an anonymous referential attribute, we make up its
       name from the the relationship name and  the role name.
       If not, just use the <name> element. -->
  <xsl:template name="at:attribute-name">
    <xsl:param name="a" select="."/>
    <xsl:choose>
      <xsl:when test="$a/@refers and not($a/name)">
        <xsl:value-of select="$a/@relation"/>
        <xsl:text>_</xsl:text>
        <xsl:value-of select="$a/@role"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$a/name"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>


  <!-- Generate attribute type. Called at {class,type}/attribute -->
  <xsl:template name="at:attribute-type">
    <xsl:choose>
      <xsl:when test="@refers">
        <xsl:text>ColdFrame.Instances.Handle</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:call-template name="ut:type-name">
          <xsl:with-param name="type" select="type"/>
          <xsl:with-param name="class" select=".."/>
        </xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>


</xsl:stylesheet>
