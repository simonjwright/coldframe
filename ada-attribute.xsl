<!-- $Id: ada-attribute.xsl,v 255195d5634a 2001/04/29 10:38:41 simon $ -->
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

     As a special exception, when portions of this file are copied by a
     stylesheet processor into an output file, you may use that output
     file without restriction.
     -->

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="1.0">


  <!-- Called from domain/class to generate the actual identifier
       record for the class. -->
  <xsl:template name="identifier-record">
    <xsl:choose>

      <!-- Output only identifier attributes. -->
      <xsl:when test="count(attribute[@identifier='yes']) &gt; 0">
        <xsl:text>  type Identifier is record&#10;</xsl:text>
        <xsl:apply-templates
          mode="instance-record-component"
          select="attribute[@identifier='yes']"/>
        <xsl:text>  end record;&#10;</xsl:text>
      </xsl:when>

      <xsl:otherwise>
        <xsl:text>  type Identifier is null record;&#10;</xsl:text>
      </xsl:otherwise>

    </xsl:choose>
  </xsl:template>


  <!-- Called from domain/class to generate the actual instance
       record for the class. -->
  <xsl:template name="instance-record">
    <xsl:param name="is-supertype"/>
    <xsl:choose>

      <!-- Output all attributes. -->
      <xsl:when test="count(attribute) &gt; 0">
        <xsl:text>  type Instance is record&#10;</xsl:text>
        <xsl:apply-templates
          mode="instance-record-component"
          select="attribute"/>
        <xsl:if test="$is-supertype">
          <xsl:text>    Current_Child : Child_Class;&#10;</xsl:text>
        </xsl:if>
        <xsl:text>  end record;&#10;</xsl:text>
      </xsl:when>

      <xsl:otherwise>
        <xsl:text>  type Instance is null record;&#10;</xsl:text>
      </xsl:otherwise>

    </xsl:choose>
  </xsl:template>


  <!-- Generate the individual components of the class identifier 
       or instance record. -->
  <xsl:template match="attribute" mode="instance-record-component">
    <xsl:text>    </xsl:text>
    <xsl:call-template name="attribute-name"/>
    <xsl:text> : </xsl:text>
    <xsl:call-template name="attribute-type"/>
    <xsl:if test="initial">
      <xsl:text> := </xsl:text>
      <xsl:value-of select="initial"/>
    </xsl:if>
    <xsl:text>;&#10;</xsl:text>
  </xsl:template>

  <xsl:template mode="instance-record-component" match="*"/>


  <!-- Called from domain/class to generate get specs -->
  <xsl:template
    match="class/attribute"
    mode="attribute-get-spec">

    <!-- Get function -->
    <xsl:text>  function Get_</xsl:text>
    <xsl:call-template name="attribute-name"/>

    <!-- If this isn't a singleton, we need a handle parameter -->
    <xsl:if test="not(../@singleton)">
      <xsl:text> (This : Handle)</xsl:text>
    </xsl:if>

    <xsl:text> return </xsl:text>
    <xsl:call-template name="attribute-type"/>
    <xsl:text>;&#10;</xsl:text>

  </xsl:template>

  <xsl:template mode="attribute-get-spec" match="*"/>


  <!-- Called from domain/class to generate set specs (non-
       identifier attributes only) -->
  <xsl:template
    match="class/attribute[not(@identifier='yes')]"
    mode="attribute-set-spec">

    <!-- Set procedure -->
    <xsl:text>  procedure Set_</xsl:text>
    <xsl:call-template name="attribute-name"/>
    <xsl:text> (</xsl:text>

    <!-- If this isn't a singleton, we need a handle parameter -->
    <xsl:if test="not(../@singleton)">
      <xsl:text>This : Handle; </xsl:text>
    </xsl:if>

    <xsl:text>To_Be : </xsl:text>
    <xsl:call-template name="attribute-type"/>
    <xsl:text>);&#10;</xsl:text>

  </xsl:template>

  <xsl:template mode="attribute-set-spec" match="*"/>


  <!-- Called from domain/class to generate get bodies -->
  <xsl:template
    match="class/attribute"
    mode="attribute-get-body">

    <!-- Get function -->
    <xsl:text>  function Get_</xsl:text>
    <xsl:call-template name="attribute-name"/>

    <!-- If this isn't a singleton, we need a handle parameter -->
    <xsl:if test="not(../@singleton)">
      <xsl:text> (This : Handle)</xsl:text>
    </xsl:if>

    <xsl:text> return </xsl:text>
    <xsl:call-template name="attribute-type"/>
    <xsl:text> is&#10;</xsl:text>
    <xsl:text>  begin&#10;</xsl:text>
    <xsl:text>    return This.</xsl:text>
    <xsl:call-template name="attribute-name"/>
    <xsl:text>;&#10;</xsl:text>
    <xsl:text>  end Get_</xsl:text>
    <xsl:call-template name="attribute-name"/>
    <xsl:text>;&#10;</xsl:text>

  </xsl:template>

  <xsl:template mode="attribute-get-body" match="*"/>


  <!-- Called from domain/class to generate set bodies (non-
       identifier attributes only) -->
  <xsl:template
    match="class/attribute[not(@identifier='yes')]"
    mode="attribute-set-body">

    <!-- Set procedure -->
    <xsl:text>  procedure Set_</xsl:text>
    <xsl:call-template name="attribute-name"/>
    <xsl:text> (</xsl:text>

    <!-- If this isn't a singleton, we need a handle parameter -->
    <xsl:if test="not(../@singleton)">
      <xsl:text>This : Handle; </xsl:text>
    </xsl:if>

    <xsl:text>To_Be : </xsl:text>
    <xsl:call-template name="attribute-type"/>
    <xsl:text>) is&#10;</xsl:text>
    <xsl:text>  begin&#10;</xsl:text>
    <xsl:text>    This.</xsl:text>
    <xsl:call-template name="attribute-name"/>
    <xsl:text> := To_Be;&#10;</xsl:text>
    <xsl:text>  end Set_</xsl:text>
    <xsl:call-template name="attribute-name"/>
    <xsl:text>;&#10;</xsl:text>

  </xsl:template>

  <xsl:template mode="attribute-set-body" match="*"/>


  <!-- Generate attribute name. Normally called at class/attribute;
       if not, supply parameter "a" as the attribute concerned.
       If this is an anonymous referential attribute, we make up its
       name from the abbreviation of the supplier, the role name, and
       the relationship name.
       If not, just use the <name> element. -->
  <xsl:template name="attribute-name">
    <xsl:param name="a" select="."/>
    <xsl:choose>
      <xsl:when test="$a/@refers and not($a/name)">
        <xsl:value-of select="$a/@relation"/>
        <xsl:text>_</xsl:text>
        <xsl:value-of select="$a/@role"/>
        <xsl:text>_</xsl:text>
        <xsl:value-of select="/domain/class[name=$a/@refers]/abbreviation"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$a/name"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>


  <!-- Generate attribute type. Called at class/attribute -->
  <xsl:template name="attribute-type">
    <xsl:choose>
      <xsl:when test="@refers">
        <xsl:value-of select="../../name"/>
        <xsl:text>.</xsl:text>
        <xsl:value-of select="@refers"/>
        <xsl:text>.Handle</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:call-template name="type-name">
          <xsl:with-param name="type" select="type"/>
        </xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>


</xsl:stylesheet>
