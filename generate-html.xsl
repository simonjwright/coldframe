<!-- $Id: generate-html.xsl,v 75920f50be06 2001/03/22 20:33:53 simon $ -->
<!-- $Id: generate-html.xsl,v 75920f50be06 2001/03/22 20:33:53 simon $ -->
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
  version="1.0">

  <xsl:strip-space elements="*"/>

  <xsl:output method="html"/>


  <xsl:template match="domain">
    <html>
      <head>
        <title><xsl:value-of select="name"/></title>
      </head>
      <body bgcolor="#FFFFFF">
        <h1><xsl:value-of select="name"/></h1>
        <xsl:text>&#10;</xsl:text>
        <xsl:apply-templates select="./documentation"/>
        <xsl:if test="class[@interface]">
          <h2>Interface Classes</h2>
          <ul>
            <xsl:apply-templates select="class[@interface]">
              <xsl:sort select="name"/>
            </xsl:apply-templates>
          </ul>
        </xsl:if>
        <xsl:if test="class[not(@interface)]">
          <h2>Private Classes</h2>
          <ul>
            <xsl:apply-templates select="class[not(@interface)]">
              <xsl:sort select="name"/>
            </xsl:apply-templates>
          </ul>
        </xsl:if>
      </body>
    </html>
  </xsl:template>


  <xsl:template match="domain/class">
    <xsl:variable name="name" select="name"/>
    <xsl:variable name="output-file-name">
      <xsl:call-template name="domain-file-name">
        <xsl:with-param name="class-name" select="$name"/>
      </xsl:call-template>
    </xsl:variable>
    <xsl:variable name="file-name">
      <xsl:call-template name="class-file-name">
        <xsl:with-param name="class-name" select="$name"/>
      </xsl:call-template>
    </xsl:variable>
    <li>
      <a href="{$file-name}"><xsl:value-of select="$name"/></a>
    </li>
    <saxon:output file="{$output-file-name}">
      <html>
        <head>
          <title><xsl:value-of select="name"/></title>
        </head>
        <body bgcolor="#FFFFFF">
          <h1><xsl:value-of select="$name"/></h1>
          <xsl:text>&#10;</xsl:text>
          <xsl:if test="../inheritance[child=$name]">
            <xsl:variable
              name="parent"
              select="../inheritance[child=$name]/parent"/>
            <xsl:variable
              name="parent-file-name">
              <xsl:call-template name="class-file-name">
                <xsl:with-param name="class-name" select="$parent"/>
              </xsl:call-template>
            </xsl:variable>
            <xsl:text>Subtype of </xsl:text>
            <i><a href="{$parent-file-name}">
            <xsl:value-of select="$parent"/></a></i>.
            <p/>
            <xsl:text>&#10;</xsl:text>
          </xsl:if>
          <xsl:apply-templates select="documentation"/>
          <xsl:text>&#10;</xsl:text>
          <xsl:choose>
            <xsl:when test="attribute">
              <h2>Attributes</h2>
              <xsl:apply-templates select="attribute">
                <xsl:sort select="."/>
              </xsl:apply-templates>
            </xsl:when>
            <xsl:otherwise>
              <h2>No attributes.</h2>
            </xsl:otherwise>
          </xsl:choose>
          <xsl:choose>
            <xsl:when test="operation">
              <h2>Operations</h2>
              <xsl:apply-templates select="operation">
                <xsl:sort select="."/>
              </xsl:apply-templates>
            </xsl:when>
            <xsl:otherwise>
              <h2>No operations.</h2>
            </xsl:otherwise>
          </xsl:choose>
        </body>
      </html>
    </saxon:output>
  </xsl:template>
    

  <xsl:template match="class/attribute">
    <xsl:variable name="name">
      <xsl:call-template name="attribute-name"/>
    </xsl:variable>
    <h3>
      <a name="{../name}.at.{$name}">
        <xsl:value-of select="$name"/>
      </a>
      <xsl:if test="@identifier"><b> (identifier)</b></xsl:if>
      <xsl:text> : </xsl:text>
      <xsl:choose>
        <xsl:when test="@refers">
          <xsl:variable name="referred-name">
            <xsl:call-template name="class-file-name">
              <xsl:with-param name="class-name" select="@refers"/>
            </xsl:call-template>
          </xsl:variable>
          <xsl:text> reference to </xsl:text>
          <a href="{$referred-name}"><xsl:value-of select="@refers"/></a>
        </xsl:when>
        <xsl:otherwise>
          <xsl:call-template name="attribute-type"/>
        </xsl:otherwise>
      </xsl:choose>
    </h3>
    <xsl:apply-templates select="documentation"/>
  </xsl:template>


  <xsl:template match="class/operation">
     <h3>
       <a name="{../name}.op.{name}">
         <xsl:value-of select="name"/>
       </a>
    </h3>
    <xsl:apply-templates select="documentation"/>
  </xsl:template>


  <xsl:template name="class-file-name">
    <xsl:param name="class-name"/>
    <xsl:value-of select="concat($class-name, '.html')"/>
  </xsl:template>


  <xsl:template name="domain-file-name">
    <xsl:param name="class-name"/>
    <xsl:value-of select="concat(/domain/name, '.doc/', $class-name, '.html')"/>
  </xsl:template>


  <!-- Supporting templates (probably should be included! since shared with
       generate-ada.xsl) -->

  <!-- Generate attribute name. Called at class/attribute -->
  <xsl:template name="attribute-name">
    <xsl:choose>
      <xsl:when test="@refers">
        <xsl:variable name="target-class" select="@refers"/>
        <xsl:value-of select="/domain/class[name=$target-class]/abbreviation"/>
        <xsl:text>_Handle_</xsl:text>
        <xsl:value-of select="@relation"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="name"/>
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


  <!-- Handle special type name conversions. -->
  <xsl:template name="type-name">
    <xsl:param name="type"/>
    <xsl:choose>

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

  <!-- Templates to support HTML markup. -->

  <xsl:template match="b">
    <b><xsl:apply-templates/></b>
  </xsl:template>

  <xsl:template match="i">
    <i><xsl:apply-templates/></i>
  </xsl:template>

  <xsl:template match="tt">
    <tt><xsl:apply-templates/></tt>
  </xsl:template>

  <xsl:template match="br">
    <br/>
  </xsl:template>

  <xsl:template match="par">
    <p><xsl:apply-templates/></p>
  </xsl:template>

</xsl:stylesheet>
