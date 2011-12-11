<?xml version="1.0" encoding="utf-8"?>

<!-- $Id: normalize-profile.xsl,v fd881b1b7e39 2011/12/11 15:20:54 simonjwright $ -->
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

<!--
     This stylesheet modifies an ArgoUML UML 1.4 Profile Definition,
     as contained in a .zargo.uml file, by replacing Argo's UUIDs by
     names derived from the defined stereotype/data type/tag
     definition names. The output contains just the modified <XMI/>
     tree.

     The reasoning is that it appears that any change to the
     definition can result in wholesale UUID changes, with the result
     that user models become invalid.

     This may become unnecessary/inappropriate with UML 2.
     -->

<xsl:stylesheet
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:UML="org.omg.xmi.namespace.UML"
  version="1.0">

  <xsl:output method="xml" encoding="iso-8859-1" indent="yes"/>
  <xsl:strip-space elements="*"/>

  <xsl:template match="/">
    <xsl:apply-templates select="uml/XMI"/>
  </xsl:template>

  <xsl:template match="*">
    <xsl:copy>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="@*">
    <xsl:copy-of select="."/>
  </xsl:template>

  <xsl:template match="UML:DataType/@xmi.id">
    <xsl:variable name="name" select="../@name"/>
    <xsl:attribute name="xmi.id">
      <xsl:value-of select="concat('cf-dt-', $name)"/>
    </xsl:attribute>
  </xsl:template>

  <xsl:template match="UML:Stereotype/@xmi.id">
    <xsl:variable name="name" select="../@name"/>
    <xsl:attribute name="xmi.id">
      <xsl:value-of select="concat('cf-st-', $name)"/>
    </xsl:attribute>
  </xsl:template>

  <!-- Tag Definitions can appear within a stereotype or at the top
  level, though the semantics are the same; so, include the stereotype
  name in the nested case. -->

  <xsl:template
    match="UML:Stereotype.definedTag/UML:TagDefinition/@xmi.id">
    <xsl:variable name="st-name" select="../../../@name"/>
    <xsl:variable name="td-name" select="../@name"/>
    <xsl:attribute name="xmi.id">
      <xsl:value-of select="concat('cf-st-', $st-name, '-td-', $td-name)"/>
    </xsl:attribute>
  </xsl:template>

  <xsl:template
    match="UML:Namespace.ownedElement/UML:TagDefinition/@xmi.id">
    <xsl:variable name="name" select="../@name"/>
    <xsl:attribute name="xmi.id">
      <xsl:value-of select="concat('cf-td-', $name)"/>
    </xsl:attribute>
  </xsl:template>

</xsl:stylesheet>
