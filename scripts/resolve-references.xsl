<?xml version="1.0" encoding="utf-8"?>

<!-- $Id: resolve-references.xsl,v d52f8deedf1f 2012/01/08 10:15:50 simonjwright $ -->
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
     ArgoUML XMI doesn't include the actual name of a referenced
     entity; instead, it inserts an xmi.idref attribute which links to
     the actual entity. This is clearly a Good Thing, because if you
     change the name you only have to change it in one place.

     This stylesheet modifies ArgoUML XMI; each xmi.idref is copied
     together with a copy of the name attribute of the referenced
     entity.

     This is to simplify the task of normalize-xmi, since this sort of
     transformation is much easier in XSLT.

     The modified XMI is only for ColdFrame processing, it's not seen
     by ArgoUML.
     -->

<xsl:stylesheet
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:UML="org.omg.xmi.namespace.UML"
  version="1.0">

  <xsl:output method="xml" encoding="iso-8859-1" indent="yes"/>
  <xsl:strip-space elements="*"/>

  <!-- Where to find the profile .xmi files. NB trailing '/'. -->
  <xsl:param name="library_path" select="'./'"/>

  <xsl:variable name="argo_xmi" select="'default-uml14.xmi'"/>
  <xsl:variable name="argo_path" select="concat($library_path, $argo_xmi)"/>
  <xsl:variable name="argo" select="document($argo_path)"/>

  <xsl:variable name="cf_xmi" select="'ColdFrameProfile.xmi'"/>
  <xsl:variable name="cf_path" select="concat($library_path, $cf_xmi)"/>
  <xsl:variable name="cf" select="document($cf_path)"/>

  <xsl:template match="@*|node()">
    <xsl:copy>
      <xsl:apply-templates select="@*|node()"/>
    </xsl:copy>
  </xsl:template>

  <!-- Find the name attribute (if any) of the externally-referenced
  object and specify that as the name attribute of this object. -->
  <xsl:template match="UML:*[@href]">
    <xsl:variable name="href" select="@href"/>
    <xsl:variable name="file" select="substring-before($href, '#')"/>
    <xsl:variable name="locator" select="substring-after($href, '#')"/>
    <xsl:attribute name="href">
      <xsl:value-of select="$href"/>
    </xsl:attribute>
    <xsl:choose>
      <xsl:when test="contains($file, $argo_xmi)">
        <xsl:attribute name="name">
          <xsl:value-of
            select="$argo//UML:*[@xmi.id=$locator]/@name"/>
        </xsl:attribute>
      </xsl:when>
      <xsl:when test="contains($file, $cf_xmi)">
        <xsl:attribute name="name">
          <xsl:value-of
            select="$cf//UML:*[@xmi.id=$locator]/@name"/>
        </xsl:attribute>
      </xsl:when>
    </xsl:choose>
  </xsl:template>

  <!-- Find the name attribute (if any) of the referenced object and
  specify that as the name attribute of this object. -->
  <xsl:template match="UML:*[@xmi.idref]">
    <xsl:variable name="idref" select="@xmi.idref"/>
    <xsl:attribute name="xmi.idref">
      <xsl:value-of select="$idref"/>
    </xsl:attribute>
    <xsl:attribute name="name">
      <xsl:value-of select="//UML:*[@xmi.id=$idref]/@name"/>
    </xsl:attribute>
  </xsl:template>

</xsl:stylesheet>
