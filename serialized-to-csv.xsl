<!-- $Id: serialized-to-csv.xsl,v c6dd3980acf1 2003/11/18 12:12:52 simon $ -->
<!-- XSL stylesheet to convert a document containing mixed serialization
     output from ColdFrame to comma-separated-variable files, one file per
     record name. -->
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
  version="1.1">
  <!-- In 1.1,
       result tree fragment can be implicitly converted to node set
       xsl:document is available -->

  <xsl:strip-space elements="*"/>

  <xsl:output method="text"/>


  <!-- Remember the main document. -->
  <xsl:variable name="main-document" select="/"/>


  <xsl:template match="*">

    <!-- collect all the record names -->
    <xsl:variable name="records">
      <xsl:for-each select="record">
        <xsl:element name="r">
          <xsl:value-of select="@name"/>
        </xsl:element>
      </xsl:for-each>
    </xsl:variable>

    <!-- sort, then only process the first occurrence of each name -->
    <xsl:for-each select="$records/r">
      <xsl:sort/>
      <xsl:if test="not (.=preceding-sibling::node())">

        <!-- choose the output file name -->
        <xsl:document href="{current()}.csv">

          <xsl:message>
            <xsl:text>processing </xsl:text>
            <xsl:value-of select="current()"/>
          </xsl:message>

          <!-- process all the records in the original document for
               this name -->
          <xsl:apply-templates
            select="$main-document/*/record[@name=current()]"/>

        </xsl:document>

      </xsl:if>
    </xsl:for-each>

  </xsl:template>


  <!-- output the records for a particular record name -->
  <xsl:template match="record">
    <xsl:if test="position()=1">
      <!-- output the headings, only for the first -->
      <xsl:for-each select="field/@name">
        <xsl:value-of select="."/>
        <xsl:text>,</xsl:text>
      </xsl:for-each>
      <xsl:text>&#10;</xsl:text>
    </xsl:if>
    <xsl:for-each select="field">
      <xsl:value-of select="."/>
      <xsl:text>,</xsl:text>
    </xsl:for-each>
    <xsl:text>&#10;</xsl:text>
  </xsl:template>


</xsl:stylesheet>
