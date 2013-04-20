<?xml version="1.0" encoding="utf-8"?>

<!-- $Id: generate-profile-html.xsl,v 890a45f07abd 2013/04/20 13:02:21 simonjwright $ -->
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
     This stylesheet generates documentation for an ArgoUML UML 1.4
     Profile Definition.
     -->

<xsl:stylesheet
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:UML="http://www.omg.org/spec/XMI"
  version="1.0">

  <xsl:strip-space elements="*"/>

  <xsl:output
    method="html"
    doctype-public="-//W3C/DTD HTML 4.01 Transitional//EN"
    doctype-system="http://www.w3.org/TR/html4/loose.dtd"/>

  <xsl:template match="/">
    <xsl:apply-templates select="uml/XMI/XMI.content"/>
  </xsl:template>

  <xsl:template match="UML:Model">
    <html>
      <head>
        <title><xsl:value-of select="@name"/></title>
        <link href="cf.css" rel="stylesheet" type="text/css"/>
      </head>
      <body bgcolor="#FFFFFF">

        <div id="header">
          <a href="http://sourceforge.net">
            <img src="http://sourceforge.net/sflogo.php?group_id=135558&amp;type=1"
              width="88" height="31" border="0" alt="SourceForge.net Logo"/>
          </a>
        </div>

        <h1><xsl:value-of select="@name"/></h1>

        <ul>
          <li><a href="#Stereotypes">Stereotypes</a></li>
          <li><a href="#Tags">Tags</a></li>
          <li><a href="#Types">Types</a></li>
        </ul>

        <h2><a name="Stereotypes">Stereotypes</a></h2>

        <table border="1">

          <tr>
            <th>Stereotype</th>
            <th>Applies to</th>
            <th>Documentation</th>
            <th>Tags</th>
          </tr>

          <xsl:apply-templates
            select="UML:Namespace.ownedElement/UML:Stereotype">
            <xsl:sort select="@name"/>
          </xsl:apply-templates>

        </table>

        <h2><a name="Tags">Tags</a></h2>

        <table border="1">

          <tr>
            <th>Tag</th>
            <th>Documentation</th>
            <th>Stereotype</th>
          </tr>

          <xsl:apply-templates
            select="UML:Namespace.ownedElement
                    /UML:Stereotype
                    /UML:Stereotype.definedTag
                    /UML:TagDefinition">
            <xsl:sort select="@name"/>
          </xsl:apply-templates>

        </table>

         <h2><a name="Types">Types</a></h2>

        <table border="1">

          <tr>
            <th>Type</th>
            <th>Documentation</th>
          </tr>

          <xsl:apply-templates
            select="UML:Namespace.ownedElement/UML:DataType">
            <xsl:sort select="@name"/>
          </xsl:apply-templates>

        </table>

      </body>
    </html>
  </xsl:template>

  <xsl:template match="UML:Stereotype">
    <tr>
      <td>
        <a name="st-{@name}"><xsl:value-of select="@name"/></a>
      </td>
      <td>
        <table>
          <xsl:for-each select="UML:Stereotype.baseClass">
            <xsl:sort select="."/>
            <tr>
              <td>
                <xsl:value-of select="."/>
              </td>
            </tr>
          </xsl:for-each>
        </table>
      </td>
      <td>
        <xsl:call-template name="break">
          <xsl:with-param
            name="text"
            select="UML:ModelElement.taggedValue
                    /UML:TaggedValue
                    [UML:TaggedValue.type/UML:TagDefinition/@name='documentation']
                    /UML:TaggedValue.dataValue"/>
        </xsl:call-template>
      </td>
      <td>
        <table>
          <xsl:for-each
            select="UML:Stereotype.definedTag/UML:TagDefinition">
            <xsl:sort select="@name"/>
            <tr>
              <td>
                <a href="#st-{../../@name}-tag-{@name}"><xsl:value-of select="@name"/></a>
              </td>
            </tr>
          </xsl:for-each>
        </table>
      </td>
    </tr>
  </xsl:template>

  <xsl:template match="UML:TagDefinition">
    <tr>
      <td>
        <!-- We might have the same tag name under more than one
             stereotype. -->
        <a name="st-{../../@name}-tag-{@name}"><xsl:value-of select="@name"/></a>
      </td>
      <td>
        <xsl:call-template name="break">
          <xsl:with-param
            name="text"
            select="UML:ModelElement.taggedValue
                    /UML:TaggedValue
                    [UML:TaggedValue.type/UML:TagDefinition/@name='documentation']
                    /UML:TaggedValue.dataValue"/>
        </xsl:call-template>
      </td>
      <td>
        <a href="#st-{../../@name}"><xsl:value-of select="../../@name"/></a>
      </td>
    </tr>
  </xsl:template>

  <xsl:template match="UML:DataType">
    <tr>
      <td>
        <xsl:value-of select="@name"/>
      </td>
      <td>
        <xsl:call-template name="break">
          <xsl:with-param
            name="text"
            select="UML:ModelElement.taggedValue
                    /UML:TaggedValue
                    [UML:TaggedValue.type/UML:TagDefinition/@name='documentation']
                    /UML:TaggedValue.dataValue"/>
        </xsl:call-template>
      </td>
    </tr>
  </xsl:template>

  <xsl:template match="*">
    <xsl:apply-templates select="@*"/>
    <xsl:apply-templates/>
  </xsl:template>

  <!--
       See http://stackoverflow.com/questions/561235/xslt-replace-n-with-br-only-in-one-node
       -->
  <xsl:template name="break">
    <xsl:param name="text" select="."/>
    <xsl:choose>
      <xsl:when test="contains($text, '&#xa;')">
        <xsl:value-of select="substring-before($text, '&#xa;')"/>
        <br/>
        <xsl:call-template name="break">
          <xsl:with-param
            name="text"
            select="substring-after($text, '&#xa;')"/>
        </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$text"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

</xsl:stylesheet>
