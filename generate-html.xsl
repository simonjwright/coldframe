<!-- $Id: generate-html.xsl,v ae99ae4c60b7 2001/04/13 11:15:35 simon $ -->

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
        <xsl:if test="association">
          <h2>Associations</h2>
          <ul>
            <xsl:apply-templates select="association">
              <xsl:sort select="name"/>
            </xsl:apply-templates>
          </ul>
        </xsl:if>
        <xsl:if test="inheritance">
          <h2>Inheritance relationships</h2>
          <ul>
            <xsl:apply-templates select="inheritance">
              <xsl:sort select="name"/>
            </xsl:apply-templates>
          </ul>
        </xsl:if>
        <xsl:if test="type[not(standard)]">
          <h2>Types</h2>
          <ul>
            <xsl:apply-templates select="type[not(standard)]">
              <xsl:sort select="name"/>
            </xsl:apply-templates>
          </ul>
        </xsl:if>
      </body>
    </html>
  </xsl:template>


  <!-- Output details of a Class; in the main file, as a pointer to the
       full info in its own file. -->
  <xsl:template match="domain/class">
    <xsl:variable name="name" select="name"/>
    <xsl:variable name="output-file-name">
      <xsl:call-template name="domain-file-name">
        <xsl:with-param name="element-name" select="$name"/>
      </xsl:call-template>
    </xsl:variable>
    <xsl:variable name="file-name">
      <xsl:call-template name="element-file-name">
        <xsl:with-param name="element-name" select="$name"/>
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
          <xsl:apply-templates select="documentation"/>
          <xsl:if test="../inheritance[child=$name]">
            <xsl:variable
              name="parent"
              select="../inheritance[child=$name]/parent"/>
            <xsl:variable
              name="parent-file-name">
              <xsl:call-template name="element-file-name">
                <xsl:with-param name="element-name" select="$parent"/>
              </xsl:call-template>
            </xsl:variable>
            <xsl:text>Subtype of </xsl:text>
            <i><a href="{$parent-file-name}">
            <xsl:value-of select="$parent"/></a></i>.
            <p/>
            <xsl:text>&#10;</xsl:text>
          </xsl:if>
          <xsl:text>&#10;</xsl:text>
          <xsl:if test="attribute">
            <h2>Attributes</h2>
            <dl>
              <xsl:apply-templates select="attribute">
                <xsl:sort select="."/>
              </xsl:apply-templates>
            </dl>
          </xsl:if>
          <xsl:if test="operation">
            <h2>Operations</h2>
            <xsl:apply-templates select="operation">
              <xsl:sort select="."/>
            </xsl:apply-templates>
          </xsl:if>
          <xsl:if
            test="../association[role/classname = $name]
                  or ../association[associative = $name]">
            <h2>Associations</h2>
            <ul>
              <xsl:apply-templates
                select="../association[role/classname = $name]
                        | ../association[associative = $name]">
                <xsl:sort select="."/>
              </xsl:apply-templates>
            </ul>
          </xsl:if>
        </body>
      </html>
    </saxon:output>
  </xsl:template>
    

  <!-- Output details of a Class' Attribute. -->
  <xsl:template match="class/attribute">
    <xsl:variable name="name">
      <xsl:call-template name="attribute-name"/>
    </xsl:variable>
    <dt>
      <xsl:value-of select="$name"/>
      <xsl:if test="@identifier"> (identifier)</xsl:if>
      <xsl:text> : </xsl:text>
      <xsl:choose>
        <xsl:when test="@refers">
          <xsl:variable name="referred-name">
            <xsl:call-template name="element-file-name">
              <xsl:with-param name="element-name" select="@refers"/>
            </xsl:call-template>
          </xsl:variable>
          <xsl:text>Reference to </xsl:text>
          <a href="{$referred-name}"><xsl:value-of select="@refers"/></a>
        </xsl:when>
        <xsl:otherwise>
          <xsl:call-template name="type-name">
            <xsl:with-param name="type" select="type"/>
          </xsl:call-template>
        </xsl:otherwise>
      </xsl:choose>
    </dt>
    <dd>
      <xsl:choose>
        <xsl:when test="@refers">
          <p>
            <xsl:text>Formalizes </xsl:text>
            <xsl:variable name="file-name">
              <xsl:call-template name="element-file-name">
                <xsl:with-param name="element-name" select="@relation"/>
              </xsl:call-template>
            </xsl:variable>
            <a href="{$file-name}"><xsl:value-of select="@relation"/></a>
          </p>
          <xsl:apply-templates select="documentation"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:apply-templates select="documentation"/>
        </xsl:otherwise>
      </xsl:choose>
    </dd>
  </xsl:template>


  <!-- Output Operation info. -->
  <xsl:template match="class/operation">
     <h3><xsl:value-of select="name"/></h3>
    <xsl:apply-templates select="documentation"/>
    <xsl:if test="@result">
      <xsl:apply-templates select="@result"/>
    </xsl:if>
    <xsl:if test="parameter">
      <h4>Parameters</h4>
      <dl>
        <xsl:apply-templates select="parameter">
          <xsl:sort select="name"/>
        </xsl:apply-templates>
      </dl>
    </xsl:if>
  </xsl:template>


  <!-- Output details of an Operation's Result. -->
  <xsl:template match="operation/@result">
    <h4>Result</h4>
    <p>
      <xsl:value-of select="."/>
    </p>
    <xsl:apply-templates select="documentation"/>
  </xsl:template>


  <!-- Output details of an Operation's Parameter. -->
  <xsl:template match="operation/parameter">
    <dt>
      <xsl:value-of select="name"/>
      <xsl:text> : </xsl:text>
      <xsl:call-template name="type-name">
        <xsl:with-param name="type" select="type"/>
      </xsl:call-template>
    </dt>
    <dd>
      <xsl:apply-templates select="documentation"/>
    </dd>
  </xsl:template>


  <!-- Output details of an Association; in the main output file, as a
       link to the full details in the Association's own file. -->
  <xsl:template match="domain/association">
    <xsl:variable name="name" select="name"/>
    <xsl:variable name="output-file-name">
      <xsl:call-template name="domain-file-name">
        <xsl:with-param name="element-name" select="name"/>
      </xsl:call-template>
    </xsl:variable>
    <xsl:variable name="file-name">
      <xsl:call-template name="element-file-name">
        <xsl:with-param name="element-name" select="name"/>
      </xsl:call-template>
    </xsl:variable>
    <li>
      <a href="{$file-name}"><xsl:value-of select="name"/></a>
    </li>
    <saxon:output file="{$output-file-name}">
      <html>
        <head>
          <title><xsl:value-of select="name"/></title>
        </head>
        <body bgcolor="#FFFFFF">
          <h1><xsl:value-of select="name"/></h1>
          <xsl:apply-templates select="documentation"/>
          <h2>Roles</h2>
          <xsl:apply-templates select="role"/>
          <xsl:apply-templates select="associative"/>
        </body>
      </html>
    </saxon:output>
  </xsl:template>


  <!-- Output details of a Role in an Association. -->
  <xsl:template match="association/role">
    <p>
      <xsl:variable name="other-role-position" select="3 - position()"/>
      <xsl:variable name="subject">
        <xsl:call-template name="element-file-name">
          <xsl:with-param
            name="element-name"
            select="../role[$other-role-position]/classname"/>
        </xsl:call-template>
      </xsl:variable>
      <xsl:variable name="object">
        <xsl:call-template name="element-file-name">
          <xsl:with-param
            name="element-name"
            select="classname"/>
        </xsl:call-template>
      </xsl:variable>
      <a href="{$subject}">
        <xsl:value-of select="../role[$other-role-position]/classname"/>
      </a>
      <xsl:text> (</xsl:text>
      <xsl:apply-templates
        mode="multiplicity"
        select="../role[$other-role-position]"/>
      <xsl:text>) </xsl:text>
      <i><xsl:value-of select="name"/></i>
      <xsl:text> (</xsl:text>
      <xsl:apply-templates mode="multiplicity" select="."/>
      <xsl:text>) </xsl:text>
      <a href="{$object}">
        <xsl:value-of select="classname"/>
      </a>
    </p>
  </xsl:template>


  <!-- Output the multiplicity and conditionality of a Role. -->
  <xsl:template mode="multiplicity" match="association/role">
    <xsl:choose>
      <xsl:when test="@multiple and @conditional">
        <xsl:text>0..n</xsl:text>
      </xsl:when>
      <xsl:when test="@multiple and not(@conditional)">
        <xsl:text>1..n</xsl:text>
      </xsl:when>
      <xsl:when test="not(@multiple) and @conditional">
        <xsl:text>0..1</xsl:text>
      </xsl:when>
      <xsl:when test="not(@multiple) and not(@conditional)">
        <xsl:text>1</xsl:text>
      </xsl:when>
    </xsl:choose>
  </xsl:template>
  <xsl:template mode="multiplicity" match="*"/>


  <!-- Output an Association's associative class (if it has one). -->
  <xsl:template match="association/associative">
    <p>
      <xsl:variable name="class">
        <xsl:call-template name="element-file-name">
          <xsl:with-param
            name="element-name"
            select="."/>
        </xsl:call-template>
      </xsl:variable>
      <xsl:text>Associative: </xsl:text>
      <a href="{$class}">
        <xsl:value-of select="."/>
      </a>
    </p>
  </xsl:template>


  <!-- Output details of an Inheritance relationship; in the main file, as
       a link to the full info in its own file. -->
  <xsl:template match="domain/inheritance">
    <xsl:variable name="name" select="name"/>
    <xsl:variable name="output-file-name">
      <xsl:call-template name="domain-file-name">
        <xsl:with-param name="element-name" select="name"/>
      </xsl:call-template>
    </xsl:variable>
    <xsl:variable name="file-name">
      <xsl:call-template name="element-file-name">
        <xsl:with-param name="element-name" select="name"/>
      </xsl:call-template>
    </xsl:variable>
    <li>
      <a href="{$file-name}"><xsl:value-of select="name"/></a>
    </li>
    <saxon:output file="{$output-file-name}">
      <html>
        <head>
          <title><xsl:value-of select="name"/></title>
        </head>
        <body bgcolor="#FFFFFF">
          <h1><xsl:value-of select="name"/></h1>
          <xsl:apply-templates select="documentation"/>
          <h2>Superclass</h2>
          <xsl:variable name="parent-file-name">
            <xsl:call-template name="element-file-name">
              <xsl:with-param name="element-name" select="parent"/>
            </xsl:call-template>
          </xsl:variable>
          <p>
            <a href="{$parent-file-name}">
              <xsl:value-of select="parent"/>
            </a>
          </p>
          <h2>Children</h2>
          <ul>
            <xsl:apply-templates select="child">
              <xsl:sort select="."/>
            </xsl:apply-templates>
          </ul>
        </body>
      </html>
    </saxon:output>
  </xsl:template>


  <!-- Output the details of a child of an Inheritance relationship. -->
  <xsl:template match="inheritance/child">
    <xsl:variable name="file-name">
      <xsl:call-template name="element-file-name">
        <xsl:with-param name="element-name" select="."/>
      </xsl:call-template>
    </xsl:variable>
    <li>
      <a href="{$file-name}">
        <xsl:value-of select="."/>
      </a>
    </li>
  </xsl:template>


  <!-- Output details of a Type; in the main file, as a link to the
       full info in its own file. -->
  <xsl:template match="domain/type">
    <xsl:variable name="name" select="name"/>
    <xsl:variable name="output-file-name">
      <xsl:call-template name="domain-file-name">
        <xsl:with-param name="element-name" select="name"/>
      </xsl:call-template>
    </xsl:variable>
    <xsl:variable name="file-name">
      <xsl:call-template name="element-file-name">
        <xsl:with-param name="element-name" select="name"/>
      </xsl:call-template>
    </xsl:variable>
    <li>
      <a href="{$file-name}"><xsl:value-of select="name"/></a>
    </li>
    <saxon:output file="{$output-file-name}">
      <html>
        <head>
          <title><xsl:value-of select="name"/></title>
        </head>
        <body bgcolor="#FFFFFF">
          <h1><xsl:value-of select="name"/></h1>
          <xsl:choose>
            <xsl:when test="enumeration">
              <ul>
                <xsl:for-each select="enumeration/literal">
                  <li>
                    <xsl:value-of select="."/>
                  </li>
                </xsl:for-each>
              </ul>
            </xsl:when>
            <xsl:when test="real">
              <p>
                <xsl:text>A real number, with minimum value </xsl:text>
                <xsl:value-of select="real/lower"/>
                <xsl:text>, maximum value </xsl:text>
                <xsl:value-of select="real/upper"/>
                <xsl:text>, and </xsl:text>
                <xsl:value-of select="real/digits"/>
                <xsl:text> significant digits.</xsl:text>
              </p>
            </xsl:when>
            <xsl:when test="integer">
              <p>
                <xsl:text>An integral number, with minimum value </xsl:text>
                <xsl:value-of select="integer/lower"/>
                <xsl:text> and maximum value </xsl:text>
                <xsl:value-of select="integer/upper"/>
                <xsl:text>.</xsl:text>
              </p>
            </xsl:when>
            <xsl:when test="string">
              <p>
                <xsl:text>A string of maximum length </xsl:text>
                <xsl:value-of select="string/max"/>
                <xsl:text> characters.</xsl:text>
              </p>
            </xsl:when>
            <xsl:when test="set">
              <xsl:variable name="class-name">
                <xsl:call-template name="element-file-name">
                  <xsl:with-param name="element-name" select="set"/>
                </xsl:call-template>
              </xsl:variable>
              <p>
                <xsl:text>A set of references to </xsl:text>
                <a href="{$class-name}"><xsl:value-of select="set"/></a>
                <xsl:text>s.</xsl:text>
              </p>
            </xsl:when>
            <xsl:otherwise>
              <xsl:text>Hmm!</xsl:text>
            </xsl:otherwise>
          </xsl:choose>
          <xsl:apply-templates select="documentation"/>
        </body>
      </html>
    </saxon:output>
  </xsl:template>


  <!-- Utilities -->

  <!-- Forms the file name for the given element: the name is <Class>.html -->
  <xsl:template name="element-file-name">
    <xsl:param name="element-name"/>
    <xsl:value-of select="concat($element-name, '.html')"/>
  </xsl:template>


  <!-- Forms the directory/file name for the given element: the name is
       <Domain>.doc/<Class>.html -->
  <xsl:template name="domain-file-name">
    <xsl:param name="element-name"/>
    <xsl:value-of
      select="concat(/domain/name,
              '.doc/',
              $element-name,
              '.html')"/>
  </xsl:template>


  <!-- Supporting templates (probably should be included! since shared with
       generate-ada.xsl) -->

  <!-- Generate attribute name. Called at class/attribute -->
  <xsl:template name="attribute-name">
    <xsl:choose>
      <xsl:when test="@refers and not(name)">
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


  <!-- Handle special type name conversions. -->
  <xsl:template name="type-name">
    <xsl:param name="type"/>
    <xsl:choose>

      <!-- Output a 'standard' type as-is -->
      <xsl:when test="/domain/type[name=$type]/standard">
        <xsl:value-of select="$type"/>
        <!-- xsl:text>Unbounded_String</xsl:text -->
      </xsl:when>

      <!-- Class -->
      <xsl:when test="/domain/class[name=$type]">
        <xsl:variable name="class-name">
          <xsl:call-template name="element-file-name">
            <xsl:with-param name="element-name" select="$type"/>
          </xsl:call-template>
        </xsl:variable>
        <a href="{$class-name}"><xsl:value-of select="$type"/></a>
      </xsl:when>

      <!-- Non-standard Types -->
      <xsl:when test="/domain/type[name=$type]">
        <xsl:variable name="type-name">
          <xsl:call-template name="element-file-name">
            <xsl:with-param name="element-name" select="$type"/>
          </xsl:call-template>
        </xsl:variable>
        <a href="{$type-name}"><xsl:value-of select="$type"/></a>
      </xsl:when>

      <xsl:otherwise>
        <xsl:text>Hmm! how did we get here?!</xsl:text>
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
