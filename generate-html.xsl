<!-- $Id: generate-html.xsl,v b6e95bcd48b6 2001/08/08 18:01:30 simon $ -->

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

  <xsl:import href="ada-attribute.xsl"/>
  <xsl:import href="ada-utilities.xsl"/>

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
        <h2>Contents</h2>
        <xsl:if test="class[@public]">
          <h3>Public Classes</h3>
          <ul>
            <xsl:apply-templates select="class[@public]" mode="index">
              <xsl:sort select="name"/>
            </xsl:apply-templates>
          </ul>
        </xsl:if>
        <xsl:if test="class[not(@public)]">
          <h3>Private Classes</h3>
          <ul>
            <xsl:apply-templates select="class[not(@public)]" mode="index">
              <xsl:sort select="name"/>
            </xsl:apply-templates>
          </ul>
        </xsl:if>
        <xsl:if test="association">
          <h3>Associations</h3>
          <ul>
            <xsl:apply-templates select="association" mode="index">
              <xsl:sort select="name"/>
            </xsl:apply-templates>
          </ul>
        </xsl:if>
        <xsl:if test="inheritance">
          <h3>Inheritance relationships</h3>
          <ul>
            <xsl:apply-templates select="inheritance" mode="index">
              <xsl:sort select="name"/>
            </xsl:apply-templates>
          </ul>
        </xsl:if>
        <xsl:if test="type[not(standard)]">
          <h3>Types</h3>
          <ul>
            <xsl:apply-templates select="type[not(standard)]" mode="index">
              <xsl:sort select="name"/>
            </xsl:apply-templates>
          </ul>
        </xsl:if>
        <!-- End of index -->
        <hr/>
        <xsl:if test="class[@public]">
          <h2>Public Classes</h2>
          <xsl:apply-templates select="class[@public]">
            <xsl:sort select="name"/>
          </xsl:apply-templates>
        </xsl:if>
        <xsl:if test="class[not(@public)]">
          <h2>Private Classes</h2>
          <xsl:apply-templates select="class[not(@public)]">
            <xsl:sort select="name"/>
          </xsl:apply-templates>
        </xsl:if>
        <xsl:if test="association">
          <h2>Associations</h2>
          <xsl:apply-templates select="association">
            <xsl:sort select="name"/>
          </xsl:apply-templates>
        </xsl:if>
        <xsl:if test="inheritance">
          <h2>Inheritance relationships</h2>
          <xsl:apply-templates select="inheritance">
            <xsl:sort select="name"/>
          </xsl:apply-templates>
        </xsl:if>
        <xsl:if test="type[not(standard)]">
          <h2>Types</h2>
          <xsl:apply-templates select="type[not(standard)]">
            <xsl:sort select="name"/>
          </xsl:apply-templates>
        </xsl:if>
      </body>
    </html>
  </xsl:template>


  <!-- Output details of a Class. -->
  <xsl:template match="domain/class" mode="index">
    <li>
      <a href="#{name}"><xsl:value-of select="name"/></a>
    </li>
  </xsl:template>

  <xsl:template match="domain/class">
    <xsl:variable name="name" select="name"/>
    <h3><a name="{$name}"><xsl:value-of select="$name"/></a></h3>
    <xsl:apply-templates select="documentation"/>
    <xsl:if test="../inheritance[child=$name]">
      <xsl:variable
        name="parent"
        select="../inheritance[child=$name]/parent"/>
      <xsl:text>Subtype of </xsl:text>
      <i><a href="#{$parent}">
      <xsl:value-of select="$parent"/></a></i>.
      <p/>
      <xsl:text>&#10;</xsl:text>
    </xsl:if>
    <xsl:text>&#10;</xsl:text>
    <xsl:if test="attribute">
      <h4>Attributes</h4>
      <dl>
        <xsl:apply-templates select="attribute">
          <xsl:sort select="."/>
        </xsl:apply-templates>
      </dl>
    </xsl:if>
    <xsl:if test="operation">
      <h4>Operations</h4>
      <xsl:apply-templates select="operation">
        <xsl:sort select="."/>
      </xsl:apply-templates>
    </xsl:if>
    <xsl:if
      test="../association[role/classname = $name]
            or ../association[associative = $name]">
      <h4>Associations</h4>
      <ul>
        <xsl:for-each
          select="../association[role/classname = $name]
                  | ../association[associative = $name]">
          <xsl:sort select="."/>
          <li>
            <a href="#{name}"><xsl:value-of select="name"/></a>
          </li>
        </xsl:for-each>
      </ul>
    </xsl:if>
  </xsl:template>
    

  <!-- Output details of a Class' Attribute. -->
  <xsl:template match="attribute">
    <xsl:variable name="name">
      <xsl:call-template name="attribute-name"/>
    </xsl:variable>
    <dt>
      <xsl:value-of select="$name"/>
      <xsl:if test="@identifier"> (identifier)</xsl:if>
      <xsl:text> : </xsl:text>
      <xsl:choose>
        <xsl:when test="@refers">
          <xsl:text>Reference to </xsl:text>
          <a href="#{@refers}"><xsl:value-of select="@refers"/></a>
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
            <a href="#{@relation}"><xsl:value-of select="@relation"/></a>
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
  <xsl:template match="operation">
     <h5><xsl:value-of select="name"/></h5>
    <xsl:apply-templates select="documentation"/>
    <xsl:if test="@result">
      <xsl:apply-templates select="@result"/>
    </xsl:if>
    <xsl:if test="parameter">
      <h6>Parameters</h6>
      <dl>
        <xsl:apply-templates select="parameter">
          <xsl:sort select="name"/>
        </xsl:apply-templates>
      </dl>
    </xsl:if>
  </xsl:template>


  <!-- Output details of an Operation's Result. -->
  <xsl:template match="operation/@result">
    <h5>Result</h5>
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


  <!-- Output details of an Association. -->
  <xsl:template match="domain/association" mode="index">
    <li>
      <a href="#{name}"><xsl:value-of select="name"/></a>
    </li>
  </xsl:template>

  <xsl:template match="domain/association">
    <h3><a name="{name}"><xsl:value-of select="name"/></a></h3>
    <xsl:apply-templates select="documentation"/>
    <h4>Roles</h4>
    <xsl:apply-templates select="role"/>
    <xsl:apply-templates select="associative"/>
  </xsl:template>


  <!-- Output details of a Role in an Association. -->
  <xsl:template match="association/role">
    <p>
      <xsl:variable name="other-role-position" select="3 - position()"/>
      <a href="#{../role[$other-role-position]/classname}">
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
      <a href="#{classname}">
        <xsl:value-of select="classname"/>
      </a>
    </p>
    <!-- XXX documentation? -->
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
      <xsl:text>Associative class: </xsl:text>
      <a href="#{.}"><xsl:value-of select="."/></a>
    </p>
  </xsl:template>


  <!-- Output details of an Inheritance relationship. -->
  <xsl:template match="domain/inheritance" mode="index">
    <li>
      <a href="#{name}"><xsl:value-of select="name"/></a>
    </li>
  </xsl:template>

  <xsl:template match="domain/inheritance">
    <h3><a name="{name}"><xsl:value-of select="name"/></a></h3>
    <xsl:apply-templates select="documentation"/>
    <h4>Superclass</h4>
    <p>
      <a href="#{parent}"><xsl:value-of select="parent"/></a>
    </p>
    <h4>Children</h4>
    <ul>
      <xsl:apply-templates select="child">
        <xsl:sort select="."/>
      </xsl:apply-templates>
    </ul>
  </xsl:template>


  <!-- Output the details of a child of an Inheritance relationship. -->
  <xsl:template match="inheritance/child">
    <li>
      <a href="#{.}"><xsl:value-of select="."/></a>
    </li>
  </xsl:template>


  <!-- Output details of a Type. -->
  <xsl:template match="domain/type" mode="index">
    <li>
      <a href="#{name}"><xsl:value-of select="name"/></a>
    </li>
  </xsl:template>

  <xsl:template match="domain/type">
    <h3><a name="{name}"><xsl:value-of select="name"/></a></h3>
    <xsl:if test="@callback">
      <p>
        <xsl:text>Callback support is provided, with support for </xsl:text>
        <xsl:value-of select="@callback"/>
        <xsl:text> registrations.</xsl:text>
      </p>
    </xsl:if>
    <xsl:choose>
      <xsl:when test="enumeration">
        <p>An enumeration, with literals:</p>
        <ul>
          <xsl:for-each select="enumeration/literal">
            <li>
              <xsl:value-of select="."/>
            </li>
          </xsl:for-each>
        </ul>
      </xsl:when>
      <xsl:when test="integer">
        <p>An integral number, with:</p>
        <ul>
          <xsl:if test="integer/lower">
            <li>
              <xsl:text>minimum value </xsl:text>
              <xsl:value-of select="integer/lower"/>
            </li>
          </xsl:if>
          <xsl:if test="integer/upper">
            <li>
              <xsl:text>maximum value </xsl:text>
              <xsl:value-of select="integer/upper"/>
            </li>
          </xsl:if>
        </ul>
      </xsl:when>
      <xsl:when test="real">
        <p>A real number, with:</p>
        <ul>
          <xsl:if test="real/lower">
            <li>
              <xsl:text>minimum value </xsl:text>
              <xsl:value-of select="real/lower"/>
            </li>
          </xsl:if>
          <xsl:if test="real/upper">
            <li>
              <xsl:text>maximum value </xsl:text>
              <xsl:value-of select="real/upper"/>
            </li>
          </xsl:if>
          <xsl:if test="real/digits">
            <li>
              <xsl:value-of select="real/digits"/>
              <xsl:text> significant digits</xsl:text>
            </li>
          </xsl:if>
        </ul>
      </xsl:when>
      <xsl:when test="attribute">
        <p>A record type:</p>
        <xsl:if test="attribute">
          <h4>Attributes</h4>
          <dl>
            <xsl:apply-templates select="attribute">
              <xsl:sort select="."/>
            </xsl:apply-templates>
          </dl>
        </xsl:if>
        <xsl:if test="operation">
          <h4>Operations</h4>
          <xsl:apply-templates select="operation">
            <xsl:sort select="."/>
          </xsl:apply-templates>
        </xsl:if>
      </xsl:when>
      <xsl:when test="set">
        <p>
          <xsl:text>A set of references to </xsl:text>
          <a href="#{set}"><xsl:value-of select="set"/></a>
          <xsl:text>s.</xsl:text>
        </p>
      </xsl:when>
      <xsl:when test="string">
        <p>
          <xsl:text>A string of maximum length </xsl:text>
          <xsl:value-of select="string/max"/>
          <xsl:text> characters.</xsl:text>
        </p>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>Hmm!</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:apply-templates select="documentation"/>
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
  <xsl:template name="attribute-name-not">
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

  <!-- Templates to catch elements missed in "mode" matches -->

  <xsl:template match="*" mode="index"/>

</xsl:stylesheet>
