<!-- $Id: generate-html.xsl,v 31f0fc86ca6f 2004/01/18 20:29:50 simon $ -->

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


  <!-- Controls how attribute accessor functions are generated. -->
  <xsl:param name="generate-accessors"/>

  <!-- Control indentation. -->
  <xsl:param name="standard-indent" select="'   '"/>
  <xsl:param name="continuation-indent" select="'  '"/>

  <!-- Control added blank lines: no or yes.. -->
  <xsl:param name="add-blank-lines" select="'yes'"/>

  <!-- Control verbosity: no or yes. -->
  <xsl:param name="verbose" select="'no'"/>

  <!-- Control comment paragraph fill width. -->
  <xsl:param name="fill-column" select="70"/>

  <!-- Control limit on using bounded containers. -->
  <xsl:param name="max-bounded-container" select="49"/>

  <!-- Control limit on number of hash buckets. -->
  <xsl:param name="max-hash-buckets" select="49"/>


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


  <xsl:template match="domain">
    <html>
      <head>
        <title><xsl:value-of select="name"/></title>
      </head>
      <body bgcolor="white">
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
    <xsl:for-each select="../inheritance[parent=$name]">
      <xsl:sort select="name"/>
      <xsl:text>Supertype in </xsl:text>
      <a href="#{name}"><xsl:value-of select="name"/></a>.
      <p/>
      <xsl:text>&#10;</xsl:text>
    </xsl:for-each>
    <xsl:for-each select="../inheritance[child=$name]">
      <xsl:sort select="name"/>
      <xsl:variable
        name="parent"
        select="../inheritance[child=$name]/parent"/>
      <xsl:text>Subtype of </xsl:text>
      <i><a href="#{parent}">
      <xsl:value-of select="parent"/></a></i>
      <xsl:text> in </xsl:text>
      <a href="#{name}"><xsl:value-of select="name"/></a>.
      <p/>
      <xsl:text>&#10;</xsl:text>
    </xsl:for-each>
    <xsl:text>&#10;</xsl:text>
    <xsl:if test="attribute">
      <h4>Attributes</h4>
      <dl>
        <xsl:apply-templates select="attribute">
          <xsl:sort select="."/>
        </xsl:apply-templates>
      </dl>
    </xsl:if>
    <xsl:if test="event[@class]">
      <h4>Class events</h4>
      <ul>
        <xsl:for-each select="event[@class]">
          <xsl:sort select="name"/>
          <xsl:call-template name="class-event-details"/>
        </xsl:for-each>
      </ul>
    </xsl:if>
    <xsl:if test="statemachine">
      <h4>States</h4>
      <ul>
        <xsl:for-each select="statemachine/state">
          <xsl:sort select="not(@initial)"/>
          <xsl:sort select="name"/>
          <xsl:call-template name="state-details"/>
        </xsl:for-each>
      </ul>
      <h4>Events</h4>
      <ul>
        <xsl:for-each select="statemachine/event">
          <xsl:sort select="name"/>
          <xsl:call-template name="event-details"/>
        </xsl:for-each>
      </ul>
      <h4>State-Event Matrix</h4>
      <xsl:call-template name="state-machine"/>
    </xsl:if>
    <xsl:if test="operation">
      <h4>Operations</h4>
      <dl>
        <xsl:apply-templates select="operation">
          <xsl:sort select="."/>
        </xsl:apply-templates>
      </dl>
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


  <!-- Output details of a State. Called at statemachine/state
       in a List context. -->
  <xsl:template name="state-details">

    <li>
      <a name="{../../name}.{name}"><xsl:value-of select="name"/></a>
      <xsl:apply-templates select="documentation"/>
    </li>

  </xsl:template>


  <!-- Output details of a class Event. Called at statemachine/event
       in a List context. -->
  <xsl:template name="class-event-details">

    <li>
      <a name="{../name}.{name}"><xsl:value-of select="name"/></a>
      <xsl:if test="type">
        <xsl:text> (payload of type </xsl:text>
        <xsl:call-template name="type-name-linked">
          <xsl:with-param name="type" select="type"/>
        </xsl:call-template>
        <xsl:text>)</xsl:text>
      </xsl:if>
      <xsl:apply-templates select="documentation"/>
    </li>

  </xsl:template>


  <!-- Output details of an instance Event. Called at statemachine/event
       in a List context. -->
  <xsl:template name="event-details">

    <li>
      <a name="{../../name}.{name}"><xsl:value-of select="name"/></a>
      <xsl:if test="type">
        <xsl:text> (payload of type </xsl:text>
        <xsl:call-template name="type-name-linked">
          <xsl:with-param name="type" select="type"/>
        </xsl:call-template>
        <xsl:text>)</xsl:text>
      </xsl:if>
      <xsl:apply-templates select="documentation"/>
    </li>

  </xsl:template>


  <!-- Called at class to output a Class's state model. -->
  <xsl:template name="state-machine">
    <table border="1">
      <tr>
        <th>State</th>
        <th>Entry Action(s)</th>
        <xsl:for-each select="statemachine/event">
          <xsl:sort select="name"/>
          <th><xsl:value-of select="name"/></th>
        </xsl:for-each>
        <th>Drop-through</th>
      </tr>
      <xsl:for-each select="statemachine/state">
        <xsl:sort select="not(@initial)"/>
        <xsl:sort select="name"/>
        <xsl:variable name="st" select="name"/>
        <tr>
          <td>
            <a href="#{../../name}.{name}">
              <xsl:value-of select="name"/>
            </a>
          </td>
          <td>
            <xsl:choose>
              <xsl:when test="action">
                <xsl:for-each select="action">
                  <a href="#{../../../name}.{.}">
                    <xsl:value-of select="."/>
                  </a>
                  <xsl:if test="position() &lt; last()">
                    <br/>
                  </xsl:if>
                </xsl:for-each>
              </xsl:when>
              <xsl:otherwise>
                <i>none</i>
              </xsl:otherwise>
            </xsl:choose>
          </td>
          <xsl:for-each select="../event">
            <xsl:sort select="name"/>
            <td>
              <xsl:choose>
                <xsl:when
                  test="../transition
                        [source=$st and event=current()]/@ignore">
                  <i>ignore</i>
                </xsl:when>
                <xsl:when
                  test="../transition
                        [source=$st and event=current()]">
                  <xsl:value-of
                    select="../transition[source=$st and event=current()]
                            /target"/>
                  <xsl:variable
                    name="action"
                    select="../transition[source=$st and event=current()]
                            /action"/>
                  <xsl:if test="$action">
                    <xsl:text>/</xsl:text>
                    <a href="#{../../name}.{$action}">
                      <xsl:value-of select="$action"/>
                    </a>
                  </xsl:if>
                </xsl:when>
                <xsl:otherwise>
                  <i>can't happen</i>
                </xsl:otherwise>
              </xsl:choose>
            </td>
          </xsl:for-each>
          <td>
            <xsl:choose>
              <xsl:when test="../transition[source=$st and not(event)]">
                <xsl:value-of
                  select="../transition[source=$st and not(event)]/target"/>
              </xsl:when>
              <xsl:otherwise>
                <i>none</i>
              </xsl:otherwise>
            </xsl:choose>
          </td>
        </tr>
      </xsl:for-each>
    </table>
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
        <xsl:when test="type='Autonumber'">
          <xsl:text>Integer (autonumbering)</xsl:text>
        </xsl:when>
        <xsl:otherwise>
          <xsl:call-template name="type-name-linked">
            <xsl:with-param name="type" select="type"/>
          </xsl:call-template>
        </xsl:otherwise>
      </xsl:choose>
    </dt>
    <dd>
      <xsl:if test="@refers">
        <p>
          <xsl:text>Formalizes </xsl:text>
          <a href="#{@relation}"><xsl:value-of select="@relation"/></a>
          <xsl:text>.</xsl:text>
        </p>
      </xsl:if>
      <xsl:apply-templates select="documentation"/>
    </dd>
  </xsl:template>


  <!-- Output Operation info. -->
  <xsl:template match="operation">
    <dt>
      <a name="{../name}.{name}"><xsl:value-of select="name"/></a>
      <xsl:if test="@abstract">
        <xsl:text> (abstract)</xsl:text>
      </xsl:if>
      <xsl:if test="@suppressed='framework'">
        <xsl:text> (automatically-generated)</xsl:text>
      </xsl:if>
      <xsl:if test="@class">
        <xsl:text> (class)</xsl:text>
      </xsl:if>
      <xsl:if test="@initialize">
        <xsl:text> (initialization)</xsl:text>
      </xsl:if>
      <xsl:if test="@finalize">
        <xsl:text> (finalization)</xsl:text>
      </xsl:if>
      <xsl:if test="@suppressed='instantiation'">
        <xsl:text> (instantiated)</xsl:text>
      </xsl:if>
      <xsl:if test="@handler">
        <xsl:text> (message handler)</xsl:text>
      </xsl:if>
      <xsl:if test="@suppressed='navigation'">
        <xsl:text> (navigation)</xsl:text>
      </xsl:if>
      <xsl:if test="@entry">
        <xsl:text> (task entry)</xsl:text>
      </xsl:if>
      <xsl:if test="@return">
        <xsl:text> returns </xsl:text>
        <xsl:call-template name="type-name-linked">
          <xsl:with-param name="type" select="@return"/>
        </xsl:call-template>
        <xsl:text></xsl:text>
      </xsl:if>
    </dt>
    <dd>
      <xsl:apply-templates select="documentation"/>
      <xsl:if test="parameter">
        <h5>Parameters</h5>
        <dl>
          <xsl:apply-templates select="parameter">
            <xsl:sort select="name"/>
          </xsl:apply-templates>
        </dl>
      </xsl:if>
    </dd>
    <p/>
  </xsl:template>


  <!-- Output details of an Operation's Parameter. -->
  <xsl:template match="operation/parameter">
    <dt>
      <xsl:value-of select="name"/>
      <xsl:text> : </xsl:text>
      <xsl:call-template name="type-name-linked">
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
      <p><xsl:text>Callback support is provided.</xsl:text></p>
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
      <xsl:when test="imported">
        <p>
          <xsl:text>Imported from </xsl:text>
          <tt><xsl:value-of select="imported"/></tt>
          <xsl:text>.</xsl:text>
        </p>
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
          <xsl:if test="real/digits">
            <li>
              <xsl:value-of select="real/digits"/>
              <xsl:text> significant digits</xsl:text>
            </li>
          </xsl:if>
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
        </ul>
      </xsl:when>
      <xsl:when test="renames">
        <p>
          <xsl:text>Renames </xsl:text>
          <tt><xsl:value-of select="renames"/></tt>
          <xsl:text>.</xsl:text>
        </p>
      </xsl:when>
      <xsl:when test="attribute">
        <xsl:choose>
          <xsl:when test="@extends">
            <p>
              <xsl:text>A record type, extending </xsl:text>
              <xsl:value-of select="@extends"/>
              <xsl:text>:</xsl:text>
            </p>
          </xsl:when>
          <xsl:otherwise>
            <p>A record type:</p>
          </xsl:otherwise>
        </xsl:choose>
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

  <!-- Output a type name, with a hyperlink to the definition if the
       type is a class or non-standard or an event.
       May be called at: attribute, operation, operation/parameter. -->

  <xsl:template name="type-name-linked">
    <xsl:param name="type" select="."/>

    <!-- XXX Need to find if I can look up the ancestor axis to see if
         there's an event with this name in the class; which would
         override any <<type>> declaration. -->

    <xsl:variable
      name="defined"
      select="/domain/type[name=$type]
              | /domain/class[name=$type]
              | /domain/class/event[name=$type]"/>

    <xsl:choose>

      <xsl:when test="../../event[name=$type]">
        <!-- an event -->
        <a href="#{../../name}.{$type}"><xsl:value-of select="$type"/></a>
      </xsl:when>

      <xsl:when test="/domain/type[name=$type]/standard">
        <!-- a standard type -->
        <xsl:value-of select="$type"/>
      </xsl:when>

      <xsl:when test="/domain/type[name=$type]
                      or /domain/class[name=$type]">
        <!-- a class or a type -->
        <a href="#{$type}"><xsl:value-of select="$type"/></a>
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

  <!-- Templates to catch elements missed in "mode" matches -->

  <xsl:template match="*" mode="index"/>

</xsl:stylesheet>
