<!-- $Id: generate-html.xsl,v 067eefe5b2c4 2008/07/08 21:50:19 simonjwright $ -->

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
  xmlns:at="http://pushface.org/coldframe/attribute"
  xmlns:ut="http://pushface.org/coldframe/utilities"
  extension-element-prefixes="saxon"
  version="1.0">

  <xsl:import href="ada-attribute.xsl"/>
  <xsl:import href="ada-utilities.xsl"/>

  <xsl:strip-space elements="*"/>

  <xsl:output 
    method="html"
    doctype-public="-//W3C/DTD HTML 4.01 Transitional//EN"
    doctype-system="http://www.w3.org/TR/html4/loose.dtd"/>

  <!-- Most of the parameters/variables here are needed by the
       imported stylesheets even though they are of no use in this
       context. -->

  <!-- +++++ Command line parameters. +++++ -->

  <!-- For identification info. -->
  <xsl:param name="coldframe-version" select="cf-DATE"/>

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

  <!-- Current working directory (absolute is best!). -->
  <xsl:param name="cwd" select="'.'"/>

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
    <html xmlns="http://www.w3.org/1999/xhtml" lang="en" xml:lang="en">
      <head>
        <meta 
          http-equiv="Content-type" 
          content="text/html; charset=iso-8859-1"/>
        <!--
        <link href="cf-gen-doc-styles.css" rel="stylesheet" type="text/css"/>
        -->
        <style type="text/css">
          * { margin : 0;
          padding : 0;
          }
          html { font-family : Verdana, Arial, Helvetica, sans-serif;
          font-size : 20pt;
          }
          body { font-size : 50%;
          margin-left : .5em;
          margin-right : .5em;
          color : black;
          background : white;
          }
          h1, h2, h3, h4, h5, h6 { margin : 0.1em; }
          h1 { font-size : 1.7em;
          text-transform : uppercase;
          letter-spacing :.2em;
          margin-top : .3em;
          }
          h2 { font-size : 1.5em;
          margin-top : .2em;
          }
          h3 { font-size : 1.3em; }
          h4 { font-size : 1.2em; }
          h5 { font-size : 1.1em; }
          h6 { font-size : 1em; }
          p { margin : .1em 0 .1em 1em; }
          dl { margin-left : 1em; }
          dl dl { margin-left : 2em; }
          ol, ul { margin-left : 2.2em; }
          table { margin-left : 1em;
          font-size : 100%; 
          border : thin solid black;
          border-collapse : collapse;
          empty-cells : show; }
          th { border : thin solid black;
          padding : 0.1em; }
          td { border : thin solid black;
          padding : 0.1em; }
          tt { font-family : Courier, monospace;
          font-size : 1.05em;
          }
          hr { margin-top : .5em; }
        </style>
        <title><xsl:value-of select="name"/></title>
      </head>
      <body>
        <h1><xsl:value-of select="name"/></h1>
        <xsl:apply-templates select="./documentation"/>
        <xsl:if test="revision">
          <p>
            <xsl:text>Revision: </xsl:text>
            <xsl:value-of select="revision"/>
          </p>
        </xsl:if>
        <h1>Contents</h1>
        <h2>All classes</h2>
        <xsl:copy-of 
          select="document(concat($cwd, '/', name, '.images/', name, '.overall.cmapx'))"/>
        <img 
          src="{name}.images/{name}.overall.png"
          border="0"
          ismap="true"
          usemap="#overall"
          alt="Class diagram for {name}"/>
        <xsl:if test="class[@public]">
          <h2>Public Classes</h2>
          <ul>
            <xsl:apply-templates select="class[@public]" mode="index">
              <xsl:sort select="name"/>
            </xsl:apply-templates>
          </ul>
        </xsl:if>
        <xsl:if test="class[not(@public)]">
          <h2>Private Classes</h2>
          <ul>
            <xsl:apply-templates select="class[not(@public)]" mode="index">
              <xsl:sort select="name"/>
            </xsl:apply-templates>
          </ul>
        </xsl:if>
        <xsl:if test="association">
          <h2>Associations</h2>
          <ul>
            <xsl:apply-templates select="association" mode="index">
              <xsl:sort select="name"/>
            </xsl:apply-templates>
          </ul>
        </xsl:if>
        <xsl:if test="inheritance">
          <h2>Inheritance relationships</h2>
          <ul>
            <xsl:apply-templates select="inheritance" mode="index">
              <xsl:sort select="name"/>
            </xsl:apply-templates>
          </ul>
        </xsl:if>
        <xsl:if test="type[not(@standard)]">
          <h2>Types</h2>
          <ul>
            <xsl:apply-templates select="type[not(@standard)]" mode="index">
              <xsl:sort select="name"/>
            </xsl:apply-templates>
          </ul>
        </xsl:if>
        <xsl:if test="exception">
          <h2>Exceptions</h2>
          <ul>
            <xsl:apply-templates select="exception" mode="index">
              <xsl:sort select="name"/>
            </xsl:apply-templates>
          </ul>
        </xsl:if>
        <!-- End of index -->
        <hr/>
        <xsl:if test="class[@public]">
          <h1>Public Classes</h1>
          <xsl:apply-templates select="class[@public]">
            <xsl:sort select="name"/>
          </xsl:apply-templates>
        </xsl:if>
        <xsl:if test="class[not(@public)]">
          <h1>Private Classes</h1>
          <xsl:apply-templates select="class[not(@public)]">
            <xsl:sort select="name"/>
          </xsl:apply-templates>
        </xsl:if>
        <xsl:if test="association">
          <h1>Associations</h1>
          <xsl:apply-templates select="association">
            <xsl:sort select="name"/>
          </xsl:apply-templates>
        </xsl:if>
        <xsl:if test="inheritance">
          <h1>Inheritance relationships</h1>
          <xsl:apply-templates select="inheritance">
            <xsl:sort select="name"/>
          </xsl:apply-templates>
        </xsl:if>
        <xsl:if test="type[not(@standard)]">
          <h1>Types</h1>
          <xsl:apply-templates select="type[not(@standard)]">
            <xsl:sort select="name"/>
          </xsl:apply-templates>
        </xsl:if>
        <xsl:if test="exception">
          <h1>Exceptions</h1>
          <xsl:apply-templates select="exception">
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
    <h2><a name="{$name}"><xsl:value-of select="$name"/></a></h2>

    <xsl:copy-of 
      select="document(concat($cwd, '/', ../name, '.images/', ../name, '.', name, '.class.cmapx'))"/>
    <img 
      src="{../name}.images/{../name}.{name}.class.png" 
      border="0"
      ismap="true"
      usemap="#{name}.class"
      alt="Context class diagram for {name}"/>

    <xsl:apply-templates select="documentation"/>

    <xsl:for-each select="../association[associative=$name]">
      <p>
        <xsl:text>Associative in </xsl:text>
        <a href="#{name}"><xsl:value-of select="name"/></a>.
      </p>
    </xsl:for-each>

    <xsl:variable 
      name="super"
      select="../inheritance[parent=$name]"/>
    <xsl:if test="$super">
      <p>
        <xsl:text>Supertype in </xsl:text>
        <a href="#{$super/name}"><xsl:value-of select="$super/name"/></a>.
      </p>
    </xsl:if>

    <xsl:for-each select="../inheritance[child=$name]">
      <p>
        <xsl:text>Subtype of </xsl:text>
        <a href="#{parent}"><xsl:value-of select="parent"/></a>
        <xsl:text> in </xsl:text>
        <a href="#{name}"><xsl:value-of select="name"/></a>.
      </p>
    </xsl:for-each>

    <!-- Only report attributes that have analyst-defined names
         (attributes created by normalization to implement
         relationships don't have names). -->
    <xsl:if test="attribute[name and @class]">
      <h3>Class attributes</h3>
      <dl>
        <xsl:apply-templates select="attribute[name and @class]">
          <xsl:sort select="."/>
        </xsl:apply-templates>
      </dl>
    </xsl:if>

    <xsl:if test="attribute[name and @identifier]">
      <h3>Identifying attributes</h3>
      <dl>
        <xsl:apply-templates select="attribute[name and @identifier]"/>
        <!-- Unsorted, to retain document (model) order. -->
      </dl>
    </xsl:if>

    <xsl:if test="attribute[name and not(@class or @identifier)]">
      <h3>Normal attributes</h3>
      <dl>
        <xsl:apply-templates select="attribute[name and not(@class or @identifier)]">
          <xsl:sort select="."/>
        </xsl:apply-templates>
      </dl>
    </xsl:if>

    <xsl:if test="event[@class]">
      <h3>Class events</h3>
      <dl>
        <xsl:for-each select="event[@class]">
          <xsl:sort select="name"/>
          <xsl:call-template name="class-event-details"/>
        </xsl:for-each>
      </dl>
    </xsl:if>
    <xsl:if test="statemachine">
      <h3>States</h3>
      <dl>
        <xsl:for-each select="statemachine/state">
          <xsl:sort select="not(@initial)"/>
          <xsl:sort select="name"/>
          <xsl:call-template name="state-details"/>
        </xsl:for-each>
      </dl>
      <h3>Events</h3>
      <dl>
        <xsl:for-each select="statemachine/event">
          <xsl:sort select="name"/>
          <xsl:call-template name="event-details"/>
        </xsl:for-each>
      </dl>
      <h3>State-Event Matrix</h3>
      <xsl:call-template name="state-machine"/>
      <h3>State Diagram</h3>
      <img
        src="{../name}.images/{../name}.{name}.state.png"
        border="0"
        alt="State diagram for {../name}.{name}"/>
    </xsl:if>

    <xsl:if test="operation[@class and @visibility='public']">
      <h3>Public class operations</h3>
      <dl>
        <xsl:apply-templates 
          select="operation[@class and @visibility='public']">
          <xsl:sort select="."/>
        </xsl:apply-templates>
      </dl>
    </xsl:if>

    <xsl:if test="operation[@class and @visibility='protected']">
      <h3>Protected class operations</h3>
      <dl>
        <xsl:apply-templates 
          select="operation[@class and @visibility='protected']">
          <xsl:sort select="."/>
        </xsl:apply-templates>
      </dl>
    </xsl:if>

    <xsl:if test="operation[@class and @visibility='private']">
      <h3>Private class operations</h3>
      <dl>
        <xsl:apply-templates 
          select="operation[@class and @visibility='private']">
          <xsl:sort select="."/>
        </xsl:apply-templates>
      </dl>
    </xsl:if>

    <xsl:if test="operation[@class and @visibility='implementation']">
      <h3>Implementation class operations </h3>
      <dl>
        <xsl:apply-templates 
          select="operation[@class and @visibility='implementation']">
          <xsl:sort select="."/>
        </xsl:apply-templates>
      </dl>
    </xsl:if>

    <xsl:if test="operation[not(@class) and @visibility='public']">
      <h3>Public instance operations</h3>
      <dl>
        <xsl:apply-templates 
          select="operation[not(@class) and @visibility='public']">
          <xsl:sort select="."/>
        </xsl:apply-templates>
      </dl>
    </xsl:if>

    <xsl:if test="operation[not(@class) and @visibility='protected']">
      <h3>Protected instance operations</h3>
      <dl>
        <xsl:apply-templates 
          select="operation[not(@class) and @visibility='protected']">
          <xsl:sort select="."/>
        </xsl:apply-templates>
      </dl>
    </xsl:if>

    <xsl:if test="operation[not(@class) and @visibility='private']">
      <h3>Private instance operations</h3>
      <dl>
        <xsl:apply-templates 
          select="operation[not(@class) and @visibility='private']">
          <xsl:sort select="."/>
        </xsl:apply-templates>
      </dl>
    </xsl:if>

    <xsl:if test="operation[not(@class) and @visibility='implementation']">
      <h3>Implementation instance operations</h3>
      <dl>
        <xsl:apply-templates 
          select="operation[not(@class) and @visibility='implementation']">
          <xsl:sort select="."/>
        </xsl:apply-templates>
      </dl>
    </xsl:if>

    <xsl:if
      test="../association[role/classname = $name]
            or ../association[associative = $name]">
      <h3>Associations</h3>
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
       in a Definition List context. -->
  <xsl:template name="state-details">
    <dt>
      <a name="{../../name}.{name}"><xsl:value-of select="name"/></a>
    </dt>
    <dd>
      <xsl:apply-templates select="documentation"/>
    </dd>
  </xsl:template>


  <!-- Output details of a class Event. Called at class/event
       in a Definition List context. -->
  <xsl:template name="class-event-details">
    <dt>
      <a name="{../name}.{name}"><xsl:value-of select="name"/></a>
      <xsl:if test="type">
        <xsl:text> (payload of type </xsl:text>
        <xsl:call-template name="type-name-linked">
          <xsl:with-param name="type" select="type"/>
        </xsl:call-template>
        <xsl:text>)</xsl:text>
      </xsl:if>
    </dt>
    <dd>
      <xsl:apply-templates select="documentation"/>
    </dd>
  </xsl:template>


  <!-- Output details of an instance Event. Called at statemachine/event
       in a Definition List context. -->
  <xsl:template name="event-details">
    <dt>
      <a name="{../../name}.{name}"><xsl:value-of select="name"/></a>
      <xsl:if test="../../event[name=current()/name]/type">
        <xsl:text> (payload of type </xsl:text>
        <xsl:call-template name="type-name-linked">
          <xsl:with-param
            name="type"
            select="../../event[name=current()/name]/type"/>
        </xsl:call-template>
        <xsl:text>)</xsl:text>
      </xsl:if>
    </dt>
    <dd>
      <xsl:apply-templates select="documentation"/>
    </dd>
  </xsl:template>


  <!-- Called at class to output a Class's state model. -->
  <xsl:template name="state-machine">
    <table border="1" summary="State machine for {name}">
      <tr>
        <th rowspan="2">State</th>
        <th rowspan="2">Entry Action(s)</th>
        <th colspan="{count(statemachine/event)}">Event</th>
        <th rowspan="2">Drop-through</th>
      </tr>
      <tr>
        <xsl:for-each select="statemachine/event">
          <xsl:sort select="name"/>
          <th><xsl:value-of select="name"/></th>
        </xsl:for-each>
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


  <!-- Output details of a Class's Attribute. -->
  <xsl:template match="attribute">
    <dt>
      <xsl:value-of select="name"/>
      <xsl:text> : </xsl:text>
      <xsl:choose>
        <!-- Don't report identifier, class: they have their own headings. -->
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
          <xsl:if test="@aliased">
            <xsl:text>, aliased</xsl:text>
          </xsl:if>
          <xsl:if test="@atomic">
            <xsl:text>, atomic</xsl:text>
          </xsl:if>
          <xsl:if test="@volatile">
            <xsl:text>, volatile</xsl:text>
          </xsl:if>
          <xsl:if test="initial">
            <xsl:text>, initial value </xsl:text>
            <xsl:value-of select="initial"/>
          </xsl:if>
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
        <xsl:text>, abstract</xsl:text>
      </xsl:if>
      <xsl:if test="@suppressed='framework'">
        <xsl:text>, automatically-generated</xsl:text>
      </xsl:if>
      <xsl:if test="@access">
        <xsl:text>, access-to-operation</xsl:text>
      </xsl:if>
      <xsl:if test="@convention">
        <xsl:text>, convention </xsl:text>
        <xsl:value-of select="@convention"/>
        <xsl:text></xsl:text>
      </xsl:if>
      <xsl:if test="@accessor">
        <xsl:text>, accessor</xsl:text>
      </xsl:if>
      <xsl:if test="@initialize">
        <xsl:text>, class initialization</xsl:text>
      </xsl:if>
      <xsl:if test="@finalize">
        <xsl:text>, instance finalization</xsl:text>
      </xsl:if>
      <xsl:if test="@final">
        <xsl:text>, action deletes instance</xsl:text>
      </xsl:if>
      <xsl:if test="@suppressed='instantiation'">
        <xsl:text>, instantiated</xsl:text>
      </xsl:if>
      <xsl:if test="@handler">
        <xsl:text>, class event handler</xsl:text>
      </xsl:if>
      <xsl:if test="@suppressed='navigation'">
        <xsl:text>, navigation</xsl:text>
      </xsl:if>
      <xsl:if test="@entry">
        <xsl:text>, entry</xsl:text>
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
        <p>
          <xsl:text>Parameters:</xsl:text>
        </p>
        <dl>
          <xsl:apply-templates select="parameter"/>
        </dl>
      </xsl:if>
    </dd>
  </xsl:template>


  <!-- Output details of an Operation's Parameter. -->
  <xsl:template match="operation/parameter">
    <dt>
      <xsl:value-of select="name"/>
      <xsl:text> : </xsl:text>
      <xsl:choose>
        <xsl:when test="@mode='inout'">
          <xsl:text>in out </xsl:text>
        </xsl:when>
        <xsl:when test="@mode='out'">
          <xsl:text>out </xsl:text>
        </xsl:when>
        <xsl:otherwise>
          <xsl:text>in </xsl:text>
        </xsl:otherwise>
      </xsl:choose>
      <xsl:call-template name="type-name-linked">
        <xsl:with-param name="type" select="type"/>
      </xsl:call-template>
      <xsl:if test="initial">
        <xsl:text>, default </xsl:text>
        <xsl:value-of select="initial"/>
      </xsl:if>
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
    <h2><a name="{name}"><xsl:value-of select="name"/></a></h2>
    <xsl:apply-templates select="documentation"/>
    <h3>Roles</h3>
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
    <xsl:apply-templates select="documentation"/>
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
    <h2><a name="{name}"><xsl:value-of select="name"/></a></h2>
    <xsl:apply-templates select="documentation"/>
    <h3>Superclass</h3>
    <p>
      <a href="#{parent}"><xsl:value-of select="parent"/></a>
    </p>
    <h3>Children</h3>
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
    <h2><a name="{name}"><xsl:value-of select="name"/></a></h2>
    <xsl:apply-templates select="documentation"/>
    <xsl:if test="@callback">
      <p><xsl:text>Callback support is provided.</xsl:text></p>
    </xsl:if>
    <xsl:choose>
      <xsl:when test="array/@unconstrained">
        <p>
          <xsl:text>An unconstrained array of </xsl:text>
          <xsl:call-template name="type-name-linked">
            <xsl:with-param name="type" select="array/type"/>
          </xsl:call-template>
          <xsl:text>, indexed by </xsl:text>
          <xsl:call-template name="type-name-linked">
            <xsl:with-param name="type" select="array/index"/>
          </xsl:call-template>
          <xsl:text>.</xsl:text>
        </p>
      </xsl:when>
      <xsl:when test="array">
        <p>
          <xsl:text>An array of </xsl:text>
          <xsl:call-template name="type-name-linked">
            <xsl:with-param name="type" select="array/type"/>
          </xsl:call-template>
          <xsl:text>, indexed by </xsl:text>
          <xsl:call-template name="type-name-linked">
            <xsl:with-param name="type" select="array/index"/>
          </xsl:call-template>
          <xsl:text>.</xsl:text>
        </p>
      </xsl:when>
      <xsl:when test="counterpart">
        <p>A counterpart for a ColdFrame class in or for another domain.</p>
      </xsl:when>
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
        <p>
          <xsl:text>An integral number, with lower bound </xsl:text>
          <xsl:choose>
            <xsl:when test="integer/lower">
              <xsl:value-of select="integer/lower"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:text>unspecified</xsl:text>
            </xsl:otherwise>
          </xsl:choose>
          <xsl:text> and upper bound </xsl:text>
          <xsl:choose>
            <xsl:when test="integer/upper">
              <xsl:value-of select="integer/upper"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:text>unspecified</xsl:text>
            </xsl:otherwise>
          </xsl:choose>
          <xsl:text>.</xsl:text>
        </p>
      </xsl:when>
      <xsl:when test="real">
        <p>
          <xsl:text>A real number, with </xsl:text>
          <xsl:choose>
            <xsl:when test="real/digits">
              <xsl:value-of select="real/digits"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:text>unspecified</xsl:text>
            </xsl:otherwise>
          </xsl:choose>
          <xsl:text> significant digits, lower bound </xsl:text>
          <xsl:choose>
            <xsl:when test="real/lower">
              <xsl:value-of select="real/lower"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:text>unspecified</xsl:text>
            </xsl:otherwise>
          </xsl:choose>
          <xsl:text> and upper bound </xsl:text>
          <xsl:choose>
            <xsl:when test="real/upper">
              <xsl:value-of select="real/upper"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:text>unspecified</xsl:text>
            </xsl:otherwise>
          </xsl:choose>
          <xsl:text>.</xsl:text>
        </p>
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
              <xsl:text>, with attributes:</xsl:text>
            </p>
          </xsl:when>
          <xsl:when test="@protected">
            <p>A protected record type, with attributes:</p>
          </xsl:when>
          <xsl:otherwise>
            <p>A record type, with attributes:</p>
          </xsl:otherwise>
        </xsl:choose>
        <dl>
          <xsl:apply-templates select="attribute">
            <xsl:sort select="."/>
          </xsl:apply-templates>
        </dl>
      </xsl:when>
      <xsl:when test="string/fixed">
        <p>
          <xsl:text>A string of fixed length </xsl:text>
          <xsl:value-of select="string/fixed"/>
          <xsl:text> characters.</xsl:text>
        </p>
      </xsl:when>
      <xsl:when test="string/max">
        <p>
          <xsl:text>A string of maximum length </xsl:text>
          <xsl:value-of select="string/max"/>
          <xsl:text> characters.</xsl:text>
        </p>
      </xsl:when>
      <xsl:when test="subtype">
        <p>
          <xsl:text>A constrained version  of </xsl:text>
          <a href="#{subtype/@constrains}">
            <xsl:value-of select="subtype/@constrains"/>
          </a>
          <xsl:text> from </xsl:text>
          <xsl:choose>
            <xsl:when test="subtype/lower">
              <xsl:value-of select="subtype/lower"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:text>its lowest value</xsl:text>
            </xsl:otherwise>
          </xsl:choose>
          <xsl:text> to </xsl:text>
          <xsl:choose>
            <xsl:when test="subtype/upper">
              <xsl:value-of select="subtype/upper"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:text>its highest value</xsl:text>
            </xsl:otherwise>
          </xsl:choose>
          <xsl:text>.</xsl:text>
        </p>
      </xsl:when>
      <xsl:when test="unsigned">
        <p>
          <xsl:text>An unsigned type, modulo </xsl:text>
          <xsl:value-of select="unsigned/mod"/>
          <xsl:text>.</xsl:text>
        </p>
      </xsl:when>
      <xsl:when test="@null">
        <p>
          <xsl:text>Empty record.</xsl:text>
        </p>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>(ColdFrame doesn't recognize this type)</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:if test="operation">
      <p>
        <xsl:value-of select="name"/>
        <xsl:text> has operations:</xsl:text>
      </p>
      <dl>
        <xsl:apply-templates select="operation">
          <xsl:sort select="."/>
        </xsl:apply-templates>
      </dl>
    </xsl:if>
  </xsl:template>


  <!-- Output details of an Exception. -->
  <xsl:template match="exception" mode="index">
    <li>
      <a href="#{name}"><xsl:value-of select="name"/></a>
    </li>
  </xsl:template>

  <xsl:template match="exception">
    <h2><a name="{name}"><xsl:value-of select="name"/></a></h2>
    <xsl:apply-templates select="documentation"/>
    <xsl:choose>
      <xsl:when test="@imported">
        <p>
          <xsl:text>Imported from </xsl:text>
          <tt><xsl:value-of select="@imported"/></tt>
          <xsl:text>.</xsl:text>
        </p>
      </xsl:when>
      <xsl:when test="@renames">
        <p>
          <xsl:text>Renames </xsl:text>
          <tt><xsl:value-of select="@renames"/></tt>
          <xsl:text>.</xsl:text>
        </p>
      </xsl:when>
    </xsl:choose>
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

      <xsl:when test="/domain/type[name=$type]/@standard">
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
