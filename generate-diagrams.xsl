<!-- $Id: generate-diagrams.xsl,v f962166725ae 2008/06/29 17:53:01 simonjwright $ -->

<!-- XSL stylesheet to generate documentation diagrams. -->
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
  version="1.1">

  <xsl:strip-space elements="*"/>

  <xsl:output method="text"/>


  <xsl:template match="domain">
    <xsl:variable name="dir">
      <xsl:value-of select="name"/>
      <xsl:text>.images/</xsl:text>
    </xsl:variable>
    <xsl:text>cd </xsl:text>
    <xsl:value-of select="$dir"/>
    <xsl:text>&#10;</xsl:text>
    <xsl:call-template name="overall-diagram"/>
    <xsl:variable name="overall-filename">
      <xsl:value-of select="name"/>
      <xsl:text>.overall</xsl:text>
    </xsl:variable>
    <xsl:text>${DOT:-dot} -Tpng -o</xsl:text>
    <xsl:value-of select="$overall-filename"/>
    <xsl:text>.png -Tcmapx -o</xsl:text>
    <xsl:value-of select="$overall-filename"/>
    <xsl:text>.cmapx </xsl:text>
    <xsl:value-of select="$overall-filename"/>
    <xsl:text>.dot&#10;</xsl:text>
    <xsl:for-each select="class">
      <xsl:call-template name="class-diagram"/>
      <xsl:variable name="filename">
        <xsl:value-of select="../name"/>
        <xsl:text>.</xsl:text>
        <xsl:value-of select="name"/>
        <xsl:text>.class</xsl:text>
      </xsl:variable>
      <xsl:text>${CIRCO:-circo} -Tpng -o</xsl:text>
      <xsl:value-of select="$filename"/>
      <xsl:text>.png -Tcmapx -o</xsl:text>
      <xsl:value-of select="$filename"/>
      <xsl:text>.cmapx </xsl:text>
      <xsl:value-of select="$filename"/>
      <xsl:text>.dot&#10;</xsl:text>
    </xsl:for-each>
    <xsl:for-each select="class[statemachine]">
      <xsl:call-template name="state-diagram"/>
      <xsl:variable name="filename">
        <xsl:value-of select="../name"/>
        <xsl:text>.</xsl:text>
        <xsl:value-of select="name"/>
        <xsl:text>.state</xsl:text>
      </xsl:variable>
      <xsl:text>${DOT:-dot} -Tpng -o</xsl:text>
      <xsl:value-of select="$filename"/>
      <xsl:text>.png </xsl:text>
      <xsl:value-of select="$filename"/>
      <xsl:text>.dot&#10;</xsl:text>
    </xsl:for-each>
  </xsl:template>


  <!-- Called at domain to output the overall class diagram as a dot
       file. -->
  <xsl:template name="overall-diagram">
    <xsl:document href="{name}.images/{name}.overall.dot">
      digraph "overall" {
      edge [fontsize=10];
      node [shape=record, style=filled, fillcolor=moccasin, fontsize=10];
      <xsl:for-each select="class[@public]">
        <xsl:sort select="name"/>
        <xsl:value-of select="name"/>
        <xsl:text> [fillcolor=green, URL="#</xsl:text>
        <xsl:value-of select="name"/>
        <xsl:text>"]&#10;</xsl:text>
      </xsl:for-each>
      <xsl:for-each select="class[not(@public)]">
        <xsl:sort select="name"/>
        <xsl:value-of select="name"/>
        <xsl:text> [URL="#</xsl:text>
        <xsl:value-of select="name"/>
        <xsl:text>"]&#10;</xsl:text>
      </xsl:for-each>
      <xsl:for-each select="association">
        <xsl:choose>
          <xsl:when test="associative">
            <xsl:value-of select="name"/>
            <xsl:text> [shape=point, URL="#</xsl:text>
            <xsl:value-of select="name"/>
            <xsl:text>"]&#10;</xsl:text>

            <xsl:value-of select="role[1]/classname"/>
            <xsl:text> -> </xsl:text>
            <xsl:value-of select="name"/>
            <xsl:text> [label="</xsl:text>
            <xsl:value-of select="name"/>
            <xsl:text>", arrowhead=none, URL="#</xsl:text>
            <xsl:value-of select="name"/>
            <xsl:text>"]&#10;</xsl:text>

            <xsl:value-of select="role[2]/classname"/>
            <xsl:text> -> </xsl:text>
            <xsl:value-of select="name"/>
            <xsl:text> [label="</xsl:text>
            <xsl:value-of select="name"/>
            <xsl:text>", arrowhead=none, URL="#</xsl:text>
            <xsl:value-of select="name"/>
            <xsl:text>"]&#10;</xsl:text>

            <xsl:value-of select="name"/>
            <xsl:text> -> </xsl:text>
            <xsl:value-of select="associative"/>
            <xsl:text> [color=blue, label="</xsl:text>
            <xsl:value-of select="name"/>
            <xsl:text>", arrowhead=none, URL="#</xsl:text>
            <xsl:value-of select="name"/>
            <xsl:text>"]&#10;</xsl:text>
          </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="role[1]/classname"/>
            <xsl:text> -> </xsl:text>
            <xsl:value-of select="role[2]/classname"/>
            <xsl:text> [label="</xsl:text>
            <xsl:value-of select="name"/>
            <xsl:text>", arrowhead=none, URL="#</xsl:text>
            <xsl:value-of select="name"/>
            <xsl:text>"]&#10;</xsl:text>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:for-each>
      <xsl:for-each select="inheritance">
        <xsl:for-each select="child">
          <!-- Output in reverse order with the arrow on the "wrong"
               end, so parents appear at the top. -->
          <xsl:value-of select="../parent"/>
          <xsl:text> -> </xsl:text>
          <xsl:value-of select="."/>
          <xsl:text> [label="</xsl:text>
          <xsl:value-of select="../name"/>
          <xsl:text>", arrowhead=none, arrowtail=onormal, URL="#</xsl:text>
          <xsl:value-of select="../name"/>
          <xsl:text>"]&#10;</xsl:text>
        </xsl:for-each>
      </xsl:for-each>
      }
    </xsl:document>
  </xsl:template>


  <!-- Called at class to output the class and its immediate
       neighbours as a dot file (for use with circo). -->
  <xsl:template name="class-diagram">
    <xsl:variable name="n" select="name"/>
    <xsl:document href="{../name}.images/{../name}.{$n}.class.dot">
      <xsl:text>digraph "</xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text>.class" {
      graph [root=</xsl:text>
      <xsl:value-of select="$n"/>
      <xsl:text>]</xsl:text>
      edge [fontsize=10]
      node [shape=record, style=filled, fillcolor=lightgray, fontsize=10]

      <!-- This class. -->
      <xsl:value-of select="$n"/>
      <xsl:text> [</xsl:text>
      <xsl:choose>
        <xsl:when test="@public">
          <xsl:text>fillcolor=green, </xsl:text>
        </xsl:when>
        <xsl:otherwise>
          <xsl:text>fillcolor=moccasin, </xsl:text>
        </xsl:otherwise>
      </xsl:choose>
      <xsl:text>URL="#</xsl:text>
      <xsl:value-of select="$n"/>
      <xsl:text>", label="{</xsl:text>
      <xsl:value-of select="$n"/>
      <xsl:text>|</xsl:text>
      <xsl:for-each select="attribute[name]">
        <xsl:sort select="not(@class)"/>
        <xsl:sort select="not(@identifier)"/>
        <xsl:sort select="name"/>
        <xsl:if test="position() &gt; 1">
          <xsl:text>\n</xsl:text>
        </xsl:if>
        <xsl:if test="@class">
          <xsl:text>&#171;class&#187; </xsl:text>
        </xsl:if>
        <xsl:if test="@identifier">
          <xsl:text>&#171;id&#187; </xsl:text>
        </xsl:if>
        <xsl:value-of select="name"/>
        <xsl:text> : </xsl:text>
        <xsl:value-of select="type"/>
      </xsl:for-each>
      <xsl:text>|</xsl:text>
      <xsl:for-each select="operation">
        <xsl:sort select="not(@class)"/>
        <xsl:sort select="name"/>
        <xsl:if test="position() &gt; 1">
          <xsl:text>\n</xsl:text>
        </xsl:if>
        <xsl:choose>
          <xsl:when test="@visibility='public'">
            <xsl:text>+ </xsl:text>
          </xsl:when>
          <xsl:when test="@visibility='protected'">
            <xsl:text># </xsl:text>
          </xsl:when>
          <xsl:when test="@visibility='private'">
            <xsl:text>- </xsl:text>
          </xsl:when>
          <xsl:when test="@visibility='implementation'">
            <xsl:text>! </xsl:text>
          </xsl:when>
        </xsl:choose>
        <xsl:if test="@class">
          <xsl:text>&#171;class&#187; </xsl:text>
        </xsl:if>
        <xsl:value-of select="name"/>
        <xsl:text>()</xsl:text>
        <xsl:if test="@return">
          <xsl:text>: </xsl:text>
          <xsl:value-of select="@return"/>
        </xsl:if>
      </xsl:for-each>
      <xsl:text>}"]&#10;</xsl:text>
      <!-- Find & output related classes (once each). -->
      <xsl:variable name="related">
        <xsl:element name="related">
          <xsl:for-each select="../association[role/classname=$n]">
            <xsl:for-each select="current()/role/classname">
              <xsl:element name="name">
                <xsl:value-of select="."/>
              </xsl:element>
            </xsl:for-each>
            <xsl:for-each select="current()/associative">
              <xsl:element name="name">
                <xsl:value-of select="."/>
              </xsl:element>
            </xsl:for-each>
          </xsl:for-each>
          <xsl:for-each select="../association[associative=$n]">
            <xsl:element name="name">
              <xsl:value-of select="associative"/>
            </xsl:element>
            <xsl:for-each select="current()/role/classname">
              <xsl:element name="name">
                <xsl:value-of select="."/>
              </xsl:element>
            </xsl:for-each>
          </xsl:for-each>
          <xsl:for-each select="../inheritance[parent=$n]">
            <xsl:for-each select="child">
              <xsl:element name="name">
                <xsl:value-of select="."/>
              </xsl:element>
            </xsl:for-each>
          </xsl:for-each>
          <xsl:for-each select="../inheritance[child=$n]">
            <xsl:element name="name">
              <xsl:value-of select="parent"/>
            </xsl:element>
          </xsl:for-each>
        </xsl:element>
      </xsl:variable>
      <xsl:for-each select="$related/related/name">
        <xsl:sort select="."/>
        <xsl:if test="not(.=$n) and not(.=preceding-sibling::node())">
          <xsl:value-of select="."/>
          <xsl:text> [</xsl:text>
          <xsl:if test="/domain/class[name=current()]/@public">
            <!-- relationships with public classes?! -->
            <xsl:text>fillcolor=green, </xsl:text>
          </xsl:if>
          <xsl:text>URL="#</xsl:text>
          <xsl:value-of select="."/>
          <xsl:text>"]&#10;</xsl:text>
        </xsl:if>
      </xsl:for-each>
      <!-- Output relationships as in overall diagram. -->
      <xsl:for-each select="../association[role/classname=$n 
                            or associative=$n]">
        <xsl:choose>
          <xsl:when test="associative">
            <xsl:value-of select="name"/>
            <xsl:text> [shape=point, URL="#</xsl:text>
            <xsl:value-of select="name"/>
            <xsl:text>"]&#10;</xsl:text>

            <xsl:value-of select="role[1]/classname"/>
            <xsl:text> -> </xsl:text>
            <xsl:value-of select="name"/>
            <xsl:text> [label="</xsl:text>
            <xsl:value-of select="name"/>
            <xsl:text>", arrowhead=none, URL="#</xsl:text>
            <xsl:value-of select="name"/>
            <xsl:text>"]&#10;</xsl:text>

            <xsl:value-of select="role[2]/classname"/>
            <xsl:text> -> </xsl:text>
            <xsl:value-of select="name"/>
            <xsl:text> [label="</xsl:text>
            <xsl:value-of select="name"/>
            <xsl:text>", arrowhead=none, URL="#</xsl:text>
            <xsl:value-of select="name"/>
            <xsl:text>"]&#10;</xsl:text>

            <xsl:value-of select="name"/>
            <xsl:text> -> </xsl:text>
            <xsl:value-of select="associative"/>
            <xsl:text> [color=blue, label="</xsl:text>
            <xsl:value-of select="name"/>
            <xsl:text>", arrowhead=none, URL="#</xsl:text>
            <xsl:value-of select="name"/>
            <xsl:text>"]&#10;</xsl:text>
          </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="role[1]/classname"/>
            <xsl:text> -> </xsl:text>
            <xsl:value-of select="role[2]/classname"/>
            <xsl:text> [label="</xsl:text>
            <xsl:value-of select="name"/>
            <xsl:text>", arrowhead=none, URL="#</xsl:text>
            <xsl:value-of select="name"/>
            <xsl:text>"]&#10;</xsl:text>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:for-each>
      <xsl:for-each select="../inheritance[parent=$n]">
        <!-- All the children of (each) inheritance relationship in
             which this class is the parent. -->
        <xsl:for-each select="child">
          <!-- Output in reverse order with the arrow on the "wrong"
               end, so parents appear at the top. -->
          <xsl:value-of select="../parent"/>
          <xsl:text> -> </xsl:text>
          <xsl:value-of select="."/>
          <xsl:text> [label="</xsl:text>
          <xsl:value-of select="../name"/>
          <xsl:text>", arrowhead=none, arrowtail=onormal, URL="#</xsl:text>
          <xsl:value-of select="../name"/>
          <xsl:text>"]&#10;</xsl:text>
        </xsl:for-each>
      </xsl:for-each>
      <xsl:for-each select="../inheritance[child=$n]">
        <!-- The parent of (each) inheritance relationship in
             which this class is a child. -->
        <!-- Output in reverse order with the arrow on the "wrong"
             end, so parents appear at the top. -->
        <xsl:value-of select="parent"/>
        <xsl:text> -> </xsl:text>
        <xsl:value-of select="$n"/>
        <xsl:text> [label="</xsl:text>
        <xsl:value-of select="name"/>
        <xsl:text>", arrowhead=none, arrowtail=onormal, URL="#</xsl:text>
        <xsl:value-of select="name"/>
        <xsl:text>"]&#10;</xsl:text>
      </xsl:for-each>
      }
     </xsl:document>
  </xsl:template>


  <!-- Called at class to output the class's state diagram as a dot
       file. -->
  <xsl:template name="state-diagram">
    <xsl:document href="{../name}.images/{../name}.{name}.state.dot">
      digraph {
      edge [fontsize=10];
      node [shape=record, style=filled, fillcolor=moccasin, fontsize=10];
      <xsl:for-each select="statemachine/state">
        <xsl:sort select="not(@initial)"/>
        <xsl:sort select="name"/>
        <xsl:value-of select="name"/>
        <xsl:choose>
          <xsl:when test="@initial">
            <xsl:text>[shape=point, fillcolor=black, width=0.1];&#10;</xsl:text>
          </xsl:when>
          <xsl:when test="@final">
            <xsl:text>[shape=point, fillcolor=none, width=0.1];&#10;</xsl:text>
          </xsl:when>
          <xsl:otherwise>
            <xsl:text>[label="{</xsl:text>
            <xsl:value-of select="name"/>
            <xsl:if test="action">
              <xsl:text>|</xsl:text>
              <xsl:for-each select="action">
                <xsl:if test="position() &gt; 1">
                  <xsl:text>\n</xsl:text>
                </xsl:if>
                <xsl:value-of select="."/>
              </xsl:for-each>
            </xsl:if>
            <xsl:text>}"];&#10;</xsl:text>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:for-each>
      <xsl:for-each select="statemachine/transition">
        <xsl:value-of select="source"/>
        <xsl:text>-></xsl:text>
        <xsl:value-of select="target"/>
        <xsl:if test="event or action">
          <xsl:text>[label="</xsl:text>
          <xsl:if test="@ignore">
            <xsl:text>&#171;ignore&#187;    \n</xsl:text>
          </xsl:if>
          <xsl:if test="@self">
            <xsl:text>&#171;self&#187;    \n</xsl:text>
          </xsl:if>
          <xsl:value-of select="event"/>
          <xsl:text>    </xsl:text>
          <xsl:if test="action">
            <xsl:text>\n/</xsl:text>
            <xsl:value-of select="action"/>
            <xsl:text>    </xsl:text>
          </xsl:if>
          <xsl:text>"]</xsl:text>
        </xsl:if>
        <xsl:text>;&#10;</xsl:text>
      </xsl:for-each>
      }
    </xsl:document>
  </xsl:template>


</xsl:stylesheet>
