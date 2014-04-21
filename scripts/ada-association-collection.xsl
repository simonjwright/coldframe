<!-- $Id: ada-association-collection.xsl,v f6d9ce14c0aa 2014/04/21 15:48:31 simonjwright $ -->
<!-- XSL stylesheet to generate Ada code for Associations. -->
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

     As a special exception, when portions of this file are copied by
     a stylesheet processor into an output file, this file does not by
     itself cause the resulting file to be covered by the GNU General
     Public License.  This exception does not however invalidate any
     other reasons why the output file might be covered by the GNU
     Public License.
     -->

<!-- Generates support for navigation from collections (navigation from
     single handles is in ada-association.xsl). -->

<xsl:stylesheet
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:ac="http://pushface.org/coldframe/association-collection"
  xmlns:ut="http://pushface.org/coldframe/utilities"
  version="1.0">

  <!-- Generate specs for Association packages. -->
  <xsl:template
    match="domain/association"
    mode="ac:association-collection-spec">

    <xsl:if test="not(/domain/class[name=current()/role/classname]/@singleton)">

      <xsl:call-template name="ut:do-not-edit"/>
      <xsl:text>pragma Style_Checks (Off);&#10;</xsl:text>
      <xsl:call-template name="ut:identification-info"/>

      <!-- Context clauses. -->
      <xsl:call-template name="ac:association-collection-spec-context"/>

      <xsl:text>package </xsl:text>
      <xsl:value-of select="../name"/>.<xsl:value-of select="name"/>
      <xsl:text>.From_Vectors is&#10;</xsl:text>
      <xsl:value-of select="$blank-line"/>

      <!-- .. navigations .. -->
      <xsl:call-template name="ac:navigation-collection-specs"/>

      <!-- .. and close. -->
      <xsl:text>end </xsl:text>
      <xsl:value-of select="../name"/>.<xsl:value-of select="name"/>
      <xsl:text>.From_Vectors;&#10;</xsl:text>

    </xsl:if>

  </xsl:template>

  <xsl:template match="*" mode="ac:association-collection-spec"/>


  <!-- Called at domain/association to generate context clauses for the
       spec. -->
  <xsl:template name="ac:association-collection-spec-context">

    <xsl:text>with </xsl:text>
    <xsl:value-of select="../name"/>
    <xsl:text>.</xsl:text>
    <xsl:value-of select="role[1]/classname"/>
    <xsl:text>.Vectors;&#10;</xsl:text>

    <xsl:text>with </xsl:text>
    <xsl:value-of select="../name"/>
    <xsl:text>.</xsl:text>
    <xsl:value-of select="role[2]/classname"/>
    <xsl:text>.Vectors;&#10;</xsl:text>

    <xsl:if test="associative">
      <xsl:text>with </xsl:text>
      <xsl:value-of select="../name"/>
      <xsl:text>.</xsl:text>
      <xsl:value-of select="associative"/>
      <xsl:text>.Vectors;&#10;</xsl:text>
    </xsl:if>

  </xsl:template>


  <!-- Generate bodies for Association packages. -->
  <xsl:template
    match="domain/association"
    mode="ac:association-collection-body">

    <xsl:if test="not(/domain/class[name=current()/role/classname]/@singleton)">

      <xsl:call-template name="ut:do-not-edit"/>
      <xsl:text>pragma Style_Checks (Off);&#10;</xsl:text>
      <xsl:call-template name="ut:identification-info"/>

      <!-- No extra context clauses. -->

      <xsl:text>package body </xsl:text>
      <xsl:value-of select="../name"/>.<xsl:value-of select="name"/>
      <xsl:text>.From_Vectors is&#10;</xsl:text>
      <xsl:value-of select="$blank-line"/>

      <!-- .. navigations .. -->
      <xsl:call-template name="ac:navigation-collection-bodies"/>

      <!-- .. and close. -->
      <xsl:text>end </xsl:text>
      <xsl:value-of select="../name"/>.<xsl:value-of select="name"/>
      <xsl:text>.From_Vectors;&#10;</xsl:text>

    </xsl:if>

  </xsl:template>

  <xsl:template match="*" mode="ac:association-collection-body"/>


  <!-- Called at domain/association to generate the navigation function
       specs. -->
  <xsl:template name="ac:navigation-collection-specs">

    <xsl:variable name="role-1" select="role[1]"/>
    <xsl:variable
      name="singleton-1"
      select="/domain/class[name=$role-1/classname]/@singleton"/>
    <xsl:variable name="role-2" select="role[2]"/>
    <xsl:variable
      name="singleton-2"
      select="/domain/class[name=$role-2/classname]/@singleton"/>

    <!-- First direction : from collection -->

    <xsl:if test="not($singleton-1) and not($singleton-2)">
      <xsl:call-template name="ac:navigation-collection-specification">
        <xsl:with-param name="role-a" select="role[1]"/>
        <xsl:with-param name="role-b" select="role[2]"/>
      </xsl:call-template>
      <xsl:text>;&#10;</xsl:text>
      <xsl:value-of select="$blank-line"/>
    </xsl:if>

    <xsl:if test="associative">

      <xsl:if test="not($singleton-1)">
        <xsl:call-template
          name="ac:navigation-collection-to-associative-specification">
          <xsl:with-param name="role-a" select="role[1]"/>
          <xsl:with-param name="role-b" select="role[2]"/>
          <xsl:with-param name="assoc" select="associative"/>
        </xsl:call-template>
        <xsl:text>;&#10;</xsl:text>
        <xsl:value-of select="$blank-line"/>
      </xsl:if>

      <xsl:if test="not($singleton-2)">
        <xsl:call-template
          name="ac:navigation-collection-from-associative-specification">
          <xsl:with-param name="role-a" select="role[1]"/>
          <xsl:with-param name="role-b" select="role[2]"/>
          <xsl:with-param name="assoc" select="associative"/>
        </xsl:call-template>
        <xsl:text>;&#10;</xsl:text>
        <xsl:value-of select="$blank-line"/>
      </xsl:if>

    </xsl:if>

    <!-- Second direction: from collection -->

    <xsl:if test="not($singleton-1) and not($singleton-2)">
      <xsl:call-template name="ac:navigation-collection-specification">
        <xsl:with-param name="role-a" select="role[2]"/>
        <xsl:with-param name="role-b" select="role[1]"/>
      </xsl:call-template>
      <xsl:text>;&#10;</xsl:text>
      <xsl:value-of select="$blank-line"/>
    </xsl:if>

    <xsl:if test="associative">

      <xsl:if test="not($singleton-2)">
        <xsl:call-template
          name="ac:navigation-collection-to-associative-specification">
          <xsl:with-param name="role-a" select="role[2]"/>
          <xsl:with-param name="role-b" select="role[1]"/>
          <xsl:with-param name="assoc" select="associative"/>
        </xsl:call-template>
        <xsl:text>;&#10;</xsl:text>
        <xsl:value-of select="$blank-line"/>
      </xsl:if>

      <xsl:if test="not($singleton-1)">
        <xsl:call-template
          name="ac:navigation-collection-from-associative-specification">
          <xsl:with-param name="role-a" select="role[2]"/>
          <xsl:with-param name="role-b" select="role[1]"/>
          <xsl:with-param name="assoc" select="associative"/>
        </xsl:call-template>
        <xsl:text>;&#10;</xsl:text>
        <xsl:value-of select="$blank-line"/>
      </xsl:if>

    </xsl:if>

  </xsl:template>


  <!-- Called at domain/association to generate the navigation function
       bodies. -->
  <xsl:template name="ac:navigation-collection-bodies">

    <xsl:variable name="role-1" select="role[1]"/>
    <xsl:variable
      name="singleton-1"
      select="/domain/class[name=$role-1/classname]/@singleton"/>
    <xsl:variable name="role-2" select="role[2]"/>
    <xsl:variable
      name="singleton-2"
      select="/domain/class[name=$role-2/classname]/@singleton"/>

    <!-- First direction : from collection -->

    <xsl:if test="not($singleton-1) and not($singleton-2)">
      <xsl:call-template name="ac:navigation-collection-body">
        <xsl:with-param name="role-a" select="role[1]"/>
        <xsl:with-param name="role-b" select="role[2]"/>
      </xsl:call-template>
    </xsl:if>

    <xsl:if test="associative">

      <xsl:if test="not($singleton-1)">
        <xsl:call-template name="ac:navigation-collection-to-associative-body">
          <xsl:with-param name="role-a" select="role[1]"/>
          <xsl:with-param name="role-b" select="role[2]"/>
          <xsl:with-param name="assoc" select="associative"/>
        </xsl:call-template>
      </xsl:if>

      <xsl:if test="not($singleton-2)">
        <xsl:call-template name="ac:navigation-collection-from-associative-body">
          <xsl:with-param name="role-a" select="role[1]"/>
          <xsl:with-param name="role-b" select="role[2]"/>
          <xsl:with-param name="assoc" select="associative"/>
        </xsl:call-template>
      </xsl:if>

    </xsl:if>

    <!-- Second direction: from collection -->

    <xsl:if test="not($singleton-1) and not($singleton-2)">
      <xsl:call-template name="ac:navigation-collection-body">
        <xsl:with-param name="role-a" select="role[2]"/>
        <xsl:with-param name="role-b" select="role[1]"/>
      </xsl:call-template>
    </xsl:if>

    <xsl:if test="associative">

      <xsl:if test="not($singleton-2)">
        <xsl:call-template name="ac:navigation-collection-to-associative-body">
          <xsl:with-param name="role-a" select="role[2]"/>
          <xsl:with-param name="role-b" select="role[1]"/>
          <xsl:with-param name="assoc" select="associative"/>
        </xsl:call-template>
      </xsl:if>

      <xsl:if test="not($singleton-1)">
        <xsl:call-template name="ac:navigation-collection-from-associative-body">
          <xsl:with-param name="role-a" select="role[2]"/>
          <xsl:with-param name="role-b" select="role[1]"/>
          <xsl:with-param name="assoc" select="associative"/>
        </xsl:call-template>
      </xsl:if>

    </xsl:if>

  </xsl:template>


  <!-- Called at domain/association to generate a navigation-from-collection
       function body. -->
  <xsl:template name="ac:navigation-collection-body">
    <xsl:param name="role-a"/>   <!-- from this .. -->
    <xsl:param name="role-b"/>   <!-- .. to this -->

    <xsl:choose>
      <xsl:when test="associative">
        <xsl:call-template
          name="ac:navigation-collection-with-associative-body">
          <xsl:with-param name="role-a" select="$role-a"/>
          <xsl:with-param name="role-b" select="$role-b"/>
        </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
        <xsl:call-template
          name="ac:navigation-collection-without-associative-body">
          <xsl:with-param name="role-a" select="$role-a"/>
          <xsl:with-param name="role-b" select="$role-b"/>
        </xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>

  </xsl:template>


  <!-- Called at domain/association to generate a navigation-from-collection
       function body when there is an associative class. -->
  <xsl:template name="ac:navigation-collection-with-associative-body">
    <xsl:param name="role-a"/>   <!-- from this .. -->
    <xsl:param name="role-b"/>   <!-- .. to this -->

    <xsl:variable name="n" select="name"/>
    <xsl:variable name="a">
      <xsl:value-of select="$role-a/classname"/>
    </xsl:variable>
    <xsl:variable name="b">
      <xsl:value-of select="$role-b/classname"/>
    </xsl:variable>
    <xsl:variable name="associative" select="associative"/>
    <xsl:variable name="c">
      <xsl:value-of select="$associative"/>
    </xsl:variable>

    <xsl:call-template name="ac:navigation-collection-specification">
      <xsl:with-param name="role-a" select="$role-a"/>
      <xsl:with-param name="role-b" select="$role-b"/>
    </xsl:call-template>
    <xsl:text> is&#10;</xsl:text>

    <xsl:value-of select="$II"/>
    <xsl:value-of
      select="/domain/class[name=$associative]/abbreviation"/>
    <xsl:text> : constant </xsl:text>
    <xsl:value-of select="$c"/>
    <xsl:text>.Vectors.Vector&#10;</xsl:text>
    <xsl:value-of select="$IIC"/>
    <xsl:text>:= </xsl:text>
    <xsl:value-of select="$role-a/name"/>
    <xsl:text> (</xsl:text>
    <xsl:value-of
      select="/domain/class[name=$role-a/classname]/abbreviation"/>
    <xsl:text>);&#10;</xsl:text>

    <xsl:value-of select="$I"/>
    <xsl:text>begin&#10;</xsl:text>

    <xsl:value-of select="$II"/>
    <xsl:text>return </xsl:text>
    <xsl:value-of  select="$role-a/name"/>
    <xsl:text> (</xsl:text>
    <xsl:value-of
      select="/domain/class[name=$associative]/abbreviation"/>
    <xsl:text>);&#10;</xsl:text>

    <xsl:value-of select="$I"/>
    <xsl:text>end </xsl:text>
    <xsl:value-of select="$role-a/name"/>
    <xsl:text>;&#10;</xsl:text>
    <xsl:value-of select="$blank-line"/>

  </xsl:template>


  <!-- Called at domain/association to generate a navigation-from-collection
       function body when there is no associative class. -->
  <xsl:template name="ac:navigation-collection-without-associative-body">
    <xsl:param name="role-a"/>   <!-- from this .. -->
    <xsl:param name="role-b"/>   <!-- .. to this -->

    <xsl:variable name="n" select="name"/>
    <xsl:variable name="a">
      <xsl:value-of select="$role-a/classname"/>
    </xsl:variable>
    <xsl:variable name="b">
      <xsl:value-of select="$role-b/classname"/>
    </xsl:variable>

    <xsl:call-template name="ac:navigation-collection-specification">
      <xsl:with-param name="role-a" select="$role-a"/>
      <xsl:with-param name="role-b" select="$role-b"/>
    </xsl:call-template>
    <xsl:text> is&#10;</xsl:text>

    <xsl:choose>

      <xsl:when test="$role-a/@multiple">

        <xsl:call-template name="ac:navigation-collection-many-to-one-body">

          <xsl:with-param name="a" select="$a"/>
          <xsl:with-param name="b" select="$b"/>
          <xsl:with-param name="role-a" select="$role-a"/>

        </xsl:call-template>

      </xsl:when>

      <xsl:when test="$role-b/@multiple">

        <xsl:call-template name="ac:navigation-collection-one-to-many-body">

          <xsl:with-param name="a" select="$a"/>
          <xsl:with-param name="b" select="$b"/>
          <xsl:with-param name="role-a" select="$role-a"/>

        </xsl:call-template>

      </xsl:when>

      <xsl:otherwise>    <!-- one-to-one -->

        <xsl:call-template name="ac:navigation-collection-one-to-one-body">

          <xsl:with-param name="a" select="$a"/>
          <xsl:with-param name="b" select="$b"/>
          <xsl:with-param name="role-a" select="$role-a"/>

        </xsl:call-template>

      </xsl:otherwise>

    </xsl:choose>

  </xsl:template>


  <!-- Called at domain/association to generate an associative navigation
       function body. -->
  <xsl:template name="ac:navigation-collection-from-associative-body">
    <xsl:param name="role-a"/>
    <xsl:param name="role-b"/>   <!-- .. to this -->
    <xsl:param name="assoc"/>    <!-- from this .. -->

    <xsl:variable name="n" select="name"/>
    <xsl:variable name="a">
      <xsl:value-of select="$role-a/classname"/>
    </xsl:variable>
    <xsl:variable name="b">
      <xsl:value-of select="$role-b/classname"/>
    </xsl:variable>
    <xsl:variable name="c">
      <xsl:value-of select="$assoc"/>
    </xsl:variable>

    <xsl:call-template
      name="ac:navigation-collection-from-associative-specification">
      <xsl:with-param name="role-a" select="$role-a"/>
      <xsl:with-param name="role-b" select="$role-b"/>
      <xsl:with-param name="assoc" select="$assoc"/>
    </xsl:call-template>
    <xsl:text> is&#10;</xsl:text>

    <xsl:choose>

      <xsl:when test="$role-a/@multiple">

        <xsl:call-template name="ac:navigation-collection-many-to-one-body">

          <xsl:with-param name="a" select="$c"/>
          <xsl:with-param name="b" select="$b"/>
          <xsl:with-param name="role-a" select="$role-a"/>

        </xsl:call-template>

      </xsl:when>

      <xsl:otherwise>

        <xsl:call-template name="ac:navigation-collection-one-to-one-body">

          <xsl:with-param name="a" select="$c"/>
          <xsl:with-param name="b" select="$b"/>
          <xsl:with-param name="role-a" select="$role-a"/>

        </xsl:call-template>

      </xsl:otherwise>

    </xsl:choose>

  </xsl:template>


  <!-- Called at domain/association to generate an associative navigation
       function body. -->
  <xsl:template name="ac:navigation-collection-to-associative-body">
    <xsl:param name="role-a"/>   <!-- from this .. -->
    <xsl:param name="role-b"/>
    <xsl:param name="assoc"/>    <!-- .. to this -->

    <xsl:variable name="n" select="name"/>
    <xsl:variable name="a">
      <xsl:value-of select="$role-a/classname"/>
    </xsl:variable>
    <xsl:variable name="b">
      <xsl:value-of select="$role-b/classname"/>
    </xsl:variable>
    <xsl:variable name="c">
      <xsl:value-of select="$assoc"/>
    </xsl:variable>

    <xsl:call-template
      name="ac:navigation-collection-to-associative-specification">
      <xsl:with-param name="role-a" select="$role-a"/>
      <xsl:with-param name="role-b" select="$role-b"/>
      <xsl:with-param name="assoc" select="$assoc"/>
    </xsl:call-template>
    <xsl:text> is&#10;</xsl:text>

    <xsl:choose>

      <xsl:when test="$role-b/@multiple">

        <xsl:call-template name="ac:navigation-collection-one-to-many-body">

          <xsl:with-param name="a" select="$a"/>
          <xsl:with-param name="b" select="$c"/>
          <xsl:with-param name="role-a" select="$role-a"/>

        </xsl:call-template>

      </xsl:when>

      <xsl:otherwise>

        <xsl:call-template name="ac:navigation-collection-one-to-one-body">

          <xsl:with-param name="a" select="$a"/>
          <xsl:with-param name="b" select="$c"/>
          <xsl:with-param name="role-a" select="$role-a"/>

        </xsl:call-template>

      </xsl:otherwise>

    </xsl:choose>

  </xsl:template>


  <!-- Utilities. -->


  <!-- Called at domain/association to generate a navigation-from-collection
       function spec (no closing ";" or "is"). -->
  <xsl:template name="ac:navigation-collection-specification">

    <xsl:param name="role-a"/>
    <xsl:param name="role-b"/>

    <xsl:value-of select="$I"/>
    <xsl:text>function </xsl:text>
    <xsl:value-of select="$role-a/name"/>
    <xsl:text>&#10;</xsl:text>
    <xsl:value-of select="$IC"/>
    <xsl:text>(</xsl:text>
    <xsl:value-of select="/domain/class[name=$role-a/classname]/abbreviation"/>
    <xsl:text> : </xsl:text>
    <xsl:value-of select="$role-a/classname"/>
    <xsl:text>.Vectors.Vector)&#10;</xsl:text>
    <xsl:value-of select="$IC"/>
    <xsl:text>return </xsl:text>
    <xsl:value-of select="$role-b/classname"/>
    <xsl:text>.Vectors.Vector</xsl:text>

  </xsl:template>


  <!-- Called at domain/association to generate a navigation-from-collection
       function spec (no closing ";" or "is"). -->
  <xsl:template name="ac:navigation-collection-from-associative-specification">

    <xsl:param name="role-a"/>
    <xsl:param name="role-b"/>   <!-- .. to this -->
    <xsl:param name="assoc"/>    <!-- from this .. -->

    <xsl:value-of select="$I"/>
    <xsl:text>function </xsl:text>
    <xsl:value-of select="$role-a/name"/>
    <xsl:text>&#10;</xsl:text>
    <xsl:value-of select="$IC"/>
    <xsl:text>(</xsl:text>
    <xsl:value-of select="/domain/class[name=$assoc]/abbreviation"/>
    <xsl:text> : </xsl:text>
    <xsl:value-of select="$assoc"/>
    <xsl:text>.Vectors.Vector)&#10;</xsl:text>
    <xsl:value-of select="$IC"/>
    <xsl:text>return </xsl:text>
    <xsl:value-of select="$role-b/classname"/>
    <xsl:text>.Vectors.Vector</xsl:text>

  </xsl:template>


  <!-- Called at domain/association to generate an associative
       navigation-from-collection function spec (no closing ";" or "is"). -->
  <xsl:template name="ac:navigation-collection-to-associative-specification">

    <xsl:param name="role-a"/>   <!-- from this .. -->
    <xsl:param name="role-b"/>
    <xsl:param name="assoc"/>    <!-- .. to this -->

    <xsl:value-of select="$I"/>
    <xsl:text>function </xsl:text>
    <xsl:value-of select="$role-a/name"/>
    <xsl:text>&#10;</xsl:text>
    <xsl:value-of select="$IC"/>
    <xsl:text>(</xsl:text>
    <xsl:value-of select="/domain/class[name=$role-a/classname]/abbreviation"/>
    <xsl:text> : </xsl:text>
    <xsl:value-of select="$role-a/classname"/>
    <xsl:text>.Vectors.Vector)&#10;</xsl:text>
    <xsl:value-of select="$IC"/>
    <xsl:text>return </xsl:text>
    <xsl:value-of select="$assoc"/>
    <xsl:text>.Vectors.Vector</xsl:text>

  </xsl:template>


  <!-- Called at domain/association to generate the body for a one-to-one
       navigation. -->
  <xsl:template name="ac:navigation-collection-one-to-one-body">

    <xsl:param name="a"/>       <!-- name of source class -->
    <xsl:param name="b"/>       <!-- name of target class -->
    <xsl:param name="role-a"/>  <!-- node for source role -->

    <!-- from one to one
            In_It : {a}.Vectors.Cursor := {a-abbrev}.First;
            T : {b}.Handle;
            Result : {b}.Vectors.Vector;
            use {a}.Vectors;
            use {b};
         begin
            while In_It /= No_Element loop
               T := {role-a} (Element (In_It));
               if T /= null then
                  Append (Result, T);
               end if;
               Next (In_It);
            end loop;
            return Result;
         end {role-a};
         -->

    <xsl:value-of select="$II"/>
    <xsl:text>In_It : </xsl:text>
    <xsl:value-of select="$a"/>
    <xsl:text>.Vectors.Cursor := </xsl:text>
    <xsl:value-of select="../class[name=$a]/abbreviation"/>
    <xsl:text>.First;&#10;</xsl:text>

    <xsl:value-of select="$II"/>
    <xsl:text>T : </xsl:text>
    <xsl:value-of select="$b"/>
    <xsl:text>.Handle;&#10;</xsl:text>

    <xsl:value-of select="$II"/>
    <xsl:text>Result : </xsl:text>
    <xsl:value-of select="$b"/>
    <xsl:text>.Vectors.Vector;&#10;</xsl:text>

    <xsl:value-of select="$II"/>
    <xsl:text>use </xsl:text>
    <xsl:value-of select="$a"/>
    <xsl:text>.Vectors;&#10;</xsl:text>

    <xsl:value-of select="$II"/>
    <xsl:text>use </xsl:text>
    <xsl:value-of select="$b"/>
    <xsl:text>;&#10;</xsl:text>

    <xsl:value-of select="$I"/>
    <xsl:text>begin&#10;</xsl:text>

    <xsl:value-of select="$II"/>
    <xsl:text>while In_It /= No_Element loop&#10;</xsl:text>

    <xsl:value-of select="$III"/>
    <xsl:text>T := </xsl:text>
    <xsl:value-of select="$role-a/name"/>
    <xsl:text> (Element (In_It));&#10;</xsl:text>

    <xsl:value-of select="$III"/>
    <xsl:text>if T /= null then&#10;</xsl:text>

    <xsl:value-of select="$IIII"/>
    <xsl:text>Append (Result, T);&#10;</xsl:text>

    <xsl:value-of select="$III"/>
    <xsl:text>end if;&#10;</xsl:text>

    <xsl:value-of select="$III"/>
    <xsl:text>Next (In_It);&#10;</xsl:text>

    <xsl:value-of select="$II"/>
    <xsl:text>end loop;&#10;</xsl:text>

    <xsl:value-of select="$II"/>
    <xsl:text>return Result;&#10;</xsl:text>

    <xsl:value-of select="$I"/>
    <xsl:text>end </xsl:text>
    <xsl:value-of select="$role-a/name"/>
    <xsl:text>;&#10;</xsl:text>
    <xsl:value-of select="$blank-line"/>

  </xsl:template>


  <!-- Called at domain/association to generate the body for a many-to-one
       navigation. -->
  <xsl:template name="ac:navigation-collection-many-to-one-body">

    <xsl:param name="a"/>       <!-- name of source class -->
    <xsl:param name="b"/>       <!-- name of target class -->
    <xsl:param name="role-a"/>  <!-- node for source role -->

    <!-- from many to one
            In_It : {a}.Vectors.Cursor := {a-abbrev}.First;
            T : {b}.Handle;
            Result : {b}.Vectors.Vector;
            use {a}.Vectors;
            use {b};
         begin
            while In_It /= No_Element loop
               T := {role-a} (Element (In_It));
               if T /= null and then not Result.Contains (T) then
                   Result.Append ( T);
               end if;
               Next (In_It);
            end loop;
            return Result;
         end {role-a};
         -->

    <xsl:value-of select="$II"/>
    <xsl:text>In_It : </xsl:text>
    <xsl:value-of select="$a"/>
    <xsl:text>.Vectors.Cursor := </xsl:text>
    <xsl:value-of select="../class[name=$a]/abbreviation"/>
    <xsl:text>.First;&#10;</xsl:text>

    <xsl:value-of select="$II"/>
    <xsl:text>T : </xsl:text>
    <xsl:value-of select="$b"/>
    <xsl:text>.Handle;&#10;</xsl:text>

    <xsl:value-of select="$II"/>
    <xsl:text>Result : </xsl:text>
    <xsl:value-of select="$b"/>
    <xsl:text>.Vectors.Vector;&#10;</xsl:text>

    <xsl:value-of select="$II"/>
    <xsl:text>use </xsl:text>
    <xsl:value-of select="$a"/>
    <xsl:text>.Vectors;&#10;</xsl:text>

    <xsl:value-of select="$II"/>
    <xsl:text>use </xsl:text>
    <xsl:value-of select="$b"/>
    <xsl:text>;&#10;</xsl:text>

    <xsl:value-of select="$I"/>
    <xsl:text>begin&#10;</xsl:text>

    <xsl:value-of select="$II"/>
    <xsl:text>while In_It /= No_Element loop&#10;</xsl:text>

    <xsl:value-of select="$III"/>
    <xsl:text>T := </xsl:text>
    <xsl:value-of select="$role-a/name"/>
    <xsl:text> (Element (In_It));&#10;</xsl:text>

    <xsl:value-of select="$III"/>
    <xsl:text>if T /= null and then not Result.Contains (T) then&#10;</xsl:text>

    <xsl:value-of select="$IIII"/>
    <xsl:text>Result.Append (T);&#10;</xsl:text>

    <xsl:value-of select="$III"/>
    <xsl:text>end if;&#10;</xsl:text>

    <xsl:value-of select="$III"/>
    <xsl:text>Next (In_It);&#10;</xsl:text>

    <xsl:value-of select="$II"/>
    <xsl:text>end loop;&#10;</xsl:text>

    <xsl:value-of select="$II"/>
    <xsl:text>return Result;&#10;</xsl:text>

    <xsl:value-of select="$I"/>
    <xsl:text>end </xsl:text>
    <xsl:value-of select="$role-a/name"/>
    <xsl:text>;&#10;</xsl:text>
    <xsl:value-of select="$blank-line"/>

  </xsl:template>


  <!-- Called at domain/association to generate the body for a one-to-many
       navigation. -->
  <xsl:template name="ac:navigation-collection-one-to-many-body">

    <xsl:param name="a"/>       <!-- name of source class -->
    <xsl:param name="b"/>       <!-- name of target class -->
    <xsl:param name="role-a"/>  <!-- node for source role -->

    <!-- from one to many (=> no need to check for multiple B's)
            In_It : {a}.Vectors.Cursor := {a-abbrev}.First;
            Result : {b}.Vectors.Vector;
            use {a}.Vectors;
         begin
            while In_It /= No_Element loop
               Result.Append ({role-a} (Element (In_It));
               Next (In_It);
            end loop;
            return Result;
         end {role-a};
         -->

    <xsl:value-of select="$II"/>
    <xsl:text>In_It : </xsl:text>
    <xsl:value-of select="$a"/>
    <xsl:text>.Vectors.Cursor := </xsl:text>
    <xsl:value-of select="../class[name=$a]/abbreviation"/>
    <xsl:text>.First;&#10;</xsl:text>

    <xsl:value-of select="$II"/>
    <xsl:text>Result : </xsl:text>
    <xsl:value-of select="$b"/>
    <xsl:text>.Vectors.Vector;&#10;</xsl:text>

    <xsl:value-of select="$II"/>
    <xsl:text>use </xsl:text>
    <xsl:value-of select="$a"/>
    <xsl:text>.Vectors;&#10;</xsl:text>

    <xsl:value-of select="$I"/>
    <xsl:text>begin&#10;</xsl:text>

    <xsl:value-of select="$II"/>
    <xsl:text>while In_It /= No_Element loop&#10;</xsl:text>

    <xsl:value-of select="$III"/>
    <xsl:text>Result.Append (</xsl:text>
    <xsl:value-of select="$role-a/name"/>
    <xsl:text> (Element (In_It)));&#10;</xsl:text>

    <xsl:value-of select="$III"/>
    <xsl:text>Next (In_It);&#10;</xsl:text>

    <xsl:value-of select="$II"/>
    <xsl:text>end loop;&#10;</xsl:text>

    <xsl:value-of select="$II"/>
    <xsl:text>return Result;&#10;</xsl:text>

    <xsl:value-of select="$I"/>
    <xsl:text>end </xsl:text>
    <xsl:value-of select="$role-a/name"/>
    <xsl:text>;&#10;</xsl:text>
    <xsl:value-of select="$blank-line"/>

  </xsl:template>


</xsl:stylesheet>
