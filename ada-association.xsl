<!-- $Id: ada-association.xsl,v b806c3ce4d90 2001/04/23 05:20:49 simon $ -->
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

     As a special exception, when portions of this file are copied by a
     stylesheet processor into an output file, you may use that output
     file without restriction.
     -->

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="1.0">

  <!-- Generate specs for Association packages. -->
  <xsl:template match="domain/association" mode="association-spec">

    <!-- Context clauses. -->
    <xsl:call-template name="association-spec-context"/>

    <xsl:text>private package </xsl:text>
    <xsl:value-of select="../name"/>.<xsl:value-of select="name"/>
    <xsl:text> is&#10;</xsl:text>

    <!-- Linking subprogram .. -->
    <xsl:call-template name="link-spec"/>

    <!-- .. unlinking procedure .. -->
    <xsl:call-template name="unlink-spec"/>

    <!-- .. navigations .. -->
    <xsl:call-template name="navigation-specs"/>

    <!-- .. and close. -->
    <xsl:text>end </xsl:text>
    <xsl:value-of select="../name"/>.<xsl:value-of select="name"/>
    <xsl:text>;&#10;</xsl:text>

  </xsl:template>

  <xsl:template match="*" mode="association-spec"/>


  <!-- Called at domain/association to generate context clauses for the
       spec. -->
  <xsl:template name="association-spec-context">

    <xsl:for-each select="role/classname | associative">
      <xsl:sort select="."/>
      <xsl:text>with </xsl:text>
      <xsl:value-of select="/domain/name"/>.<xsl:value-of select="."/>
      <xsl:text>.Collections;&#10;</xsl:text>
    </xsl:for-each>

  </xsl:template>


  <!-- Called at domain/association to generate the linking subprogram spec -->
  <xsl:template name="link-spec">

    <xsl:choose>

      <xsl:when test="associative">
        <xsl:call-template name="link-function-specification"/>
      </xsl:when>

      <xsl:otherwise>
        <xsl:call-template name="link-procedure-specification"/>
      </xsl:otherwise>

    </xsl:choose>

    <xsl:text>;&#10;</xsl:text>

  </xsl:template>


  <!-- Called at domain/association to generate the unlinking procedure
       spec -->
  <xsl:template name="unlink-spec">

    <xsl:choose>

      <xsl:when test="associative">
        <xsl:call-template name="unlink-associative-specification"/>
      </xsl:when>

      <xsl:otherwise>
        <xsl:call-template name="unlink-specification"/>
      </xsl:otherwise>

    </xsl:choose>

    <xsl:text>;&#10;</xsl:text>

  </xsl:template>


  <!-- Generate bodies for Association packages. -->
  <xsl:template match="domain/association" mode="association-body">

    <!-- Context clauses. -->
    <xsl:call-template name="association-body-context"/>

    <xsl:text>package body </xsl:text>
    <xsl:value-of select="../name"/>.<xsl:value-of select="name"/>
    <xsl:text> is&#10;</xsl:text>

    <!-- Linking subprogram .. -->
    <xsl:call-template name="link-body"/>

    <!-- .. unlinking procedure .. -->
    <xsl:call-template name="unlink-body"/>

    <!-- .. navigations .. -->
    <xsl:call-template name="navigation-bodies"/>

    <!-- .. and close. -->
    <xsl:text>end </xsl:text>
    <xsl:value-of select="../name"/>.<xsl:value-of select="name"/>
    <xsl:text>;&#10;</xsl:text>

  </xsl:template>

  <xsl:template match="*" mode="association-body"/>


  <!-- Called at domain/association to generate context clauses for the
       body. -->
  <xsl:template name="association-body-context">

    <xsl:for-each select="role/classname | associative">
      <xsl:sort select="."/>
      <xsl:text>with </xsl:text>
      <xsl:value-of select="/domain/name"/>.<xsl:value-of select="."/>
      <xsl:text>.Abstract_Containers;&#10;</xsl:text>
      <xsl:text>with </xsl:text>
      <xsl:value-of select="/domain/name"/>.<xsl:value-of select="."/>
      <xsl:text>.Selection_Function;&#10;</xsl:text>
    </xsl:for-each>

  </xsl:template>


  <!-- Called at domain/association to generate the linking subprogram body -->
  <xsl:template name="link-body">
    <xsl:choose>

      <xsl:when test="associative">

        <xsl:call-template name="link-function-specification"/>
        <xsl:text> is&#10;</xsl:text>

        <xsl:text>    Result : </xsl:text>
        <xsl:value-of select="/domain/name"/>
        <xsl:text>.</xsl:text>
        <xsl:value-of select="associative"/>
        <xsl:text>.Handle;&#10;</xsl:text>

        <xsl:text>  begin&#10;</xsl:text>

        <xsl:text>    Result := </xsl:text>
        <xsl:value-of select="associative"/>
        <xsl:text>.Create&#10;</xsl:text>
        <xsl:text>       ((</xsl:text>

        <xsl:choose>

          <xsl:when test="role[1]/@multiple and role[2]/@multiple">
            <xsl:call-template name="referential-attribute-name">
              <xsl:with-param name="supplier" select="role[1]/classname"/>
              <xsl:with-param name="relation" select="name"/>
            </xsl:call-template>
            <xsl:text> => </xsl:text>
            <xsl:value-of select="role[1]/name"/>
            <xsl:text>,&#10;</xsl:text>
            <xsl:text>         </xsl:text>
            <xsl:call-template name="referential-attribute-name">
              <xsl:with-param name="supplier" select="role[2]/classname"/>
              <xsl:with-param name="relation" select="name"/>
            </xsl:call-template>
            <xsl:text> => </xsl:text>
            <xsl:value-of select="role[2]/name"/>
            <xsl:text>));&#10;</xsl:text>
          </xsl:when>

          <xsl:otherwise>
            <xsl:variable name="multiple-role" select="role[@multiple]"/>
            <xsl:variable name="single-role" select="role[not(@multiple)]"/>
            <xsl:call-template name="referential-attribute-name">
              <xsl:with-param
                name="supplier"
                select="$multiple-role/classname"/>
              <xsl:with-param name="relation" select="name"/>
            </xsl:call-template>
            <xsl:text> => </xsl:text>
            <xsl:value-of select="$multiple-role/name"/>
            <xsl:text>));&#10;</xsl:text>
            <xsl:text>    </xsl:text>
            <xsl:value-of select="associative"/>
            <xsl:text>.Set_</xsl:text>
            <xsl:call-template name="referential-attribute-name">
              <xsl:with-param
                name="supplier"
                select="$single-role/classname"/>
              <xsl:with-param name="relation" select="name"/>
            </xsl:call-template>
            <xsl:text> (Result, </xsl:text>
            <xsl:value-of select="role[1]/name"/>
            <xsl:text>);&#10;</xsl:text>
          </xsl:otherwise>

          <!-- XXX what about 1-1:1? -->

        </xsl:choose>

        <xsl:text>    return Result;&#10;</xsl:text>
        <xsl:text>  end Link;&#10;</xsl:text>

      </xsl:when>

      <xsl:when test="not(associative)">

        <xsl:call-template name="link-procedure-specification"/>
        <xsl:text> is&#10;</xsl:text>
        <xsl:text>  begin&#10;</xsl:text>

        <xsl:text>    </xsl:text>
        <xsl:value-of select="role[not(@source)]/classname"/>
        <xsl:text>.Set_</xsl:text>
        <xsl:call-template name="referential-attribute-name">
          <xsl:with-param
            name="supplier"
            select="role[@source]/classname"/>
          <xsl:with-param name="relation" select="name"/>
        </xsl:call-template>
        <xsl:text> (</xsl:text>
        <xsl:value-of select="role[not(@source)]/name"/>
        <xsl:text>, </xsl:text>
        <xsl:value-of select="role[@source]/name"/>
        <xsl:text>);&#10;</xsl:text>
        <xsl:text>  end Link;&#10;</xsl:text>

      </xsl:when>

    </xsl:choose>
  </xsl:template>


  <!-- Called at domain/association to generate the unlinking procedure
       body -->
  <xsl:template name="unlink-body">

    <xsl:choose>
      <xsl:when test="associative">
        <xsl:call-template name="unlink-associative-specification"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:call-template name="unlink-specification"/>
      </xsl:otherwise>
    </xsl:choose>

    <xsl:text> is&#10;</xsl:text>
    <xsl:text>  begin&#10;</xsl:text>
    <xsl:text>    null;&#10;</xsl:text>
    <xsl:text>  end Unlink;&#10;</xsl:text>

  </xsl:template>


  <!-- Called at domain/association to generate the navigation function
       specs. -->
  <xsl:template name="navigation-specs">

    <xsl:call-template name="navigation-specification">
      <xsl:with-param name="role-a" select="role[1]"/>
      <xsl:with-param name="role-b" select="role[2]"/>
    </xsl:call-template>
    <xsl:text>;&#10;</xsl:text>

    <xsl:call-template name="navigation-specification">
      <xsl:with-param name="role-a" select="role[2]"/>
      <xsl:with-param name="role-b" select="role[1]"/>
    </xsl:call-template>
    <xsl:text>;&#10;</xsl:text>

  </xsl:template>


  <!-- Called at domain/association to generate the navigation function
       bodies. -->
  <xsl:template name="navigation-bodies">

    <xsl:call-template name="navigation-body">
      <xsl:with-param name="role-a" select="role[1]"/>
      <xsl:with-param name="role-b" select="role[2]"/>
    </xsl:call-template>

    <xsl:call-template name="navigation-body">
      <xsl:with-param name="role-a" select="role[2]"/>
      <xsl:with-param name="role-b" select="role[1]"/>
    </xsl:call-template>

  </xsl:template>


  <!-- Called at domain/association to generate a navigation function
       body. -->
  <xsl:template name="navigation-body">
    <xsl:param name="role-a"/>
    <xsl:param name="role-b"/>

    <xsl:call-template name="navigation-specification">
      <xsl:with-param name="role-a" select="$role-a"/>
      <xsl:with-param name="role-b" select="$role-b"/>
    </xsl:call-template>
    <xsl:text> is&#10;</xsl:text>

    <xsl:choose>

      <xsl:when test="associative">
        <xsl:text>    raise Program_Error;&#10;</xsl:text>
      </xsl:when>

      <xsl:when test="not(associative)">
        
        <xsl:if test="$role-b/@multiple or $role-a/@source">
          <xsl:text>    function Sel (This : </xsl:text>
          <xsl:value-of select="/domain/name"/>
          <xsl:text>.</xsl:text>
          <xsl:value-of select="$role-b/classname"/>
          <xsl:text>.Handle) return Boolean is&#10;</xsl:text>
          <xsl:text>     use type </xsl:text>
          <xsl:value-of select="/domain/name"/>
          <xsl:text>.</xsl:text>
          <xsl:value-of select="$role-a/classname"/>
          <xsl:text>.Handle;&#10;</xsl:text>
          <xsl:text>    begin&#10;</xsl:text>
          <xsl:text>      return </xsl:text>
          <xsl:value-of select="/domain/name"/>
          <xsl:text>.</xsl:text>
          <xsl:value-of select="$role-b/classname"/>
          <xsl:text>.Get_</xsl:text>
          <xsl:call-template name="referential-attribute-name">
            <xsl:with-param
              name="supplier"
              select="$role-a/classname"/>
            <xsl:with-param name="relation" select="name"/>
          </xsl:call-template>
          <xsl:text> (This) = </xsl:text>
          <xsl:value-of
            select="/domain/class[name=$role-a/classname]/abbreviation"/>
          <xsl:text>;&#10;</xsl:text>
          <xsl:text>    end Sel;&#10;</xsl:text>
          <xsl:text>    function Find is&#10;</xsl:text>
          <xsl:text>    new </xsl:text>
          <xsl:value-of select="/domain/name"/>
          <xsl:text>.</xsl:text>
          <xsl:value-of select="$role-b/classname"/>
          <xsl:text>.Selection_Function (Sel);&#10;</xsl:text>
          <xsl:text>    Result : constant </xsl:text>
          <xsl:value-of select="/domain/name"/>
          <xsl:text>.</xsl:text>
          <xsl:value-of select="$role-b/classname"/>
          <xsl:text>.Collections.Collection := Find;&#10;</xsl:text>
        </xsl:if>

      </xsl:when>

    </xsl:choose>

    <xsl:text>  begin&#10;</xsl:text>

    <xsl:choose>

      <xsl:when test="associative">
        <xsl:text>    raise Program_Error;&#10;</xsl:text>
      </xsl:when>

      <xsl:when test="not(associative)">

        <xsl:choose>

          <xsl:when test="$role-b/@multiple">
            <xsl:text>    return Result;&#10;</xsl:text>
          </xsl:when>

          <xsl:when test="$role-a/@source">
            <xsl:text>    if </xsl:text>
            <xsl:value-of select="/domain/name"/>
            <xsl:text>.</xsl:text>
            <xsl:value-of select="$role-b/classname"/>
            <xsl:text>.Collections.Is_Empty (Result) then&#10;</xsl:text>
            <xsl:text>      return null;&#10;</xsl:text>
            <xsl:text>    else&#10;</xsl:text>
            <xsl:text>      return </xsl:text>
            <xsl:value-of select="/domain/name"/>
            <xsl:text>.</xsl:text>
            <xsl:value-of select="$role-b/classname"/>
            <xsl:text>.Collections.First (Result);&#10;</xsl:text>
            <xsl:text>    end if;&#10;</xsl:text>
          </xsl:when>

          <xsl:otherwise>
            <xsl:text>    return </xsl:text>
            <xsl:value-of select="/domain/name"/>
            <xsl:text>.</xsl:text>
            <xsl:value-of select="$role-a/classname"/>
            <xsl:text>.Get_</xsl:text>
            <xsl:call-template name="referential-attribute-name">
              <xsl:with-param
                name="supplier"
                select="$role-b/classname"/>
              <xsl:with-param name="relation" select="name"/>
            </xsl:call-template>
            <xsl:text> (</xsl:text>
            <xsl:value-of
              select="/domain/class[name=$role-a/classname]/abbreviation"/>
            <xsl:text>);&#10;</xsl:text>
          </xsl:otherwise>

        </xsl:choose>

      </xsl:when>

    </xsl:choose>

    <xsl:text>  end </xsl:text>
    <xsl:value-of select="$role-a/name"/>
    <xsl:text>;&#10;</xsl:text>

  </xsl:template>


  <!-- Utilities. -->


  <!-- Called at domain/association to generate the specification (no closing
       ";" or "is") for the linking function spec for associative
       associations. -->
  <xsl:template name="link-function-specification">

    <xsl:text>  function Link&#10;</xsl:text>
    <xsl:text>     (</xsl:text>
    <xsl:value-of select="role[1]/name"/>
    <xsl:text> : </xsl:text>
    <xsl:value-of select="/domain/name"/>
    <xsl:text>.</xsl:text>
    <xsl:value-of select="role[1]/classname"/>
    <xsl:text>.Handle</xsl:text>
    <xsl:text>;&#10;</xsl:text>
    <xsl:text>      </xsl:text>
    <xsl:value-of select="role[2]/name"/>
    <xsl:text> : </xsl:text>
    <xsl:value-of select="/domain/name"/>
    <xsl:text>.</xsl:text>
    <xsl:value-of select="role[2]/classname"/>
    <xsl:text>.Handle</xsl:text>
    <xsl:text>) return </xsl:text>
    <xsl:value-of select="/domain/name"/>
    <xsl:text>.</xsl:text>
    <xsl:value-of select="associative"/>
    <xsl:text>.Handle</xsl:text>

  </xsl:template>
 

  <!-- Called at domain/association to generate the specification (no closing
       ";" or "is") for the linking procedure spec for non-associative
       associations. -->
  <xsl:template name="link-procedure-specification">
    <xsl:text>  procedure Link&#10;</xsl:text>
    <xsl:text>     (</xsl:text>
    <xsl:value-of select="role[1]/name"/>
    <xsl:text> : </xsl:text>
    <xsl:value-of select="/domain/name"/>
    <xsl:text>.</xsl:text>
    <xsl:value-of select="role[1]/classname"/>
    <xsl:text>.Handle</xsl:text>
    <xsl:text>;&#10;</xsl:text>
    <xsl:text>      </xsl:text>
    <xsl:value-of select="role[2]/name"/>
    <xsl:text> : </xsl:text>
    <xsl:value-of select="/domain/name"/>
    <xsl:text>.</xsl:text>
    <xsl:value-of select="role[2]/classname"/>
    <xsl:text>.Handle</xsl:text>
    <xsl:text>)</xsl:text>
  </xsl:template>


  <!-- Called at domain/association to generate the unlinking procedure
       specification (no closing ";" or "is") for associative
       associations. -->
  <xsl:template name="unlink-associative-specification">
    <xsl:text>  procedure Unlink&#10;</xsl:text>
    <xsl:text>     (</xsl:text>
    <xsl:value-of select="associative"/>
    <xsl:text>_Handle : </xsl:text>
    <xsl:value-of select="/domain/name"/>
    <xsl:text>.</xsl:text>
    <xsl:value-of select="associative"/>
    <xsl:text>.Handle)</xsl:text>
  </xsl:template>


  <!-- Called at domain/association to generate the unlinking procedure
       specification (no closing ";" or "is") for non-associative
       associations. -->
  <xsl:template name="unlink-specification">
    <xsl:text>  procedure Unlink&#10;</xsl:text>
    <xsl:text>     (</xsl:text>
    <xsl:value-of select="role[1]/name"/>
    <xsl:text> : </xsl:text>
    <xsl:value-of select="/domain/name"/>
    <xsl:text>.</xsl:text>
    <xsl:value-of select="role[1]/classname"/>
    <xsl:text>.Handle</xsl:text>
    <xsl:text>;&#10;</xsl:text>
    <xsl:text>      </xsl:text>
    <xsl:value-of select="role[2]/name"/>
    <xsl:text> : </xsl:text>
    <xsl:value-of select="/domain/name"/>
    <xsl:text>.</xsl:text>
    <xsl:value-of select="role[2]/classname"/>
    <xsl:text>.Handle</xsl:text>
    <xsl:text>)</xsl:text>
  </xsl:template>


  <!-- Called at domain/association to generate a navigation function
       spec (no closing ";" or "is"). -->
  <xsl:template name="navigation-specification">
    <xsl:param name="role-a"/>
    <xsl:param name="role-b"/>
    <xsl:text>  function </xsl:text>
    <xsl:value-of select="$role-a/name"/>
    <xsl:text>&#10;</xsl:text>
    <xsl:text>     (</xsl:text>
    <xsl:value-of select="/domain/class[name=$role-a/classname]/abbreviation"/>
    <xsl:text> : </xsl:text>
    <xsl:value-of select="/domain/name"/>
    <xsl:text>.</xsl:text>
    <xsl:value-of select="$role-a/classname"/>
    <xsl:text>.Handle) return </xsl:text>
    <xsl:value-of select="/domain/name"/>
    <xsl:text>.</xsl:text>
    <xsl:value-of select="$role-b/classname"/>
    <xsl:choose>
      <xsl:when test="$role-b/@multiple">
        <xsl:text>.Collections.Collection</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>.Handle</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>


</xsl:stylesheet>
