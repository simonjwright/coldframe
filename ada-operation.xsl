<!-- $Id: ada-operation.xsl,v 0507e6778854 2001/04/13 12:42:15 simon $ -->
<!-- XSL stylesheet to generate Ada code for Operations. -->
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

  <!-- Generate child subprogram specs. -->
  <xsl:template match="class/operation" mode="operation-spec">
    <xsl:apply-templates select="." mode="operation-context"/>
    <xsl:call-template name="subprogram-specification"/>
    <xsl:text>;&#10;</xsl:text>
  </xsl:template>

  <xsl:template mode="operation-spec" match="*"/>


  <!-- Generate child subprogram context. -->
  <!-- XXX at present, doesn't ensure there's only one of each -->
  <xsl:template match="class/operation" mode="operation-context">

    <!-- Find the names of all the types involved -->
    <xsl:for-each select="parameter/type | @return">

      <!-- .. sorted, so we can uniqueify them (when I've worked
           out how) .. -->
      <xsl:sort select="."/>

      <!-- .. only using those whose names are those of classes in
           the domain .. -->
      <xsl:if test="/domain/class/name=.">
        <xsl:text>with </xsl:text>
        <xsl:value-of select="/domain/name"/>
        <xsl:text>.</xsl:text>
        <xsl:value-of select="."/>
        <xsl:text>;&#10;</xsl:text>
      </xsl:if>

      <!-- .. or sets of classes in the domain .. -->
      <xsl:variable name="type" select="."/>
      <xsl:variable name="type-name" select="/domain/type[name=$type]"/>
      <xsl:if test="$type-name/set">
        <xsl:text>with </xsl:text>
        <xsl:value-of select="/domain/name"/>
        <xsl:text>.</xsl:text>
        <xsl:value-of select="$type-name/set"/>
        <xsl:text>.Collections;&#10;</xsl:text>
      </xsl:if>

    </xsl:for-each>
  </xsl:template>

  <xsl:template mode="operation-context" match="*"/>


  <!-- Generate the bodies of child operations. The bodies are
       compilable but generate Program_Error if called. -->
  <xsl:template match="class/operation" mode="operation-body">

    <xsl:call-template name="subprogram-specification"/>
    <xsl:text> is&#10;</xsl:text>
    <xsl:text>begin&#10;</xsl:text>

    <xsl:choose>

      <!-- If it's a function, we have to supply a return statement
           after raising the exception for it to compile.-->
      <xsl:when test="@return">
        <xsl:text>  raise Program_Error;&#10;</xsl:text>
        <xsl:text>  return </xsl:text>
        <xsl:call-template name="default-value">
          <xsl:with-param name="type" select="@return"/>
        </xsl:call-template>
        <xsl:text>;&#10;</xsl:text>
      </xsl:when>

      <xsl:otherwise>
        <xsl:text>  raise Program_Error;&#10;</xsl:text>
      </xsl:otherwise>

    </xsl:choose>

    <xsl:text>end </xsl:text>
    <xsl:value-of select="../../name"/>
    <xsl:text>.</xsl:text>
    <xsl:value-of select="../name"/>
    <xsl:text>.</xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>;&#10;</xsl:text>

  </xsl:template>

  <xsl:template mode="operation-body" match="*"/>


  <!-- Called from class/operation to generate a subprogram specification.
       Ends without the closing ";" or " is". -->
  <xsl:template name="subprogram-specification">
    <xsl:choose>

      <!-- If there's a return attribute, it's a function. -->
      <xsl:when test="@return">
        <xsl:text>function </xsl:text>
        <xsl:value-of select="../../name"/>
        <xsl:text>.</xsl:text>
        <xsl:value-of select="../name"/>
        <xsl:text>.</xsl:text>
        <xsl:value-of select="name"/>
        <xsl:call-template name="parameter-list"/>
        <xsl:text>&#10;   return </xsl:text>
        <xsl:call-template name="type-name">
          <xsl:with-param name="type" select="@return"/>
        </xsl:call-template>
      </xsl:when>

      <!-- If there's no return attribute, it's a procedure. -->
      <xsl:otherwise>
        <xsl:text>procedure </xsl:text>
        <xsl:value-of select="../../name"/>
        <xsl:text>.</xsl:text>
        <xsl:value-of select="../name"/>
        <xsl:text>.</xsl:text>
        <xsl:value-of select="name"/>
        <xsl:call-template name="parameter-list"/>
      </xsl:otherwise>

    </xsl:choose>
  </xsl:template>


  <!-- Called from class/operation to generate a subprogram parameter list -->
  <xsl:template name="parameter-list">

    <!-- In Ada, an empty parameter list is void (not "()" as in C).
         If the operation has parameters, we clearly need a parameter
         list here! Otherwise, we have to check for a Handle; if
         the Class is a singleton, all operations are class operations,
         otherwise it depends on the class attribute. -->
    <xsl:if
      test="parameter or (not(../@singleton) and not(@class='yes'))">

      <xsl:text>&#10;</xsl:text>
      <xsl:text>  (</xsl:text>
      <xsl:if test="not(../@singleton) and not(@class='yes')">
        <xsl:text>This : Handle</xsl:text>
        <xsl:if test="parameter">
          <xsl:text>;&#10;   </xsl:text>
        </xsl:if>
      </xsl:if>
      <xsl:apply-templates mode="parameter"/>
      <xsl:text>)</xsl:text>

    </xsl:if>

  </xsl:template>


  <!-- Called from class/operation to generate a subprogram parameter -->
  <xsl:template match="operation/parameter" mode="parameter">
    <xsl:value-of select="name"/>
    <xsl:text> : </xsl:text>
    <xsl:choose>
      <xsl:when test="@mode='inout'">
        <xsl:text>in out </xsl:text>
      </xsl:when>
      <xsl:when test="@mode='out'">
        <xsl:text>out </xsl:text>
      </xsl:when>
    </xsl:choose>
    <xsl:call-template name="type-name">
      <xsl:with-param name="type" select="type"/>
    </xsl:call-template>
    <xsl:if test="initial">
      <xsl:text> := </xsl:text>
      <xsl:value-of select="initial"/>
    </xsl:if>
    <xsl:if test="position() &lt; last()">
      <xsl:text>;&#10;</xsl:text>
      <xsl:text>   </xsl:text>
    </xsl:if>
  </xsl:template>

  <xsl:template mode="parameter" match="*"/>


  <!-- Called from class/operation to generate a default value.
       Used for default return value for function bodies,
       defaults in initializers. -->
  <xsl:template name="default-value">
    <xsl:param name="type"/>
    <xsl:choose>

      <!-- Ordinary string -->
      <xsl:when test="$type='String'">
        <xsl:text>""</xsl:text>
      </xsl:when>

      <!-- Unbounded string -->
      <xsl:when test="$type='Unbounded_String' or $type='Text'">
        <xsl:text>Null_Unbounded_String</xsl:text>
      </xsl:when>

      <xsl:otherwise>
        <xsl:variable name="the-type" select="../../type[name=$type]"/>
        <xsl:choose>

          <!-- Bounded string -->
          <xsl:when test="$the-type/string/max">
            <xsl:value-of select="$type"/>
            <xsl:text>_Package.Null_Bounded_String</xsl:text>
          </xsl:when>

          <!-- Class -->
          <xsl:when test="../../class/name=$type">
            <xsl:text>null</xsl:text>
          </xsl:when>

          <!-- Set of classes -->
          <xsl:when test="$the-type/set">
            <xsl:value-of select="/domain/name"/>
            <xsl:text>.</xsl:text>
            <xsl:value-of select="$the-type/set"/>
            <xsl:text>.Collections.Null_Container</xsl:text>
          </xsl:when>

          <!-- Default: assume scalar -->
          <xsl:otherwise>
            <xsl:call-template name="type-name">
              <xsl:with-param name="type" select="$type"/>
            </xsl:call-template>
            <xsl:text>'First</xsl:text>
          </xsl:otherwise>

        </xsl:choose>
      </xsl:otherwise>

    </xsl:choose>
  </xsl:template>


</xsl:stylesheet>
