<!-- $Id: ada-type.xsl,v 74ad9a2bdaed 2001/09/27 18:28:48 simon $ -->
<!-- XSL stylesheet to generate Ada code for types. -->
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

<xsl:stylesheet
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  version="1.0">

  <!-- Generate domain context clauses. -->
  <xsl:template mode="domain-context" match="domain/type">

    <!-- Context for non-record domain types. -->

    <xsl:if test="string/max">
      <!-- string/max implies an instantiation of Ada.Strings.Bounded -->
      <xsl:text>with Ada.Strings.Bounded;</xsl:text>
      <xsl:text> use Ada.Strings.Bounded;&#10;</xsl:text>
    </xsl:if>

    <!-- Context for imported types. -->
    <xsl:if test="imported">
      <xsl:text>with </xsl:text>
      <xsl:value-of select="imported"/>
      <xsl:text>;&#10;</xsl:text>
    </xsl:if>

    <!-- Context for record domain types; cf class-spec-context. -->

    <xsl:if test="attribute/type='Date'
                  or operation/parameter/type='Date'
                  or attribute/type='Time'
                  or operation/parameter/type='Time'">
      <!-- The above imply use of Ada.Calendar. -->
      <xsl:text>with Ada.Calendar;</xsl:text>
      <xsl:text> use Ada.Calendar;&#10;</xsl:text>
    </xsl:if>

    <xsl:if test="attribute/type='Unbounded_String'
                  or operation/parameter/type='Unbounded_String'
                  or attribute/type='Text'
                  or operation/parameter/type='Text'">
      <!-- All the above imply use of Unbounded_Strings. -->
      <xsl:text>with Ada.Strings.Unbounded;</xsl:text>
      <xsl:text> use Ada.Strings.Unbounded;&#10;</xsl:text>
    </xsl:if>

  </xsl:template>

  <xsl:template mode="domain-context" match="*"/>


  <!-- Called at domain to generate domain Types entries (not for
       standard types). -->
  <xsl:template name="domain-types">
    <xsl:call-template name="sorted-domain-types">
      <xsl:with-param name="nodes" select="/.."/>
      <xsl:with-param name="finished" select="type[standard]"/>
    </xsl:call-template>
  </xsl:template>


  <!-- Called at domain to output the type declarations sorted
       in dependency order. -->
  <xsl:template name="sorted-domain-types">

    <!-- The types which are to be output in this pass -->
    <xsl:param name="nodes"/>

    <!-- The types which have already been output -->
    <xsl:param name="finished"/>

    <!-- Saxon-5.5 needs this, or it goes into an infinite loop, -->
    <!-- <xsl:message>
      <xsl:text>domain-types: nodes </xsl:text>
      <xsl:value-of select="count($nodes)"/>
      <xsl:text>, finished </xsl:text>
      <xsl:value-of select="count($finished)"/>
      <xsl:text>, total </xsl:text>
      <xsl:value-of select="count(type)"/>
    </xsl:message> -->

    <!-- Output the types for this pass -->
    <xsl:for-each select="$nodes">
      <xsl:sort select="name"/>
      <xsl:call-template name="domain-type"/>
    </xsl:for-each>

    <!-- The set of types output so far -->
    <xsl:variable name="processed" select="$finished | $nodes"/>

    <!-- The set of types not yet output and dependent only on types
         that have been output already -->
    <xsl:variable
      name="next"
      select="type[not($processed/name=name)
              and not(attribute/type[not($processed/name=.)])]"/>

    <xsl:choose>

      <xsl:when test="$next">

        <!-- More types to be output, so make the recursive call -->
        <xsl:call-template name="sorted-domain-types">
          <xsl:with-param name="nodes" select="$next"/>
          <xsl:with-param name="finished" select="$processed"/>
        </xsl:call-template>

      </xsl:when>

      <xsl:otherwise>

        <!-- No more types able to be output, check we haven't missed any.
             These should only be records with attributes of undeclared
             type. -->

        <xsl:variable name="missing" select="type[not($processed/name=name)]"/>

        <xsl:if test="$missing">

          <xsl:message terminate="yes">
            <xsl:for-each select="$missing">
              <xsl:sort select="name"/>
              <xsl:text>Couldn't generate type </xsl:text>
              <xsl:value-of select="name"/>
              <xsl:text>&#10;</xsl:text>
              <xsl:for-each select="attribute/type[not($processed/name=.)]">
                <xsl:sort select="."/>
                <xsl:text>  undeclared type </xsl:text>
                <xsl:value-of select="."/>
                <xsl:text>&#10;</xsl:text>
              </xsl:for-each>
            </xsl:for-each>
          </xsl:message>

        </xsl:if>

      </xsl:otherwise>

    </xsl:choose>

  </xsl:template>


  <!-- Called at domain/type to generate a single domain Type entry
       (not for standard types). -->
  <xsl:template name="domain-type">
    <xsl:choose>

      <xsl:when test="attribute">

        <xsl:choose>

          <xsl:when test="@discriminated">

            <!--
                 type {name}_Discriminant is
                   ({attr-name}_T,
                    {attr-name}_T);
                 type {name}
                   (Discriminant : {name}_Discriminant
                    := {name}_Discriminant'First)
                 is record
                   case Discriminant is
                      when {attr-name}_T =>
                         {attr-name} : {attr-type}[ := {attr-init}];
                      when {attr-name}_T =>
                         {attr-name} : {attr-type}[ := {attr-init}];
                   end case;
                 end record;
                 -->

            <!-- The discriminant enumeration type -->
            <xsl:value-of select="$I"/>
            <xsl:text>type </xsl:text>
            <xsl:value-of select="name"/>
            <xsl:text>_Discriminant is</xsl:text>
            <xsl:text>&#10;</xsl:text>
            <xsl:value-of select="$IC"/>
            <xsl:text>(</xsl:text>
            <xsl:for-each select="attribute/name">
              <xsl:sort select="."/>
              <xsl:value-of select="."/>
              <xsl:text>_T</xsl:text>
              <xsl:if test="position() &lt; last()">
                <xsl:text>,&#10;</xsl:text>
                <xsl:value-of select="$IC"/>
                <xsl:text> </xsl:text>
              </xsl:if>
            </xsl:for-each>
            <xsl:text>);&#10;</xsl:text>

            <!-- The discriminated record type -->
            <xsl:value-of select="$I"/>
            <xsl:text>type </xsl:text>
            <xsl:value-of select="name"/>
            <xsl:text>&#10;</xsl:text>
            <xsl:value-of select="$IC"/>
            <xsl:text>(Discriminant : </xsl:text>
            <xsl:value-of select="name"/>
            <xsl:text>_Discriminant</xsl:text>
            <xsl:text>&#10;</xsl:text>
            <xsl:value-of select="$IC"/>
            <xsl:text> := </xsl:text>
            <xsl:value-of select="name"/>
            <xsl:text>_Discriminant'First)&#10;</xsl:text>
            <xsl:value-of select="$I"/>
            <xsl:text>is record&#10;</xsl:text>
            <xsl:value-of select="$II"/>
            <xsl:text>case Discriminant is&#10;</xsl:text>

            <xsl:for-each select="attribute">
              <xsl:sort select="name"/>

              <xsl:value-of select="$III"/>
              <xsl:text>when </xsl:text>
              <xsl:value-of select="name"/>
              <xsl:text>_T =&gt;&#10;</xsl:text>

              <xsl:call-template name="single-record-component">
                <xsl:with-param name="indent" select="$IIII"/>
              </xsl:call-template>

            </xsl:for-each>

            <xsl:value-of select="$II"/>
            <xsl:text>end case;&#10;</xsl:text>
            <xsl:value-of select="$I"/>
            <xsl:text>end record;&#10;</xsl:text>

          </xsl:when>

          <xsl:otherwise>

            <!--
                 type {name} is record
                   {attr-name} : {attr-type}[ := {attr-init}];
                 end record;
                 -->

            <xsl:value-of select="$I"/>
            <xsl:text>type </xsl:text>
            <xsl:value-of select="name"/>
            <xsl:text> is record&#10;</xsl:text>
            <xsl:apply-templates mode="instance-record-component"/>
            <xsl:value-of select="$I"/>
            <xsl:text>end record;&#10;</xsl:text>
            
          </xsl:otherwise>

        </xsl:choose>

      </xsl:when>
      
      <xsl:when test="enumeration">
        <xsl:value-of select="$I"/>
        <xsl:text>type </xsl:text>
        <xsl:value-of select="name"/>
        <xsl:text> is</xsl:text>
        <xsl:text>&#10;</xsl:text>
        <xsl:value-of select="$IC"/>
        <xsl:text>(</xsl:text>
        <xsl:for-each select="enumeration/literal">
          <xsl:value-of select="."/>
          <xsl:if test="position() &lt; last()">
            <xsl:text>,&#10;</xsl:text>
            <xsl:value-of select="$IC"/>
            <xsl:text> </xsl:text>
          </xsl:if>
        </xsl:for-each>
        <xsl:text>);&#10;</xsl:text>
      </xsl:when>
      
      <xsl:when test="imported">
        <xsl:value-of select="$I"/>
        <xsl:text>subtype </xsl:text>
        <xsl:value-of select="name"/>
        <xsl:text> is </xsl:text>
        <xsl:value-of select="imported"/>
        <xsl:text>.</xsl:text>
        <xsl:value-of select="name"/>
        <xsl:text>;&#10;</xsl:text>
      </xsl:when>
      
      <xsl:when test="integer">
        <xsl:value-of select="$I"/>
        <xsl:text>type </xsl:text>
        <xsl:value-of select="name"/>
        <xsl:text> is range </xsl:text>
        <xsl:value-of select="integer/lower"/>
        <xsl:text> .. </xsl:text>
        <xsl:value-of select="integer/upper"/>
        <xsl:text>;&#10;</xsl:text>
      </xsl:when>
      
      <xsl:when test="real">
        <xsl:value-of select="$I"/>
        <xsl:text>type </xsl:text>
        <xsl:value-of select="name"/>
        <xsl:text> is digits </xsl:text>
        <xsl:value-of select="real/digits"/>
        <xsl:if test="real/lower and real/upper">
          <xsl:text> range </xsl:text>
          <xsl:value-of select="real/lower"/>
          <xsl:text> .. </xsl:text>
          <xsl:value-of select="real/upper"/>
        </xsl:if>
        <xsl:text>;&#10;</xsl:text>
      </xsl:when>
      
      <!-- sets are implemented as class Collections; no action here -->
      <xsl:when test="set"/>
        
      <xsl:when test="string">
        <xsl:value-of select="$I"/>
        <xsl:text>package </xsl:text>
        <xsl:value-of select="name"/>
        <xsl:text>_Package is&#10;</xsl:text>
        <xsl:value-of select="$IC"/>
        <xsl:text>new Generic_Bounded_Length (Max =&gt; </xsl:text>
        <xsl:value-of select="string/max"/>
        <xsl:text>);&#10;</xsl:text>
        <xsl:value-of select="$I"/>
        <xsl:text>subtype </xsl:text>
        <xsl:value-of select="name"/>
        <xsl:text> is </xsl:text>
        <xsl:value-of select="name"/>
        <xsl:text>_Package.Bounded_String;&#10;</xsl:text>
      </xsl:when>
      
      <xsl:otherwise>
        <xsl:text>  -- Unrecognised type category for </xsl:text>
        <xsl:value-of select="name"/>
        <xsl:text>&#10;</xsl:text>
      </xsl:otherwise>
      
    </xsl:choose>
  </xsl:template>


  <!-- Generate domain Types support entries (not for standard types).
       We do these as child packages in case they're not actually 
       needed. -->
  <xsl:template mode="domain-type-support" match="domain/type">
    <xsl:if test="not(standard)">
      <xsl:choose>

        <xsl:when test="attribute"/>
        
        <xsl:when test="enumeration"/>
        
        <xsl:when test="imported"/>

        <xsl:when test="integer"/>

        <xsl:when test="real"/>

        <xsl:when test="set"/>

        <xsl:when test="string">
          <xsl:text>with ColdFrame.String_Hash;&#10;</xsl:text>
          <xsl:text>function </xsl:text>
          <xsl:value-of select="../name"/>
          <xsl:text>.</xsl:text>
          <xsl:value-of select="name"/>
          <xsl:text>_Hash is&#10;</xsl:text>
          <xsl:value-of select="$C"/>
          <xsl:text>new ColdFrame.String_Hash.Bounded_Hash (</xsl:text>
          <xsl:value-of select="name"/>
          <xsl:text>_Package);&#10;</xsl:text>
        </xsl:when>

        <xsl:otherwise>
          <xsl:text>-- Unrecognised type category for </xsl:text>
          <xsl:value-of select="name"/>
          <xsl:text>&#10;</xsl:text>
        </xsl:otherwise>

      </xsl:choose>
    </xsl:if>
  </xsl:template>

  <xsl:template mode="domain-type-support" match="*"/>


</xsl:stylesheet>
