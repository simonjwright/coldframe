<!-- $Id: ada-type.xsl,v 68536b08e9a8 2002/02/28 19:57:09 simon $ -->
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
  version="1.1">

  <!-- Called at /domain to generate domain context clauses. -->
  <xsl:template name="domain-context">

    <!-- Context for time (in record components). -->

    <xsl:if test="type/attribute/type='Date'
                  or type/operation/parameter/type='Date'
                  or type/attribute/type='Time'
                  or type/operation/parameter/type='Time'">
      <!-- The above imply use of Ada.Calendar. -->
      <xsl:text>with Ada.Calendar; use Ada.Calendar;&#10;</xsl:text>
    </xsl:if>

    <!-- Context for bounded strings -->
    <xsl:if test="type/string/max">
      <!-- string/max implies an instantiation of Ada.Strings.Bounded -->
      <xsl:text>with Ada.Strings.Bounded; use Ada.Strings.Bounded;&#10;</xsl:text>
    </xsl:if>

    <!-- Context for unbounded strings (in record components). -->

    <xsl:if test="type/attribute/type='Unbounded_String'
                  or type/operation/parameter/type='Unbounded_String'
                  or type/attribute/type='Text'
                  or type/operation/parameter/type='Text'">
      <!-- All the above imply use of Unbounded_Strings. -->
      <xsl:text>with Ada.Strings.Unbounded;</xsl:text>
      <xsl:text> use Ada.Strings.Unbounded;&#10;</xsl:text>
    </xsl:if>

    <!-- Context for imported and renamed types, ensuring uniqueness. -->

    <!-- First, make a nodeset containing "with" elements containing
         the package names. -->
    <xsl:variable name="imported-renamed-withs">
      <xsl:for-each select="type/imported">
        <xsl:element name="with">
          <xsl:value-of select="."/>
        </xsl:element>
      </xsl:for-each>
      <xsl:for-each select="type/renames">
        <xsl:variable name="package">
          <xsl:call-template name="find-source-package">
            <xsl:with-param name="input" select="."/>
          </xsl:call-template>
        </xsl:variable>
        <xsl:if test="string-length($package)">
          <xsl:element name="with">
            <xsl:value-of select="$package"/>
          </xsl:element>
        </xsl:if>      
      </xsl:for-each>
    </xsl:variable>

    <!-- Then, sort, and output unique occurrences. -->
    <!-- XXX Saxon 6.5.1 allows this result tree fragment to be
         implicitly converted to a node set if the version is 1.1,
         so I've changed this file to require 1.1.
         Should consider saxon:node-set() or exsl:node-set(). -->
    <xsl:for-each select="$imported-renamed-withs/with">
      <xsl:sort select="."/>
      <xsl:if test="not (.=preceding-sibling::node())">
        <xsl:text>with </xsl:text>
        <xsl:value-of select="."/>
        <xsl:text>;&#10;</xsl:text>
      </xsl:if>
    </xsl:for-each>

  </xsl:template>


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

    <!-- Saxon-5.5 needs this, or it goes into an infinite loop. -->
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
      <xsl:value-of select="$blank-line"/>
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
        <xsl:text>&#10;</xsl:text>
        <xsl:value-of select="$I"/>
        <xsl:text>is </xsl:text>
        <xsl:value-of select="imported"/>
        <xsl:text>.</xsl:text>
        <xsl:value-of select="name"/>
        <xsl:text>;&#10;</xsl:text>
        <xsl:value-of select="$I"/>
        <xsl:text>use type </xsl:text>
        <xsl:value-of select="name"/>
        <xsl:text>;&#10;</xsl:text>
      </xsl:when>
      
      <xsl:when test="renames">
        <xsl:value-of select="$I"/>
        <xsl:text>subtype </xsl:text>
        <xsl:value-of select="name"/>
        <xsl:text>&#10;</xsl:text>
        <xsl:value-of select="$I"/>
        <xsl:text>is </xsl:text>
        <xsl:value-of select="renames"/>
        <xsl:text>;&#10;</xsl:text>
        <xsl:value-of select="$I"/>
        <xsl:text>use type </xsl:text>
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

        <xsl:when test="renames"/>

        <xsl:when test="integer"/>

        <xsl:when test="real"/>

        <xsl:when test="string">
          <xsl:text>with ColdFrame.Hash.Strings.Bounded;&#10;</xsl:text>
          <xsl:text>function </xsl:text>
          <xsl:value-of select="../name"/>
          <xsl:text>.</xsl:text>
          <xsl:value-of select="name"/>
          <xsl:text>_Hash is&#10;</xsl:text>
          <xsl:value-of select="$C"/>
          <xsl:text>new ColdFrame.Hash.Strings.Bounded (</xsl:text>
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


  <!-- Called to extract the package name from a (possibly) qualified
       type name. -->
  <xsl:template name="find-source-package">
    <!-- The input name -->
    <xsl:param name="input"/>
    <!-- The package name so far -->
    <xsl:param name="package" select="''"/>

    <xsl:variable name="before" select="substring-before($input, '.')"/>
    <xsl:variable name="after" select="substring-after($input, '.')"/>

    <xsl:choose>

      <!-- Terminate recursion when there are no more components in the
           input to process -->
      <xsl:when test="string-length($before)=0">
        <xsl:value-of select="$package"/>
      </xsl:when>

      <xsl:otherwise>
        
        <xsl:variable name="new-package">
          <xsl:choose>
            <xsl:when test="string-length($package)=0">
              <xsl:value-of select="$before"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:value-of select="concat($package, '.', $before)"/>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:variable>

        <xsl:call-template name="find-source-package">
          <xsl:with-param name="input" select="$after"/>
          <xsl:with-param name="package" select="$new-package"/>
        </xsl:call-template>

      </xsl:otherwise>

    </xsl:choose>

  </xsl:template>

</xsl:stylesheet>
