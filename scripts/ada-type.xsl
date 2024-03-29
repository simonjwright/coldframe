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

     As a special exception, when portions of this file are copied by
     a stylesheet processor into an output file, this file does not by
     itself cause the resulting file to be covered by the GNU General
     Public License.  This exception does not however invalidate any
     other reasons why the output file might be covered by the GNU
     Public License.
     -->

<xsl:stylesheet
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:at="http://pushface.org/coldframe/attribute"
  xmlns:op="http://pushface.org/coldframe/operation"
  xmlns:ty="http://pushface.org/coldframe/type"
  xmlns:ut="http://pushface.org/coldframe/utilities"
  version="1.0">

  <!-- Called at /domain to generate domain context clauses. -->
  <xsl:template name="ty:domain-context">

    <!-- Context for special references (eg pi). -->
    <xsl:if test="references='pi'">
      <xsl:text>with Ada.Numerics;&#10;</xsl:text>
    </xsl:if>

    <!-- Context for
         (a) {counterpart} and Counterpart
         (b) use of domain classes as attribute/parameter/result type. -->
    <xsl:variable name="counterpart">
      <!-- Need an element to make a nodeset next. -->
      <xsl:element name="name">Counterpart</xsl:element>
    </xsl:variable>
    <xsl:variable
      name="classes-or-counterpart"
      select="$counterpart/name
              | /domain/class/name"/>
    <xsl:if test="type/counterpart
                  or type/attribute/type=$classes-or-counterpart
                  or type/operation/parameter/type=$classes-or-counterpart
                  or type/operation/@return=$classes-or-counterpart">
      <xsl:text>with ColdFrame.Instances;&#10;</xsl:text>
    </xsl:if>

    <!-- Context for time. -->

    <xsl:if test="type/attribute/type='Date'
                  or type/operation/parameter/type='Date'
                  or type/operation/@return='Date'
                  or type/attribute/type='Time'
                  or type/operation/parameter/type='Time'
                  or type/operation/@return='Time'">
      <!-- The above imply use of ColdFrame.Project.Calendar.Time -->
      <xsl:text>with ColdFrame.Project.Calendar;&#10;</xsl:text>
    </xsl:if>

    <!-- Context for bounded strings. -->

    <xsl:if test="type/string/max">
      <!-- string/max implies an instantiation of Ada.Strings.Bounded -->
      <xsl:text>with Ada.Strings.Bounded; use Ada.Strings.Bounded;&#10;</xsl:text>
    </xsl:if>

    <!-- Context for unbounded strings. -->

    <xsl:if test="type/attribute/type='Unbounded_String'
                  or type/operation/parameter/type='Unbounded_String'
                  or type/operation/@return='Unbounded_String'
                  or type/array/type='Unbounded_String'
                  or type/array/type='Text'
                  or type/operation/parameter/type='Text'
                  or type/operation/@return='Text'
                  or type/attribute/type='Text'" >
      <!-- All the above imply use of Unbounded_Strings. -->
      <xsl:text>with Ada.Strings.Unbounded;</xsl:text>
      <xsl:text> use Ada.Strings.Unbounded;&#10;</xsl:text>
    </xsl:if>

    <!-- Context for imported, renamed and extended types and imported or
         renamed exceptions, ensuring uniqueness. -->

    <!-- First, make a nodeset containing "with" elements containing
         the package names. -->
    <xsl:variable name="imported-renamed-withs">
      <xsl:for-each select="type/imported | exception/@imported">
        <xsl:element name="with">
          <xsl:value-of select="."/>
        </xsl:element>
      </xsl:for-each>
      <xsl:for-each select="type/renames | type/@extends | exception/@renames">
        <xsl:variable name="package">
          <xsl:call-template name="ty:find-source-package">
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


  <!-- Called at domain to output the type declarations sorted
       in dependency order. -->
  <xsl:template name="ty:sorted-domain-types">

    <!-- The types to be output -->
    <xsl:param name="types"/>

    <!-- The types which are to be output in this pass -->
    <xsl:param name="nodes" select="/.."/>

    <!-- The types which have already been output -->
    <xsl:param name="finished"/>

    <!-- Saxon-5.5 needs this, or it goes into an infinite loop. -->
    <!--
    <xsl:message>
      <xsl:text>domain-types: to do now </xsl:text>
      <xsl:value-of select="count($nodes)"/>
      <xsl:text>, finished already </xsl:text>
      <xsl:value-of select="count($finished)"/>
      <xsl:text>, total in domain </xsl:text>
      <xsl:value-of select="count(type)"/>
    </xsl:message>
    -->

    <!-- Output the types for this pass. -->
    <!-- We don't actually output @access-to-operation types, they
         merely act as carriers for operations marked @access. This is
         an ArgoUML/UML 1.4 issue; if you give an attribute or
         operation of type Foo, there'd better be some Class or
         DataType in the model called Foo or it'll invent one.  -->
    <xsl:for-each select="$nodes[not(@access-to-operation)]">
      <xsl:sort select="name"/>
      <xsl:call-template name="ty:domain-type"/>
      <xsl:call-template name="ut:commentary">
        <xsl:with-param name="indent" select="$I"/>
      </xsl:call-template>
      <xsl:value-of select="$blank-line"/>
    </xsl:for-each>

    <!-- Any implied access types -->
    <xsl:variable name="access-types">
      <xsl:for-each select="$nodes/@access">
        <xsl:element name="name">
          <xsl:value-of select="."/>
        </xsl:element>
      </xsl:for-each>
    </xsl:variable>

    <!-- The set of types output so far -->
    <xsl:variable
      name="processed"
      select="$finished | $nodes | $access-types"/>

    <!-- The set of types not yet output and dependent only on types
         that have been output already -->
    <xsl:variable
      name="next"
      select="$types[not($processed/name=name)
                     and not(
                      attribute[not($processed/name=type)]
                      or array[not($processed/name=type)]
                      or array[not($processed/name=index)]
                      or subtype[not($processed/name=@constrains)]
                      or (@protected and (
                           operation
                            [not(@access)]/parameter[not($processed/name=type)]
                           or operation
                            [not(@access)]/@return[not($processed/name=.)]))
                     )]"/>

    <xsl:choose>

      <xsl:when test="$next">

        <!-- More types to be output, so make the recursive call -->
        <xsl:call-template name="ty:sorted-domain-types">
          <xsl:with-param name="types" select="$types"/>
          <xsl:with-param name="nodes" select="$next"/>
          <xsl:with-param name="finished" select="$processed"/>
        </xsl:call-template>

      </xsl:when>

      <xsl:otherwise>

        <!-- No more types able to be output, check we haven't missed any.
             These should only be records with attributes of undeclared
             type. -->

        <xsl:variable
          name="missed"
          select="$types[not($processed/name=name)]"/>

        <xsl:if test="$missed">

          <!-- Construct a set containing (possibly multiple copies
               of) "name" elements for each missed type that isn't in
               the types defined in this domain. -->

          <xsl:variable name="missing-types">
            <xsl:for-each select="$missed">
              <xsl:for-each
                select=
                "attribute[not($processed/name=type)]/type
                | array[not($processed/name=type)]/type
                | array[not($processed/name=index)]/index
                | subtype[not($processed/name=@constrains)]/@constrains
                | operation[not(@access) and ../@protected]
                /parameter[not($processed/name=type)]/type
                | operation[not(@access) and ../@protected
                and @return and not($processed/name=@return)]/@return">
                <xsl:if test="not($types/name=.)">
                  <xsl:element name="name">
                    <xsl:value-of select="."/>
                  </xsl:element>
                </xsl:if>
              </xsl:for-each>
            </xsl:for-each>
          </xsl:variable>

          <xsl:for-each select="$missing-types">
            <xsl:sort select="name"/>
            <xsl:if test="not(name=preceding-sibling::name)">
              <xsl:variable name="n" select="name"/>
              <xsl:message>
                <xsl:choose>
                  <xsl:when test="$checking-policy='strict'">
                    <xsl:call-template name="ut:log-error"/>
                    <xsl:text>Error: unknown type </xsl:text>
                  </xsl:when>
                  <xsl:otherwise>
                    <xsl:text>Warning: unknown type </xsl:text>
                  </xsl:otherwise>
                </xsl:choose>
                <xsl:value-of select="$n"/>
              </xsl:message>
              <xsl:for-each
                select="$types
                        [attribute/type=$n
                        or array/type=$n
                        or array/index=$n
                        or subtype/@constrains=$n
                        or operation[not(@access) and ../@protected]
                        /parameter/type=$n
                        or operation[not(@access) and ../@protected
                        and @return]/@return=$n]">
                <xsl:sort select="name"/>
                <xsl:message>
                  <xsl:text>  used in </xsl:text>
                  <xsl:value-of select="name"/>
                </xsl:message>
              </xsl:for-each>
            </xsl:if>
          </xsl:for-each>

          <!-- Fake having output the missing types, then make a
               recursive call, starting with no nodes to be
               output. -->
          <xsl:call-template name="ty:sorted-domain-types">
            <xsl:with-param name="types" select="$types"/>
            <xsl:with-param name="next" select="/.."/>
            <xsl:with-param
              name="finished"
              select="$processed | $missing-types"/>
          </xsl:call-template>

        </xsl:if>

      </xsl:otherwise>

    </xsl:choose>

  </xsl:template>


  <!-- Called at domain/type to generate a single domain Type entry
       (not for standard types). -->
  <xsl:template name="ty:domain-type">
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
            <xsl:value-of select="$blank-line"/>

            <xsl:for-each select="attribute">
              <xsl:sort select="name"/>

              <xsl:value-of select="$III"/>
              <xsl:text>when </xsl:text>
              <xsl:value-of select="name"/>
              <xsl:text>_T =&gt;&#10;</xsl:text>

              <xsl:call-template name="at:single-record-component">
                <xsl:with-param name="indent" select="$IIII"/>
              </xsl:call-template>
              <xsl:value-of select="$blank-line"/>

            </xsl:for-each>

            <xsl:value-of select="$II"/>
            <xsl:text>end case;&#10;</xsl:text>
            <xsl:value-of select="$I"/>
            <xsl:text>end record;&#10;</xsl:text>

          </xsl:when>

          <xsl:when test="@protected">

            <xsl:call-template name="ty:protected-type-spec"/>

          </xsl:when>

          <xsl:when test="@extends">

            <!--
                 type {name} is new {base} with record
                   {attr-name} : {attr-type}[ := {attr-init}];
                 end record;
                 -->

            <xsl:value-of select="$I"/>
            <xsl:text>type </xsl:text>
            <xsl:value-of select="name"/>
            <xsl:text> is new </xsl:text>
            <xsl:value-of select="@extends"/>
            <xsl:text> with record&#10;</xsl:text>
            <xsl:apply-templates mode="at:instance-record-component"/>
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
            <xsl:value-of select="$blank-line"/>
            <xsl:apply-templates mode="at:instance-record-component"/>
            <xsl:value-of select="$I"/>
            <xsl:text>end record;&#10;</xsl:text>

          </xsl:otherwise>

        </xsl:choose>

      </xsl:when>

      <xsl:when test="array">
        <xsl:value-of select="$I"/>
        <xsl:text>type </xsl:text>
        <xsl:value-of select="name"/>
        <xsl:text> is&#10;</xsl:text>
        <xsl:value-of select="$IC"/>
        <xsl:text>array (</xsl:text>
        <xsl:value-of select="array/index"/>
        <xsl:if test="array/@unconstrained">
          <xsl:text> range &lt;&gt;</xsl:text>
        </xsl:if>
        <xsl:text>)&#10;</xsl:text>
        <xsl:value-of select="$IC"/>
        <xsl:text>of </xsl:text>
        <xsl:call-template name="ut:type-name">
          <xsl:with-param name="type" select="array/type"/>
          <xsl:with-param name="class" select="."/>
          <xsl:with-param name="is-class" select="'no'"/>
        </xsl:call-template>
        <xsl:text>;&#10;</xsl:text>
        <xsl:if test="@atomic">
          <xsl:value-of select="$I"/>
          <xsl:text>pragma Atomic_Components (</xsl:text>
          <xsl:value-of select="name"/>
          <xsl:text>);&#10;</xsl:text>
        </xsl:if>
        <xsl:if test="@volatile">
          <xsl:value-of select="$I"/>
          <xsl:text>pragma Volatile_Components (</xsl:text>
          <xsl:value-of select="name"/>
          <xsl:text>);&#10;</xsl:text>
        </xsl:if>
      </xsl:when>

      <xsl:when test="counterpart">
        <xsl:value-of select="$I"/>
        <xsl:text>subtype </xsl:text>
        <xsl:value-of select="name"/>
        <xsl:text> is ColdFrame.Instances.Handle;&#10;</xsl:text>
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
        <xsl:if test="@atomic">
          <xsl:value-of select="$I"/>
          <xsl:text>pragma Atomic (</xsl:text>
          <xsl:value-of select="name"/>
          <xsl:text>);&#10;</xsl:text>
        </xsl:if>
        <xsl:if test="@volatile">
          <xsl:value-of select="$I"/>
          <xsl:text>pragma Volatile (</xsl:text>
          <xsl:value-of select="name"/>
          <xsl:text>);&#10;</xsl:text>
        </xsl:if>
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
        <xsl:text>use all type </xsl:text>
        <xsl:value-of select="name"/>
        <xsl:text>;&#10;</xsl:text>
      </xsl:when>

      <xsl:when test="@null">
        <xsl:value-of select="$I"/>
        <xsl:text>type </xsl:text>
        <xsl:value-of select="name"/>
        <xsl:text> is null record;&#10;</xsl:text>
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
        <xsl:text>use all type </xsl:text>
        <xsl:value-of select="name"/>
        <xsl:text>;&#10;</xsl:text>
      </xsl:when>

      <xsl:when test="integer">

        <!--
             type {type} is range {lower} .. {upper};
             -->

        <xsl:value-of select="$I"/>
        <xsl:text>type </xsl:text>
        <xsl:value-of select="name"/>
        <xsl:text> is range </xsl:text>
        <xsl:choose>
          <xsl:when test="integer/lower">
            <xsl:value-of select="integer/lower"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:message>
              <xsl:text>Warning: lower bound not specified for </xsl:text>
               <xsl:value-of select="name"/>
            </xsl:message>
            <xsl:text>Integer'First</xsl:text>
          </xsl:otherwise>
        </xsl:choose>
        <xsl:text> .. </xsl:text>
        <xsl:choose>
          <xsl:when test="integer/upper">
            <xsl:value-of select="integer/upper"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:message>
              <xsl:text>Warning: upper bound not specified for </xsl:text>
               <xsl:value-of select="name"/>
            </xsl:message>
            <xsl:text>Integer'Last</xsl:text>
          </xsl:otherwise>
        </xsl:choose>
        <xsl:text>;&#10;</xsl:text>
        <xsl:if test="@atomic">
          <xsl:value-of select="$I"/>
          <xsl:text>pragma Atomic (</xsl:text>
          <xsl:value-of select="name"/>
          <xsl:text>);&#10;</xsl:text>
        </xsl:if>
        <xsl:if test="@volatile">
          <xsl:value-of select="$I"/>
          <xsl:text>pragma Volatile (</xsl:text>
          <xsl:value-of select="name"/>
          <xsl:text>);&#10;</xsl:text>
        </xsl:if>
      </xsl:when>

      <xsl:when test="real">

        <!--
             subtype {type} is [Long_]Float
               range {lower} .. {upper};
             -->

        <xsl:variable name="base">
          <xsl:choose>
            <xsl:when test="real/digits &gt; 6">
              <xsl:text>Long_Float</xsl:text>
            </xsl:when>
            <xsl:otherwise>
              <xsl:text>Float</xsl:text>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:variable>

        <xsl:value-of select="$I"/>
        <xsl:text>subtype </xsl:text>
        <xsl:value-of select="name"/>
        <xsl:text> is </xsl:text>
        <xsl:value-of select="$base"/>
        <xsl:text>&#10;</xsl:text>
        <xsl:value-of select="$IC"/>
        <xsl:text>range </xsl:text>
        <xsl:choose>
          <xsl:when test="real/lower or real/upper">
            <xsl:choose>
              <xsl:when test="real/lower">
                <xsl:value-of select="real/lower"/>
              </xsl:when>
              <xsl:otherwise>
                <xsl:message>
                  <xsl:text>Warning: lower bound not specified for </xsl:text>
                  <xsl:value-of select="name"/>
                </xsl:message>
                <xsl:value-of select="$base"/>
                <xsl:text>'First</xsl:text>
              </xsl:otherwise>
            </xsl:choose>
            <xsl:text> .. </xsl:text>
            <xsl:choose>
              <xsl:when test="real/upper">
                <xsl:value-of select="real/upper"/>
              </xsl:when>
              <xsl:otherwise>
                <xsl:message>
                  <xsl:text>Warning: upper bound not specified for </xsl:text>
                  <xsl:value-of select="name"/>
                </xsl:message>
                <xsl:value-of select="$base"/>
                <xsl:text>'Last</xsl:text>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="$base"/>
            <xsl:text>'First .. </xsl:text>
            <xsl:value-of select="$base"/>
            <xsl:text>'Last</xsl:text>
          </xsl:otherwise>
        </xsl:choose>
        <xsl:text>;&#10;</xsl:text>

      </xsl:when>

      <xsl:when test="string/max">
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
        <xsl:value-of select="$I"/>
        <xsl:text>use type </xsl:text>
        <xsl:value-of select="name"/>
        <xsl:text>;&#10;</xsl:text>
      </xsl:when>

      <xsl:when test="string/fixed">
        <xsl:value-of select="$I"/>
        <xsl:text>subtype </xsl:text>
        <xsl:value-of select="name"/>
        <xsl:text> is String (1 .. </xsl:text>
        <xsl:value-of select="string/fixed"/>
        <xsl:text>);&#10;</xsl:text>
      </xsl:when>

      <xsl:when test="subtype">

        <!--
             subtype {type}
                is {type}
                range {lower} .. {upper};
             -->

        <xsl:value-of select="$I"/>
        <xsl:text>subtype </xsl:text>
        <xsl:value-of select="name"/>
        <xsl:text>&#10;</xsl:text>
        <xsl:value-of select="$IC"/>
        <xsl:text>is </xsl:text>
        <xsl:value-of select="subtype/@constrains"/>
        <xsl:text>&#10;</xsl:text>
        <xsl:value-of select="$IC"/>
        <xsl:text>range </xsl:text>
        <xsl:choose>
          <xsl:when test="subtype/lower">
            <xsl:value-of select="subtype/lower"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:message>
              <xsl:text>Warning: lower bound not specified for </xsl:text>
               <xsl:value-of select="name"/>
            </xsl:message>
            <xsl:value-of select="subtype/@constrains"/>
            <xsl:text>'First</xsl:text>
          </xsl:otherwise>
        </xsl:choose>
        <xsl:text> .. </xsl:text>
        <xsl:choose>
          <xsl:when test="subtype/upper">
            <xsl:value-of select="subtype/upper"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:message>
              <xsl:text>Warning: upper bound not specified for </xsl:text>
               <xsl:value-of select="name"/>
            </xsl:message>
            <xsl:value-of select="subtype/@constrains"/>
            <xsl:text>'Last</xsl:text>
          </xsl:otherwise>
        </xsl:choose>
        <xsl:text>;&#10;</xsl:text>
      </xsl:when>

      <xsl:when test="unsigned">

        <!--
             type {type} is mod {mod};
             -->

        <xsl:value-of select="$I"/>
        <xsl:text>type </xsl:text>
        <xsl:value-of select="name"/>
        <xsl:text> is mod </xsl:text>
        <xsl:value-of select="unsigned/mod"/>
        <xsl:text>;&#10;</xsl:text>
        <xsl:if test="@atomic">
          <xsl:value-of select="$I"/>
          <xsl:text>pragma Atomic (</xsl:text>
          <xsl:value-of select="name"/>
          <xsl:text>);&#10;</xsl:text>
        </xsl:if>
        <xsl:if test="@volatile">
          <xsl:value-of select="$I"/>
          <xsl:text>pragma Volatile (</xsl:text>
          <xsl:value-of select="name"/>
          <xsl:text>);&#10;</xsl:text>
        </xsl:if>
      </xsl:when>

      <xsl:otherwise>
        <xsl:message>
          <xsl:call-template name="ut:log-error"/>
          <xsl:text>CF: unrecognised type category for </xsl:text>
          <xsl:value-of select="name"/>
        </xsl:message>
      </xsl:otherwise>

    </xsl:choose>

    <!-- pragma Convention, if required. -->
    <xsl:if test="@convention">
      <xsl:value-of select="$I"/>
      <xsl:text>pragma Convention (</xsl:text>
      <xsl:value-of select="@convention"/>
      <xsl:text>, </xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text>);&#10;</xsl:text>
    </xsl:if>

    <!-- Generate access types (if required) immediately after the type
         they access. -->
    <xsl:if test="@access">
      <xsl:value-of select="$I"/>
      <xsl:text>type </xsl:text>
      <xsl:value-of select="@access"/>
      <xsl:text> is access all </xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text>;&#10;</xsl:text>
      <xsl:value-of select="$I"/>
      <xsl:text>for </xsl:text>
      <xsl:value-of select="@access"/>
      <xsl:text>'Storage_Size use 0;&#10;</xsl:text>
    </xsl:if>

  </xsl:template>


  <!-- Generate domain Types support entries (not for standard types).
       We do these as child units in case they're not actually
       needed. -->
  <xsl:template mode="ty:domain-type-support" match="domain/type">
    <xsl:if test="not(@standard)">
      <xsl:choose>

        <xsl:when test="array"/>

        <xsl:when test="attribute"/>

        <xsl:when test="counterpart"/>

        <xsl:when test="enumeration"/>

        <xsl:when test="imported"/>

        <xsl:when test="integer"/>

        <xsl:when test="@null"/>

        <xsl:when test="real"/>

        <xsl:when test="renames"/>

        <xsl:when test="string/fixed"/>

        <xsl:when test="string/max">
          <xsl:call-template name="ut:do-not-edit"/>
          <xsl:text>pragma Style_Checks (Off);&#10;</xsl:text>
          <xsl:call-template name="ut:identification-info"/>
          <xsl:text>with ColdFrame.Hash.Strings.Bounded;&#10;</xsl:text>
          <xsl:text>private function </xsl:text>
          <xsl:value-of select="../name"/>
          <xsl:text>.</xsl:text>
          <xsl:value-of select="name"/>
          <xsl:text>_Hash&#10;</xsl:text>
          <xsl:text>is new ColdFrame.Hash.Strings.Bounded (</xsl:text>
          <xsl:value-of select="../name"/>
          <xsl:text>.</xsl:text>
          <xsl:value-of select="name"/>
          <xsl:text>_Package);&#10;</xsl:text>
        </xsl:when>

        <xsl:when test="subtype"/>

        <xsl:when test="unsigned"/>

        <xsl:otherwise>
          <xsl:text>--  Unrecognised type category for </xsl:text>
          <xsl:value-of select="name"/>
          <xsl:text>&#10;</xsl:text>
        </xsl:otherwise>

      </xsl:choose>
    </xsl:if>
  </xsl:template>

  <xsl:template mode="ty:domain-type-support" match="*"/>


  <!-- Called to generate operation specifications for types. -->
  <xsl:template match="type/operation" mode="ty:domain-type-operation-spec">

    <xsl:call-template name="op:subprogram-specification">
      <xsl:with-param name="indent" select="$I"/>
      <xsl:with-param name="is-class" select="'no'"/>
    </xsl:call-template>

    <xsl:if test="@renames">
      <xsl:text>&#10;</xsl:text>
      <xsl:value-of select="$IC"/>
      <xsl:text>renames </xsl:text>
      <xsl:value-of select="@renames"/>
    </xsl:if>

    <xsl:text>;&#10;</xsl:text>

    <!-- Specify the Convention, if needed. -->
    <xsl:if test="@convention">
      <xsl:value-of select="$I"/>
      <xsl:text>pragma Convention (</xsl:text>
      <xsl:value-of select="@convention"/>
      <xsl:text>, </xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text>);&#10;</xsl:text>
    </xsl:if>

    <xsl:call-template name="ut:commentary">
      <xsl:with-param name="indent" select="$I"/>
    </xsl:call-template>
    <xsl:value-of select="$blank-line"/>
  </xsl:template>

  <xsl:template mode="ty:domain-type-operation-spec" match="*"/>


  <!-- Called to generate operation body stubs for types. -->
  <xsl:template
    match="type/operation[not(@renames)]"
    mode="ty:domain-type-operation-body-stub">
    <xsl:value-of select="$I"/>
    <xsl:text>pragma Style_Checks (On);&#10;</xsl:text>
    <xsl:call-template name="op:subprogram-specification">
      <xsl:with-param name="indent" select="$I"/>
      <xsl:with-param name="is-class" select="'no'"/>
    </xsl:call-template>
    <xsl:text> is separate;&#10;</xsl:text>
    <xsl:value-of select="$I"/>
    <xsl:text>pragma Style_Checks (Off);&#10;</xsl:text>
    <xsl:value-of select="$blank-line"/>
  </xsl:template>

  <xsl:template mode="ty:domain-type-operation-body-stub" match="*"/>


  <!-- Called to generate operation bodies for types. -->
  <xsl:template
    match="type/operation[not(@renames)]"
    mode="ty:domain-type-operation-body">

    <xsl:choose>

      <xsl:when test="$generate-stubs='yes'">

        <xsl:variable name="subprogram-name">
          <xsl:value-of select="../../name"/>
          <xsl:text>.</xsl:text>
          <xsl:value-of select="name"/>
        </xsl:variable>

        <xsl:call-template name="ut:should-not-edit"/>
        <xsl:call-template name="ut:identification-info"/>

        <xsl:value-of select="$blank-line"/>
        <xsl:call-template name="ut:commentary">
          <xsl:with-param name="indent" select="''"/>
          <xsl:with-param name="separate-pars" select="$blank-line"/>
        </xsl:call-template>

        <xsl:text>pragma Style_Checks (Off);&#10;</xsl:text>
        <xsl:text>with ColdFrame.Stubs;&#10;</xsl:text>
        <xsl:value-of select="$blank-line"/>

        <xsl:text>separate (</xsl:text>
        <xsl:value-of select="../../name"/>
        <xsl:text>)&#10;</xsl:text>
        <xsl:call-template name="op:subprogram-specification">
          <xsl:with-param name="indent" select="''"/>
          <xsl:with-param name="is-class" select="'no'"/>
        </xsl:call-template>
        <xsl:text> is&#10;</xsl:text>

        <xsl:value-of select="$I"/>
        <xsl:text>Lock : ColdFrame.Stubs.Lock (ColdFrame.Stubs.Mutex'Access);&#10;</xsl:text>
        <xsl:value-of select="$I"/>
        <xsl:text>pragma Unreferenced (Lock);&#10;</xsl:text>
        <xsl:value-of select="$I"/>
        <xsl:text>Call : constant Positive := ColdFrame.Stubs.Note_Entry&#10;</xsl:text>
        <xsl:value-of select="$IC"/>
        <xsl:text>(&quot;</xsl:text>
        <xsl:value-of select="$subprogram-name"/>
        <xsl:text>&quot;);&#10;</xsl:text>
        <xsl:text>begin&#10;</xsl:text>

        <xsl:for-each select="parameter[not(@mode) or @mode='inout']">
          <xsl:value-of select="$I"/>
          <xsl:call-template name="ut:type-name">
            <xsl:with-param name="type" select="type"/>
            <xsl:with-param name="class" select=".."/>
            <xsl:with-param name="is-class" select="'no'"/>
          </xsl:call-template>
          <xsl:text>'Output&#10;</xsl:text>
          <xsl:value-of select="$IC"/>
          <xsl:text>(ColdFrame.Stubs.Get_Input_Value_Stream&#10;</xsl:text>
          <xsl:value-of select="$IIC"/>
          <xsl:text>(&quot;</xsl:text>
          <xsl:value-of select="$subprogram-name"/>
          <xsl:text>&quot;, &quot;</xsl:text>
          <xsl:value-of select="name"/>
          <xsl:text>&quot;, Call, </xsl:text>
          <xsl:value-of select="name"/>
          <xsl:text>'Size),&#10;</xsl:text>
          <xsl:value-of select="$IC"/>
          <xsl:text> </xsl:text>
          <xsl:value-of select="name"/>
          <xsl:text>);&#10;</xsl:text>
        </xsl:for-each>
        <xsl:value-of select="$I"/>
        <xsl:text>ColdFrame.Stubs.Check_For_Exception&#10;</xsl:text>
        <xsl:value-of select="$IC"/>
        <xsl:text>(&quot;</xsl:text>
        <xsl:value-of select="$subprogram-name"/>
        <xsl:text>&quot;, Call);&#10;</xsl:text>
        <xsl:for-each select="parameter[@mode='inout' or @mode='out']">
          <xsl:value-of select="$I"/>
          <xsl:value-of select="name"/>
          <xsl:text> := </xsl:text>
          <xsl:call-template name="ut:type-name">
            <xsl:with-param name="type" select="type"/>
            <xsl:with-param name="class" select=".."/>
            <xsl:with-param name="is-class" select="'no'"/>
          </xsl:call-template>
          <xsl:text>'Input&#10;</xsl:text>
          <xsl:value-of select="$IC"/>
          <xsl:text>(ColdFrame.Stubs.Get_Output_Value_Stream&#10;</xsl:text>
          <xsl:value-of select="$IIC"/>
          <xsl:text>(&quot;</xsl:text>
          <xsl:value-of select="$subprogram-name"/>
          <xsl:text>&quot;, &quot;</xsl:text>
          <xsl:value-of select="name"/>
          <xsl:text>&quot;, Call));&#10;</xsl:text>
        </xsl:for-each>
        <xsl:if test="@return">
          <xsl:value-of select="$I"/>
          <xsl:text>return </xsl:text>
          <xsl:call-template name="ut:type-name">
            <xsl:with-param name="type" select="@return"/>
            <xsl:with-param name="class" select=".."/>
            <xsl:with-param name="is-class" select="'no'"/>
          </xsl:call-template>
          <xsl:text>'Input&#10;</xsl:text>
          <xsl:value-of select="$IC"/>
          <xsl:text>(ColdFrame.Stubs.Get_Output_Value_Stream&#10;</xsl:text>
          <xsl:value-of select="$IIC"/>
          <xsl:text>(&quot;</xsl:text>
          <xsl:value-of select="$subprogram-name"/>
          <xsl:text>&quot;, &quot;return&quot;, Call));&#10;</xsl:text>
        </xsl:if>

        <xsl:text>end </xsl:text>
        <xsl:value-of select="name"/>
        <xsl:text>;&#10;</xsl:text>

      </xsl:when>

      <xsl:otherwise>

        <xsl:call-template name="ut:should-edit"/>
        <xsl:value-of select="$blank-line"/>

        <xsl:call-template name="ut:commentary">
          <xsl:with-param name="indent" select="''"/>
          <xsl:with-param name="separate-pars" select="$blank-line"/>
        </xsl:call-template>

        <xsl:text>separate (</xsl:text>
        <xsl:value-of select="../../name"/>
        <xsl:text>)&#10;</xsl:text>
        <xsl:call-template name="op:subprogram-specification">
          <xsl:with-param name="indent" select="''"/>
          <xsl:with-param name="is-class" select="'no'"/>
        </xsl:call-template>
        <xsl:text> is&#10;</xsl:text>

        <!-- If it's a function, we have to organize a return value. -->
        <xsl:variable name="return-info">
          <xsl:if test="@return">
            <xsl:element name="return-type">
              <xsl:call-template name="ut:type-name">
                <xsl:with-param name="type" select="@return"/>
                <xsl:with-param name="is-class" select="'no'"/>
              </xsl:call-template>
            </xsl:element>
            <xsl:element name="return-value">
              <xsl:call-template name="ut:dummy-return-value">
                <xsl:with-param name="type" select="@return"/>
              </xsl:call-template>
            </xsl:element>
          </xsl:if>
        </xsl:variable>

        <xsl:if test="@return and $return-info/return-value=''">
          <xsl:value-of select="$I"/>
          <xsl:text>Dummy : </xsl:text>
          <xsl:value-of select="$return-info/return-type"/>
          <xsl:text>;&#10;</xsl:text>
        </xsl:if>

        <xsl:value-of select="$I"/>
        <xsl:text>Unimplemented : exception;&#10;</xsl:text>
        <xsl:text>begin&#10;</xsl:text>
        <xsl:value-of select="$I"/>
        <xsl:text>raise Unimplemented;&#10;</xsl:text>

        <xsl:if test="@return">

          <xsl:value-of select="$I"/>
          <xsl:text>return </xsl:text>

          <xsl:choose>

            <xsl:when test="not($return-info/return-value='')">
              <xsl:value-of select="$return-info/return-value"/>
            </xsl:when>

            <xsl:otherwise>
              <xsl:text>Dummy</xsl:text>
            </xsl:otherwise>

          </xsl:choose>

          <xsl:text>;&#10;</xsl:text>

        </xsl:if>

        <xsl:text>end </xsl:text>
        <xsl:value-of select="name"/>
        <xsl:text>;&#10;</xsl:text>

      </xsl:otherwise>

    </xsl:choose>

  </xsl:template>

  <xsl:template mode="ty:domain-type-operation-body" match="*"/>


  <!-- Called at domain/type[@protected] to generate protected type specs. -->
  <xsl:template name="ty:protected-type-spec">

    <!-- The error of having no attributes is detected in normalization. -->
    <xsl:if test="count(operation) = 0">
      <xsl:message>
        <xsl:call-template name="ut:log-error"/>
        <xsl:text>Error: protected type </xsl:text>
        <xsl:value-of select="name"/>
        <xsl:text> has no operations</xsl:text>
      </xsl:message>
     </xsl:if>

    <!--
         protected type {name} is
            {operations}
         private
            {operations}
            {attributes}
         end {name};
         -->

    <xsl:value-of select="$I"/>
    <xsl:text>protected type </xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text> is&#10;</xsl:text>

    <xsl:value-of select="$blank-line"/>

    <xsl:for-each select="operation[not(@access)
                                    and not(@suppressed)
                                    and not(@visibility='private')]">
      <xsl:sort select="name"/>
      <xsl:call-template name="op:subprogram-specification">
        <xsl:with-param name="indent" select="$II"/>
        <xsl:with-param name="is-class" select="'no'"/>
      </xsl:call-template>
      <xsl:text>;&#10;</xsl:text>
      <xsl:call-template name="ut:commentary">
        <xsl:with-param name="indent" select="$II"/>
      </xsl:call-template>
      <xsl:value-of select="$blank-line"/>
    </xsl:for-each>

    <xsl:value-of select="$I"/>
    <xsl:text>private&#10;</xsl:text>

    <xsl:for-each select="operation[not(@access)
                                    and not(@suppressed)
                                    and @visibility='private']">
      <xsl:sort select="name"/>
      <xsl:call-template name="op:subprogram-specification">
        <xsl:with-param name="indent" select="$II"/>
        <xsl:with-param name="is-class" select="'no'"/>
      </xsl:call-template>
      <xsl:text>;&#10;</xsl:text>
      <xsl:call-template name="ut:commentary">
        <xsl:with-param name="indent" select="$II"/>
      </xsl:call-template>
    </xsl:for-each>

    <xsl:apply-templates mode="at:instance-record-component"/>

    <xsl:value-of select="$I"/>
    <xsl:text>end </xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>;&#10;</xsl:text>

    <!-- Commentary output by caller. -->

  </xsl:template>


  <!-- Called to generate protected type bodies. -->
  <xsl:template match="type[@protected]" mode="ty:protected-type-body">

    <xsl:call-template name="ut:should-edit"/>
    <xsl:value-of select="$blank-line"/>

    <xsl:call-template name="ut:commentary">
      <xsl:with-param name="indent" select="''"/>
      <xsl:with-param name="separate-pars" select="$blank-line"/>
    </xsl:call-template>

    <xsl:text>separate (</xsl:text>
    <xsl:value-of select="../name"/>
    <xsl:text>)&#10;</xsl:text>
    <xsl:text>protected body </xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text> is&#10;</xsl:text>
    <xsl:value-of select="$blank-line"/>
    <xsl:text>&#10;</xsl:text>

    <xsl:for-each select="operation[not(@access) and not(@suppressed)]">
      <xsl:sort select="name"/>

      <xsl:call-template name="ut:commentary">
        <xsl:with-param name="indent" select="$I"/>
      </xsl:call-template>

      <xsl:call-template name="op:subprogram-specification">
        <xsl:with-param name="indent" select="$I"/>
        <xsl:with-param name="is-class" select="'no'"/>
      </xsl:call-template>
      <xsl:if test="@entry">
        <xsl:text> when False</xsl:text>
      </xsl:if>
      <xsl:text> is&#10;</xsl:text>

      <!-- If it's a function, we have to organize a return value. -->
      <xsl:variable name="return-info">
        <xsl:if test="@return">
          <xsl:element name="return-type">
            <xsl:call-template name="ut:type-name">
              <xsl:with-param name="type" select="@return"/>
              <xsl:with-param name="is-class" select="'no'"/>
            </xsl:call-template>
          </xsl:element>
          <xsl:element name="return-value">
            <xsl:call-template name="ut:dummy-return-value">
              <xsl:with-param name="type" select="@return"/>
            </xsl:call-template>
          </xsl:element>
        </xsl:if>
      </xsl:variable>

      <xsl:if test="@return and $return-info/return-value=''">
        <xsl:value-of select="$II"/>
        <xsl:text>Dummy : </xsl:text>
        <xsl:value-of select="$return-info/return-type"/>
        <xsl:text>;&#10;</xsl:text>
      </xsl:if>

      <xsl:value-of select="$II"/>
      <xsl:text>Unimplemented : exception;&#10;</xsl:text>
      <xsl:value-of select="$I"/>
      <xsl:text>begin&#10;</xsl:text>
      <xsl:value-of select="$II"/>
      <xsl:text>raise Unimplemented;&#10;</xsl:text>

      <xsl:if test="@return">

        <xsl:value-of select="$II"/>
        <xsl:text>return </xsl:text>

        <xsl:choose>

          <xsl:when test="not($return-info/return-value='')">
            <xsl:value-of select="$return-info/return-value"/>
          </xsl:when>

          <xsl:otherwise>
            <xsl:text>Dummy</xsl:text>
          </xsl:otherwise>

        </xsl:choose>

        <xsl:text>;&#10;</xsl:text>

      </xsl:if>

      <xsl:value-of select="$I"/>
      <xsl:text>end </xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text>;&#10;</xsl:text>
      <xsl:value-of select="$blank-line"/>
      <xsl:text>&#10;</xsl:text>

    </xsl:for-each>
    <xsl:text>end </xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>;&#10;</xsl:text>

  </xsl:template>

  <xsl:template match="*" mode="ty:protected-type-body"/>


  <!-- Called to extract the package name from a (possibly) qualified
       type name. -->
  <xsl:template name="ty:find-source-package">
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

        <xsl:call-template name="ty:find-source-package">
          <xsl:with-param name="input" select="$after"/>
          <xsl:with-param name="package" select="$new-package"/>
        </xsl:call-template>

      </xsl:otherwise>

    </xsl:choose>

  </xsl:template>


</xsl:stylesheet>
