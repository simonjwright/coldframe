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

     As a special exception, when portions of this file are copied by
     a stylesheet processor into an output file, this file does not by
     itself cause the resulting file to be covered by the GNU General
     Public License.  This exception does not however invalidate any
     other reasons why the output file might be covered by the GNU
     Public License.
     -->

<xsl:stylesheet
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:op="http://pushface.org/coldframe/operation"
  xmlns:ut="http://pushface.org/coldframe/utilities"
  version="1.0">


  <!-- called at domain/class to generate subprogram specs for a class.
       Since this may be a child class, we handle all the operations
       in the ancestor tree. -->
  <xsl:template name="op:visible-operation-specs">

    <!-- The classes to be processed this time. The default is the
         current class. -->
    <xsl:param name="parents" select="."/>

    <!-- The operations so far. The default value is "null". -->
    <xsl:param name="operations" select="/.."/>

    <!-- The starting class. -->
    <xsl:param name="current" select="."/>

    <xsl:choose>

      <xsl:when test="$parents">

        <!-- Still something to collect; call self recursively with the
             parent node(s), omitting operations we already have and
             <<generated>> (etc) operations. -->
        <xsl:call-template name="op:visible-operation-specs">
          <xsl:with-param
            name="parents"
            select="../class[name=../inheritance[child=$parents/name]/parent]"/>
          <xsl:with-param
            name="operations"
            select="$parents/operation
                      [not(name=$operations/name)
                       and not(@visibility='private')
                       and not(@access)
                       and not(@suppressed)
                       and not(@entry)
                       and not(@renames)]
                    | $operations"/>
          <xsl:with-param
            name="current"
            select="$current"/>
        </xsl:call-template>

      </xsl:when>

      <xsl:otherwise>

        <!-- $operations contains all the nodes to be processed. -->

        <!-- Check for a private (and interesting!) operation with the
             same name as an (inherited) public one.
             Clearly there should be some rules here, preferably
             embodied at some more global place (eg, in
             normalization). For example, what would the designer have
             intended by marking a renaming private? -->
        <xsl:for-each
          select="$operations[name
                  =current()/operation
                    [@visibility='private'
                     and not(@suppressed)
                     and not(@entry)]/name]">
          <xsl:call-template name="ut:log-error"/>
          <xsl:message>
            <xsl:text>Error: "inherited" private operation </xsl:text>
            <xsl:value-of select="../name"/>.<xsl:value-of select="name"/>
            <xsl:text> in </xsl:text>
            <xsl:value-of select="$current/name"/>
          </xsl:message>
        </xsl:for-each>

        <xsl:apply-templates select="$operations" mode="op:operation-spec">
          <xsl:sort select="name"/>
          <xsl:with-param name="current" select="."/>
        </xsl:apply-templates>

      </xsl:otherwise>

    </xsl:choose>

  </xsl:template>


  <!-- Generate subprogram specs (but not parental <<class>> or
       <<finalize>> operations, or any access-to-operations). -->
  <xsl:template match="class/operation" mode="op:operation-spec">

    <!-- The current class. -->
    <xsl:param name="current"/>

    <xsl:if test="..=$current or not(@class or @finalize)">

      <xsl:if test="@accessor">
        <xsl:value-of select="$I"/>
        <xsl:text>--  Accessor&#10;</xsl:text>
      </xsl:if>

      <xsl:call-template name="op:subprogram-specification">
        <xsl:with-param name="indent" select="$I"/>
      </xsl:call-template>
      <xsl:text>;&#10;</xsl:text>

      <!-- Inline accessors of the current class. -->
      <!-- or not!
      <xsl:if test="@accessor and ..=$current">
        <xsl:value-of select="$I"/>
        <xsl:text>pragma Inline_Always (</xsl:text>
        <xsl:value-of select="name"/>
        <xsl:text>);&#10;</xsl:text>
      </xsl:if>
      -->

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

    </xsl:if>

  </xsl:template>

  <xsl:template mode="op:operation-spec" match="*"/>


  <!-- Generate renaming operation specs. -->
  <xsl:template match="operation[@renames]" mode="op:renaming-operation-spec">

    <xsl:call-template name="op:subprogram-specification">
      <xsl:with-param name="indent" select="$I"/>
    </xsl:call-template>
    <xsl:text>&#10;</xsl:text>
    <xsl:value-of select="$IC"/>
    <xsl:text>renames </xsl:text>
    <xsl:value-of select="@renames"/>
    <xsl:text>;&#10;</xsl:text>
    <xsl:call-template name="ut:commentary">
      <xsl:with-param name="indent" select="$I"/>
    </xsl:call-template>
    <xsl:value-of select="$blank-line"/>

  </xsl:template>

  <xsl:template mode="op:renaming-operation-spec" match="*"/>


  <!-- called at domain/class to generate subprogram body parts
       (proper bodies or stubs) for a class. Since this may be a child
       type, we handle all the operations in the ancestor tree. -->
  <xsl:template name="op:operation-body-parts">

    <!-- The classes to be processed this time. The default is the
         current class. -->
    <xsl:param name="parents" select="."/>

    <!-- The operations so far. The default value is "null". -->
    <xsl:param name="operations" select="/.."/>

    <xsl:choose>

      <xsl:when test="$parents">

        <!-- Still something to collect; call self recursively with the
             parent node(s). -->
        <xsl:call-template name="op:operation-body-parts">

          <xsl:with-param
            name="parents"
            select="../class[name=../inheritance[child=$parents/name]/parent]"/>

          <xsl:with-param
            name="operations"
            select="$parents/operation
                      [not(name=$operations/name)
                       and not(@access)
                       and not(@suppressed)
                       and not(@entry)
                       and not(@renames)]
                    | $operations"/>

        </xsl:call-template>

      </xsl:when>

      <xsl:otherwise>

        <!-- $operations contains all the nodes to be processed. -->

        <xsl:apply-templates
          select="$operations"
          mode="op:operation-body-in-body">
          <xsl:with-param name="current" select="."/>
        </xsl:apply-templates>

      </xsl:otherwise>

    </xsl:choose>

  </xsl:template>


  <!-- Generate the package body parts of operations (but not parental
       <<class>>, <<finalize>> or private operations, or
       access-to-operations, which are realized in the Class
       package). -->
  <xsl:template match="class/operation" mode="op:operation-body-in-body">

    <!-- The current class. -->
    <xsl:param name="current"/>

    <xsl:choose>

      <xsl:when test="$current=..">

        <xsl:choose>

          <xsl:when test="@abstract">

            <!-- Abstract in current class; we need to dispatch downward. -->

            <xsl:call-template name="op:generate-dispatch-to-child">
              <xsl:with-param name="current" select="$current"/>
            </xsl:call-template>

          </xsl:when>

          <xsl:when test="@accessor">

            <xsl:call-template name="op:generate-accessor-body">
              <xsl:with-param name="current" select="$current"/>
            </xsl:call-template>

          </xsl:when>

          <xsl:when test="body">

            <xsl:call-template name="op:generate-body-with-body-text">
              <xsl:with-param name="current" select="$current"/>
            </xsl:call-template>

          </xsl:when>

          <xsl:otherwise>

            <!-- Concrete in current class; we provide a stub. -->
            <xsl:value-of select="$I"/>
            <xsl:text>pragma Style_Checks (On);&#10;</xsl:text>
            <xsl:call-template name="op:subprogram-specification">
              <xsl:with-param name="indent" select="$I"/>
            </xsl:call-template>
            <xsl:text>&#10;</xsl:text>
            <xsl:value-of select="$I"/>
            <xsl:text>is separate;&#10;</xsl:text>
            <xsl:value-of select="$I"/>
            <xsl:text>pragma Style_Checks (Off);&#10;</xsl:text>
            <xsl:value-of select="$blank-line"/>

          </xsl:otherwise>

        </xsl:choose>

      </xsl:when>

      <xsl:otherwise>

        <!-- The operation is declared in an ancestor class. -->

        <xsl:choose>

          <xsl:when test="@abstract">

            <!-- Abstract in ancestor class; we need to dispatch
                 downward (if there's anywhere to go). -->

            <xsl:if test="not(/domain/inheritance[parent=$current/name])">
              <xsl:call-template name="ut:log-error"/>
              <xsl:message>
                <xsl:text>Error: no concrete operation for </xsl:text>
                <xsl:value-of select="../name"/>.<xsl:value-of select="name"/>
                <xsl:text> in </xsl:text>
                <xsl:value-of select="$current/name"/>
              </xsl:message>
            </xsl:if>

            <xsl:call-template name="op:generate-dispatch-to-child">
              <xsl:with-param name="current" select="$current"/>
            </xsl:call-template>

          </xsl:when>

          <xsl:when test="not(@class or @finalize)
                          and not(@visibility='private')">

            <!-- Concrete, non-<<class>>, non-<<finalize>>, visible in
                 ancestor class; we need to call the operation in our
                 parent. -->

            <xsl:call-template name="op:generate-dispatch-to-parent">
              <xsl:with-param name="current" select="$current"/>
            </xsl:call-template>

          </xsl:when>

        </xsl:choose>

      </xsl:otherwise>

    </xsl:choose>

  </xsl:template>

  <xsl:template mode="op:operation-body-in-body" match="*"/>


  <!-- called at domain/class to generate subprogram bodies for a class.
       Since this may be a child type, we handle all the operations
       in the ancestor tree. -->
  <xsl:template name="op:operation-separate-bodies">

    <!-- The classes to be processed this time. The default is the
         current class. -->
    <xsl:param name="parents" select="."/>

    <!-- The operations so far. The default value is "null". -->
    <xsl:param name="operations" select="/.."/>

    <xsl:choose>

      <xsl:when test="$parents">

        <!-- Still something to collect; call self recursively with the
             parent node(s). -->
        <xsl:call-template name="op:operation-separate-bodies">
          <xsl:with-param
            name="parents"
            select="../class[name=../inheritance[child=$parents/name]/parent]"/>
          <xsl:with-param
            name="operations"
            select="$parents/operation
                      [not(@suppressed)
                       and not(@entry)
                       and not(body)
                       and not(name=$operations/name)]
                    | $operations"/>
        </xsl:call-template>

      </xsl:when>

      <xsl:otherwise>

        <!-- $operations contains all the nodes to be processed. -->

        <xsl:apply-templates
          select="$operations"
          mode="op:operation-separate-body">
          <xsl:with-param name="current" select="."/>
        </xsl:apply-templates>

      </xsl:otherwise>

    </xsl:choose>

  </xsl:template>


  <!-- Generate the separate bodies of operations. If recognised as
       accessors (but not marked as such) they will contain real
       implementations; if stubs are required, they'll contain stubs;
       otherwise they'll raise Unimplemented if called. -->
  <xsl:template
    mode="op:operation-separate-body"
    match="class/operation[not(@access)]">

    <!-- The current class (not necessarily the one where the operation
         is defined, if we're talking inheritance) -->
    <xsl:param name="current"/>

    <xsl:if test="$current=..
                  and not(@abstract)
                  and not(@accessor)
                  and not(@renames)">

      <!-- Concrete in current class. -->

      <xsl:call-template name="op:generate-separate-body">
        <xsl:with-param name="current" select="$current"/>
      </xsl:call-template>

    </xsl:if>

  </xsl:template>

  <xsl:template mode="op:operation-separate-body" match="*"/>


  <!-- Called from operation to generate a subprogram specification.
       Ends without the closing ";" or " is". -->
  <xsl:template name="op:subprogram-specification">
    <xsl:param name="indent"/>
    <xsl:param name="is-class" select="'yes'"/>
    <!-- yes means it's an operation of a class, not of a type. -->

    <xsl:variable name="cont" select="concat($indent, $C)"/>

    <xsl:choose>

      <!-- If there's a return attribute, it's a function. -->
      <xsl:when test="@return">
        <!-- Check for entry (illegal for functions) -->
        <xsl:if test="@entry">
          <xsl:call-template name="ut:log-error"/>
          <xsl:message>
            <xsl:text>Error: function </xsl:text>
            <xsl:value-of select="../name"/>.<xsl:value-of select="name"/>
            <xsl:text> can't be an entry</xsl:text>
          </xsl:message>
        </xsl:if>
        <xsl:value-of select="$indent"/>
        <xsl:text>function </xsl:text>
        <xsl:value-of select="name"/>
        <xsl:call-template name="op:parameter-list">
          <xsl:with-param name="indent" select="$cont"/>
          <xsl:with-param name="is-class" select="$is-class"/>
        </xsl:call-template>
        <xsl:text>&#10;</xsl:text>
        <xsl:value-of select="$cont"/>
        <xsl:text>return </xsl:text>
        <xsl:call-template name="ut:type-name">
          <xsl:with-param name="type" select="@return"/>
          <xsl:with-param name="class" select=".."/>
          <xsl:with-param name="is-class" select="$is-class"/>
        </xsl:call-template>
      </xsl:when>

      <!-- If there's no return attribute, it's a procedure or entry. -->
      <xsl:otherwise>
        <xsl:value-of select="$indent"/>
        <xsl:choose>
          <xsl:when test="not(@entry)">
            <xsl:text>procedure </xsl:text>
          </xsl:when>
          <xsl:otherwise>
            <xsl:text>entry </xsl:text>
          </xsl:otherwise>
        </xsl:choose>
        <xsl:value-of select="name"/>
        <xsl:call-template name="op:parameter-list">
          <xsl:with-param name="indent" select="$cont"/>
          <xsl:with-param name="is-class" select="$is-class"/>
         </xsl:call-template>
      </xsl:otherwise>

    </xsl:choose>
  </xsl:template>


  <!-- Called from operation to generate a subprogram parameter list -->
  <xsl:template name="op:parameter-list">
    <xsl:param name="indent" select="''"/>
    <xsl:param name="is-class" select="'yes'"/>

    <!-- In Ada, an empty parameter list is void (not "()" as in C).
         If the operation has parameters, we clearly need a parameter
         list here! Otherwise, we have to check for a Handle; if
         the Class is public, a singleton, a utility, or a Type, all
         operations are class operations, otherwise it depends on the
         @class attribute. -->

    <xsl:variable
      name="no-this"
      select="$is-class='no'
              or ../@public or ../@singleton or ../@utility
              or @class"/>

    <xsl:if test="parameter or not($no-this)">

      <xsl:text>&#10;</xsl:text>
      <xsl:value-of select="$indent"/>
      <xsl:text>(</xsl:text>
      <xsl:if test="not($no-this)">
        <xsl:text>This : not null Handle</xsl:text>
        <xsl:if test="parameter">
          <xsl:text>;&#10; </xsl:text>
          <xsl:value-of select="$indent"/>
        </xsl:if>
      </xsl:if>
      <xsl:apply-templates mode="op:parameter">
        <xsl:with-param name="indent" select="$indent"/>
        <xsl:with-param name="is-class" select="$is-class"/>
      </xsl:apply-templates>
      <xsl:text>)</xsl:text>

    </xsl:if>

  </xsl:template>


  <!-- Called from operation to generate a subprogram parameter -->
  <xsl:template match="operation/parameter" mode="op:parameter">
    <xsl:param name="indent" select="''"/>
    <xsl:param name="is-class" select="'yes'"/>

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

    <xsl:if test="@not-null">
      <xsl:text>not null </xsl:text>
    </xsl:if>

    <xsl:call-template name="ut:type-name">
      <xsl:with-param name="type" select="type"/>
      <xsl:with-param name="class" select="../.."/>
      <xsl:with-param name="is-class" select="$is-class"/>
    </xsl:call-template>

    <xsl:if test="default">
      <xsl:text> := </xsl:text>
      <xsl:value-of select="default"/>
    </xsl:if>

    <xsl:if test="position() &lt; last()">
      <xsl:text>;&#10;</xsl:text>
      <xsl:value-of select="$indent"/>
      <xsl:text> </xsl:text>
    </xsl:if>

  </xsl:template>

  <xsl:template mode="op:parameter" match="*"/>


  <!-- Called at class/operation to generate a dispatch-to-child body. -->
  <xsl:template name="op:generate-dispatch-to-child">

    <!-- The current class (not necessarily the one where the operation
         is defined, if we're talking inheritance) -->
    <xsl:param name="current"/>

    <!--
         case This.{relation}_Current_Child.Current is
           when {child-1}_T =>
             {return} {child-1}.{operation}
               (This => {child-1}.Handle
                  (This.{relation}_Current_Child.{child-1-abbrev}){,
                other-parameter-assignments});
           when Null_T =>
             raise Constraint_Error;
         end case;
         -->

    <!-- Save the current operation. -->
    <xsl:variable name="op" select="."/>

    <!-- XXX won't work with partitioned inheritance -->
    <xsl:variable
      name="rel"
      select="/domain/inheritance[parent=$current/name]"/>

    <xsl:call-template name="op:subprogram-specification">
      <xsl:with-param name="indent" select="$I"/>
    </xsl:call-template>
    <xsl:text> is&#10;</xsl:text>
    <xsl:value-of select="$I"/>
    <xsl:text>begin&#10;</xsl:text>

    <!-- XXX won't work for partitioned inheritance -->
    <xsl:value-of select="$II"/>
    <xsl:text>case This.</xsl:text>
    <xsl:value-of select="$rel/name"/>
    <xsl:text>_Current_Child.Current is&#10;</xsl:text>

    <xsl:for-each select="$rel/child">
      <xsl:sort select="."/>

      <xsl:variable name="child" select="."/>

      <xsl:value-of select="$III"/>
      <xsl:text>when </xsl:text>
      <xsl:value-of select="."/>
      <xsl:text>_T =&gt;&#10;</xsl:text>

      <xsl:value-of select="$IIII"/>
      <xsl:if test="$op/@return">
        <xsl:text>return </xsl:text>
      </xsl:if>
      <xsl:value-of select="."/>.<xsl:value-of select="$op/name"/>
      <xsl:text>&#10;</xsl:text>

      <xsl:value-of select="$IIIIC"/>
      <xsl:text>(This =&gt; </xsl:text>
      <xsl:value-of select="$child"/>
      <xsl:text>.Handle&#10;</xsl:text>
      <xsl:value-of select="$IIIIC"/>
      <xsl:text>   (This.</xsl:text>
      <xsl:value-of select="$rel/name"/>
      <xsl:text>_Current_Child.</xsl:text>
      <xsl:value-of select="/domain/class[name=$child]/abbreviation"/>
      <xsl:text>)</xsl:text>

      <xsl:for-each select="$op/parameter">
        <xsl:text>,&#10;</xsl:text>
        <xsl:value-of select="$IIIIC"/>
        <xsl:text> </xsl:text>
        <xsl:value-of select="name"/>
        <xsl:text> =&gt; </xsl:text>

        <xsl:choose>

          <!-- Test for covariance.
               If the parameter is of the type of the class in which the
               operation is defined, convert to the child's type.
               XXX this may not always be the right thing. -->

          <xsl:when test="type=$op/../name">
            <xsl:value-of select="$child"/>
            <xsl:text>.Handle (</xsl:text>
            <xsl:value-of select="name"/>
            <xsl:text>.</xsl:text>
            <xsl:value-of select="$rel/name"/>
            <xsl:text>_Current_Child.</xsl:text>
            <xsl:value-of select="/domain/class[name=$child]/abbreviation"/>
            <xsl:text>)</xsl:text>
          </xsl:when>

          <xsl:otherwise>
            <xsl:value-of select="name"/>
          </xsl:otherwise>

        </xsl:choose>

      </xsl:for-each>

      <xsl:text>);&#10;</xsl:text>

    </xsl:for-each>

    <xsl:value-of select="$III"/>
    <xsl:text>when Null_T =&gt;&#10;</xsl:text>
    <xsl:value-of select="$IIII"/>
    <xsl:text>raise Constraint_Error;&#10;</xsl:text>

    <xsl:value-of select="$II"/>
    <xsl:text>end case;&#10;</xsl:text>

    <xsl:value-of select="$I"/>
    <xsl:text>end </xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>;&#10;</xsl:text>
    <xsl:value-of select="$blank-line"/>

  </xsl:template>


  <!-- Called at class/operation to generate a dispatch-to-parent body. -->
  <xsl:template name="op:generate-dispatch-to-parent">

    <!-- The current class (not necessarily the one where the operation
         is defined; we're talking inheritance) -->
    <xsl:param name="current"/>

    <!--
         {return} {parent}.{operation-name}
           (This => {parent}.Handle (Get_{relation}_Parent (This)){,
            other-parameter-assignments});
         -->

    <!-- Save the current operation. -->
    <xsl:variable name="op" select="."/>

    <!-- Find the relation through which we've inherited the operation. -->
    <xsl:variable name="rel-name">
      <xsl:call-template name="op:find-implementing-relationship">
          <xsl:with-param name="child" select="$current/name"/>
          <xsl:with-param name="parents" select="../name"/>
      </xsl:call-template>
    </xsl:variable>
    <xsl:variable name="rel" select="/domain/inheritance[name=$rel-name]"/>

    <xsl:call-template name="op:subprogram-specification">
      <xsl:with-param name="indent" select="$I"/>
    </xsl:call-template>
    <xsl:text> is&#10;</xsl:text>
    <xsl:value-of select="$I"/>
    <xsl:text>begin&#10;</xsl:text>

    <xsl:value-of select="$II"/>
    <xsl:if test="$op/@return">
      <xsl:text>return </xsl:text>
    </xsl:if>

    <xsl:value-of select="$rel/parent"/>
    <xsl:text>.</xsl:text>
    <xsl:value-of select="$op/name"/>
    <xsl:text>&#10;</xsl:text>

    <xsl:value-of select="$IIC"/>
    <xsl:text>(This =&gt; </xsl:text>
    <xsl:value-of select="$rel/parent"/>
    <xsl:text>.Handle (Get_</xsl:text>
    <xsl:value-of select="$rel/name"/>
    <xsl:text>_Parent (This))</xsl:text>

    <xsl:for-each select="$op/parameter">
      <xsl:text>,&#10; </xsl:text>
      <xsl:value-of select="$IIC"/>
      <xsl:value-of select="name"/>
      <xsl:text> =&gt; </xsl:text>

        <xsl:choose>

          <!-- Test for covariance.
               If the parameter is of the type of the class in which the
               operation is defined, convert to the parent's type.
               XXX this may not always be the right thing. -->

          <xsl:when test="type=$op/../name">
            <xsl:value-of select="$rel/parent"/>
            <xsl:text>.Handle (Get_</xsl:text>
            <xsl:value-of select="$rel/name"/>
            <xsl:text>_Parent (</xsl:text>
            <xsl:value-of select="name"/>
            <xsl:text>))</xsl:text>
          </xsl:when>

          <xsl:otherwise>
            <xsl:value-of select="name"/>
          </xsl:otherwise>

        </xsl:choose>

    </xsl:for-each>

    <xsl:text>);&#10;</xsl:text>

    <xsl:value-of select="$I"/>
    <xsl:text>end </xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>;&#10;</xsl:text>
    <xsl:value-of select="$blank-line"/>

  </xsl:template>


  <!-- Called at class/operation to determine the relationship through
       which an operation is inherited. I had hoped to return the
       relationship, but can only manage to return a string. -->
  <xsl:template name="op:find-implementing-relationship">

    <!-- The potential parents. -->
    <xsl:param name="parents"/>

    <!-- The child class we're looking for. -->
    <xsl:param name="child"/>

    <xsl:choose>

      <xsl:when test="/domain/inheritance[child=$child and parent=$parents]">
        <xsl:value-of select="/domain/inheritance
                              [child=$child and parent=$parents]/name"/>
      </xsl:when>

      <xsl:otherwise>
        <xsl:call-template name="op:find-implementing-relationship">
          <xsl:with-param name="child" select="$child"/>
          <xsl:with-param
            name="parents"
            select="/domain/inheritance[parent=$parents]/child"/>
        </xsl:call-template>
      </xsl:otherwise>

    </xsl:choose>

  </xsl:template>


  <!-- Called at class/operation to generate a body. -->
  <xsl:template name="op:generate-accessor-body">

    <!-- The current class (not necessarily the one where the operation
         is defined, if we're talking inheritance) -->
    <xsl:param name="current"/>

    <xsl:variable name="att-to-set"
      select="$current/attribute[concat('Set_',name)=current()/name]"/>
    <xsl:variable name="att-to-get"
      select="$current/attribute[concat('Get_',name)=current()/name]"/>

    <xsl:call-template name="op:subprogram-specification">
      <xsl:with-param name="indent" select="$I"/>
    </xsl:call-template>
    <xsl:text> is&#10;</xsl:text>

    <xsl:choose>

      <!-- Check for Set accessor. -->
      <xsl:when test="$generate-accessors='defined'
                      and not(@return) and count(parameter)=1
                      and $att-to-set/type=parameter/type
                      and (($att-to-set/@class and @class)
                      or (not($att-to-set/@class) and not(@class)))">
        <xsl:value-of select="$I"/>
        <xsl:text>begin&#10;</xsl:text>
        <xsl:value-of select="$II"/>
        <xsl:if test="not(@class)">
          <xsl:text>This.</xsl:text>
        </xsl:if>
        <xsl:value-of select="$att-to-set/name"/>
        <xsl:text> := </xsl:text>
        <xsl:value-of select="parameter/name"/>
        <xsl:text>;&#10;</xsl:text>
      </xsl:when>

      <!-- Check for Get accessor. -->
      <xsl:when test="$generate-accessors='defined'
                      and @return and not(parameter)
                      and $att-to-get/type=@return
                      and (($att-to-get/@class and @class)
                      or (not($att-to-get/@class) and not(@class)))">
        <xsl:value-of select="$I"/>
        <xsl:text>begin&#10;</xsl:text>
        <xsl:value-of select="$II"/>
        <xsl:text>return </xsl:text>
        <xsl:if test="not(@class)">
          <xsl:text>This.</xsl:text>
        </xsl:if>
        <xsl:value-of select="$att-to-get/name"/>
        <xsl:text>;&#10;</xsl:text>
      </xsl:when>

      <!-- If neither, it's an error. -->
      <xsl:otherwise>
        <xsl:call-template name="ut:log-error"/>
        <xsl:message>
          <xsl:text>Error: </xsl:text>
          <xsl:value-of select="../name"/>.<xsl:value-of select="name"/>
          <xsl:text> is wrongly stereotyped as an accessor</xsl:text>
        </xsl:message>
      </xsl:otherwise>

    </xsl:choose>

    <xsl:value-of select="$I"/>
    <xsl:text>end </xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>;&#10;</xsl:text>
    <xsl:value-of select="$blank-line"/>

  </xsl:template>


  <!-- Called at class/operation to generate a body. -->
  <xsl:template name="op:generate-body-with-body-text">

    <!-- The current class (not necessarily the one where the operation
         is defined, if we're talking inheritance) -->
    <xsl:param name="current"/>

    <xsl:call-template name="op:subprogram-specification">
      <xsl:with-param name="indent" select="$I"/>
    </xsl:call-template>
    <xsl:text> is&#10;</xsl:text>
    <xsl:value-of select="$I"/>
    <xsl:text>begin&#10;</xsl:text>

    <xsl:for-each select="body/line">
      <xsl:value-of select="$II"/>
      <xsl:value-of select="."/>
      <xsl:text>&#10;</xsl:text>
    </xsl:for-each>

    <xsl:value-of select="$I"/>
    <xsl:text>end </xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>;&#10;</xsl:text>
    <xsl:value-of select="$blank-line"/>

  </xsl:template>


  <!-- Called at class/operation to generate a separate body. -->
  <xsl:template name="op:generate-separate-body">

    <!-- The current class (not necessarily the one where the operation
         is defined, if we're talking inheritance) -->
    <xsl:param name="current"/>

    <xsl:variable name="att-to-set"
      select="$current/attribute[concat('Set_',name)=current()/name]"/>
    <xsl:variable name="att-to-get"
      select="$current/attribute[concat('Get_',name)=current()/name]"/>

    <!-- The "edit/don't" comments depend on the circumstances, so calculate
         the heading here for reuse. -->
    <xsl:variable name="heading">
      <xsl:text>separate (</xsl:text>
      <xsl:value-of select="../../name"/>
      <xsl:text>.</xsl:text>
      <xsl:value-of select="$current/name"/>
      <xsl:text>)&#10;</xsl:text>
      <xsl:call-template name="op:subprogram-specification"/>
      <xsl:text> is&#10;</xsl:text>
      <xsl:if test="../@public">
        <!-- Check on public class operations that the domain has been
             initialized; hopefully the implementer will retain this. -->
        <xsl:value-of select="$I"/>
        <xsl:text>pragma Assert&#10;</xsl:text>
        <xsl:value-of select="$IC"/>
        <xsl:text>(Domain_Initialized,&#10;</xsl:text>
        <xsl:value-of select="$IC"/>
        <xsl:text> "</xsl:text>
       <xsl:value-of select="../../name"/>
        <xsl:text> not initialized");&#10;</xsl:text>
       </xsl:if>
    </xsl:variable>

    <xsl:choose>

      <!-- Check for non-accessor auto-generated Set operations -->
      <xsl:when test="$generate-accessors='defined'
                      and not(@return) and count(parameter)=1
                      and $att-to-set/type=parameter/type
                      and (($att-to-set/@class and @class)
                      or (not($att-to-set/@class) and not(@class)))">
        <xsl:call-template name="ut:should-not-edit"/>
        <xsl:call-template name="ut:identification-info"/>

        <xsl:value-of select="$blank-line"/>
        <xsl:call-template name="ut:commentary">
          <xsl:with-param name="indent" select="''"/>
          <xsl:with-param name="separate-pars" select="$blank-line"/>
        </xsl:call-template>

        <xsl:value-of select="$heading"/>
        <xsl:text>begin&#10;</xsl:text>
        <xsl:value-of select="$I"/>
        <xsl:if test="not(@class)">
          <xsl:text>This.</xsl:text>
        </xsl:if>
        <xsl:value-of select="$att-to-set/name"/>
        <xsl:text> := </xsl:text>
        <xsl:value-of select="parameter/name"/>
        <xsl:text>;&#10;</xsl:text>
      </xsl:when>

      <!-- Check for non-accessor auto-generated Get operations -->
      <xsl:when test="$generate-accessors='defined'
                      and @return and not(parameter)
                      and $att-to-get/type=@return
                      and (($att-to-get/@class and @class)
                      or (not($att-to-get/@class) and not(@class)))">
        <xsl:call-template name="ut:should-not-edit"/>
        <xsl:call-template name="ut:identification-info"/>

        <xsl:value-of select="$blank-line"/>
        <xsl:call-template name="ut:commentary">
          <xsl:with-param name="indent" select="''"/>
          <xsl:with-param name="separate-pars" select="$blank-line"/>
        </xsl:call-template>

        <xsl:value-of select="$heading"/>
        <xsl:text>begin&#10;</xsl:text>
        <xsl:value-of select="$I"/>
        <xsl:text>return </xsl:text>
        <xsl:if test="not(@class)">
          <xsl:text>This.</xsl:text>
        </xsl:if>
        <xsl:value-of select="$att-to-get/name"/>
        <xsl:text>;&#10;</xsl:text>
      </xsl:when>

      <xsl:when test="$generate-stubs='yes'">

        <xsl:variable name="subprogram-name">
          <xsl:value-of select="../../name"/>
          <xsl:text>.</xsl:text>
          <xsl:value-of select="../name"/>
          <xsl:text>.</xsl:text>
          <xsl:value-of select="name"/>
        </xsl:variable>

        <xsl:call-template name="ut:should-not-edit"/>
        <xsl:text>pragma Style_Checks (Off);&#10;</xsl:text>
        <xsl:call-template name="ut:identification-info"/>

        <xsl:value-of select="$blank-line"/>
        <xsl:call-template name="ut:commentary">
          <xsl:with-param name="indent" select="''"/>
          <xsl:with-param name="separate-pars" select="$blank-line"/>
        </xsl:call-template>

        <xsl:value-of select="$heading"/>

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

      </xsl:when>

      <!-- If it's a function, we have to supply a return statement
           after raising the exception for it to compile. -->
      <xsl:when test="@return">

        <xsl:call-template name="ut:should-edit"/>
        <xsl:value-of select="$blank-line"/>

        <xsl:call-template name="ut:commentary">
          <xsl:with-param name="indent" select="''"/>
          <xsl:with-param name="separate-pars" select="$blank-line"/>
        </xsl:call-template>

        <xsl:value-of select="$heading"/>

        <!-- We have to organize a return value. Need XSL version 1.1. -->
        <xsl:variable name="return-info">
          <xsl:element name="return-type">
            <xsl:call-template name="ut:type-name">
              <xsl:with-param name="type" select="@return"/>
              <xsl:with-param name="class" select=".."/>
              <xsl:with-param name="is-class" select="'yes'"/>
            </xsl:call-template>
          </xsl:element>
          <xsl:element name="return-value">
            <xsl:call-template name="ut:dummy-return-value">
              <xsl:with-param name="type" select="@return"/>
            </xsl:call-template>
          </xsl:element>
        </xsl:variable>

        <xsl:if test="$return-info/return-value=''">
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

      </xsl:when>

      <!-- .. and this is for procedures. -->
      <xsl:otherwise>
        <xsl:call-template name="ut:should-edit"/>
        <xsl:value-of select="$blank-line"/>

        <xsl:call-template name="ut:commentary">
          <xsl:with-param name="indent" select="''"/>
          <xsl:with-param name="separate-pars" select="$blank-line"/>
        </xsl:call-template>

        <xsl:value-of select="$heading"/>
        <xsl:value-of select="$I"/>
        <xsl:text>Unimplemented : exception;&#10;</xsl:text>
        <xsl:text>begin&#10;</xsl:text>
        <xsl:value-of select="$I"/>
        <xsl:text>raise Unimplemented;&#10;</xsl:text>
      </xsl:otherwise>

    </xsl:choose>

    <xsl:text>end </xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>;&#10;</xsl:text>

  </xsl:template>


  <!-- Called to generate access-to-subprogram types. -->
  <xsl:template
    match="operation[@access]"
    mode="op:access-to-operation">
    <xsl:param name="is-class" select="'yes'"/>
    <xsl:value-of select="$I"/>
    <xsl:text>type </xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text> is access </xsl:text>
    <xsl:choose>
      <xsl:when test="@return">
        <xsl:text>function</xsl:text>
        <xsl:call-template name="op:parameter-list">
          <xsl:with-param name="indent" select="$IC"/>
          <xsl:with-param name="is-class" select="$is-class"/>
        </xsl:call-template>
        <xsl:text>&#10;</xsl:text>
        <xsl:value-of select="$II"/>
        <xsl:text>return </xsl:text>
        <xsl:call-template name="ut:type-name">
          <xsl:with-param name="type" select="@return"/>
          <xsl:with-param name="class" select=".."/>
          <xsl:with-param name="is-class" select="$is-class"/>
        </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>procedure</xsl:text>
        <xsl:call-template name="op:parameter-list">
          <xsl:with-param name="indent" select="$IC"/>
          <xsl:with-param name="is-class" select="$is-class"/>
        </xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:text>;&#10;</xsl:text>
    <xsl:call-template name="ut:commentary">
      <xsl:with-param name="indent" select="$I"/>
    </xsl:call-template>
    <xsl:value-of select="$blank-line"/>
  </xsl:template>

  <xsl:template match="*" mode="op:access-to-operation"/>


  <!-- Called at type/operation or class/operation to register it for
       test stubbing. -->
  <xsl:template name="op:register-operation-stub">

    <!-- The name of the operation. -->
    <xsl:param name="subprogram-name"/>

    <xsl:value-of select="$I"/>
    <xsl:text>ColdFrame.Stubs.Register_Subprogram&#10;</xsl:text>
    <xsl:value-of select="$IC"/>
    <xsl:text>("</xsl:text>
    <xsl:value-of select="$subprogram-name"/>
    <xsl:text>");&#10;</xsl:text>
    <xsl:for-each select="parameter
                          [not(@mode) or @mode='in' or @mode='inout']">
      <xsl:value-of select="$I"/>
      <xsl:text>ColdFrame.Stubs.Register_Input_Parameter&#10;</xsl:text>
      <xsl:value-of select="$IC"/>
      <xsl:text>("</xsl:text>
      <xsl:value-of select="$subprogram-name"/>
      <xsl:text>",&#10;</xsl:text>
      <xsl:value-of select="$IC"/>
      <xsl:text> "</xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text>");&#10;</xsl:text>
    </xsl:for-each>
    <xsl:for-each select="parameter[@mode='out' or @mode='inout']">
      <xsl:value-of select="$I"/>
      <xsl:text>ColdFrame.Stubs.Register_Output_Parameter&#10;</xsl:text>
      <xsl:value-of select="$IC"/>
      <xsl:text>("</xsl:text>
      <xsl:value-of select="$subprogram-name"/>
      <xsl:text>",&#10;</xsl:text>
      <xsl:value-of select="$IC"/>
      <xsl:text> "</xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text>");&#10;</xsl:text>
    </xsl:for-each>
    <xsl:if test="@return">
      <xsl:value-of select="$I"/>
      <xsl:text>ColdFrame.Stubs.Register_Output_Parameter&#10;</xsl:text>
      <xsl:value-of select="$IC"/>
      <xsl:text>("</xsl:text>
      <xsl:value-of select="$subprogram-name"/>
      <xsl:text>",&#10;</xsl:text>
      <xsl:value-of select="$IC"/>
      <xsl:text> "return");&#10;</xsl:text>
    </xsl:if>
    <xsl:value-of select="$blank-line"/>

  </xsl:template>


</xsl:stylesheet>
