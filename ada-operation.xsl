<!-- $Id: ada-operation.xsl,v 76bb92682ad8 2001/06/26 18:46:59 simon $ -->
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


  <!-- called at domain/class to generate subprogram specs for a class.
       Since this may be a child type, we handle all the operations
       in the ancestor tree. -->
  <xsl:template name="operation-specs">

    <!-- The classes to be processed this time. The default is the
         current class. -->
    <xsl:param name="parents" select="."/>

    <!-- The operations so far. The default value is "null". -->
    <xsl:param name="operations" select="/.."/>

    <xsl:choose>

      <xsl:when test="$parents">

        <!-- Still something to collect; call self recursively with the
             parent node(s). -->
        <xsl:call-template name="operation-specs">
          <xsl:with-param
            name="parents"
            select="../class[name=../inheritance[child=$parents/name]/parent]"/>
          <xsl:with-param
            name="operations"
            select="$parents/operation[not(name=$operations/name)]
                    | $operations"/>
        </xsl:call-template>

      </xsl:when>

      <xsl:otherwise>

        <!-- $operations contains all the nodes to be processed. -->

        <xsl:apply-templates select="$operations" mode="operation-spec">
          <xsl:with-param name="current" select="."/>
        </xsl:apply-templates>

      </xsl:otherwise>

    </xsl:choose>

  </xsl:template>


  <!-- Generate subprogram specs (but not parental <<class>> operations,
       or access-to-operations). -->
  <xsl:template match="class/operation[not(@access)]" mode="operation-spec">

    <!-- The current class. -->
    <xsl:param name="current"/>

    <xsl:if test="..=$current or not(@class)">
      <xsl:call-template name="subprogram-specification">
        <xsl:with-param name="indent" select="'  '"/>
      </xsl:call-template>
      <xsl:text>;&#10;</xsl:text>
    </xsl:if>

  </xsl:template>

  <xsl:template mode="operation-spec" match="*"/>


  <!-- Generate subprogram context for specs. -->
  <!-- XXX at present, doesn't ensure there's only one of each -->
  <xsl:template match="class/operation" mode="operation-spec-context">

    <!-- The current class. -->
    <xsl:param name="current"/>

    <!-- Find the names of all the types involved -->
    <xsl:for-each select="parameter/type | @return">

      <!-- .. sorted, so we can uniqueify them (when I've worked
           out how) .. -->
      <xsl:sort select="."/>

      <!-- .. only using those whose names are those of classes in
           the domain (and not the current class) .. -->

      <xsl:if test="/domain/class/name=. and not(.=$current/name)">

        <xsl:choose>

          <!-- It seems (26.vi.01) that GNAT 3.14a1 has a problem with
               "with type" and tasks. -->
          <xsl:when test="$current/@active">
            <xsl:text>with </xsl:text>
            <xsl:value-of select="/domain/name"/>
            <xsl:text>.</xsl:text>
            <xsl:value-of select="."/>
            <xsl:text>;&#10;</xsl:text>
          </xsl:when>

          <!-- Normally, use "with type" to minimise risk of circularities -->
          <xsl:otherwise>
           <xsl:text>with type </xsl:text>
            <xsl:value-of select="/domain/name"/>
            <xsl:text>.</xsl:text>
            <xsl:value-of select="."/>
            <xsl:text>.Handle is access;&#10;</xsl:text>
          </xsl:otherwise>

        </xsl:choose>

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

  <xsl:template mode="operation-spec-context" match="*"/>


  <!-- Generate subprogram context for bodies. -->
  <!-- XXX at present, doesn't ensure there's only one of each -->
  <xsl:template match="class/operation" mode="operation-body-context">

    <!-- The current class. -->
    <xsl:param name="current"/>

    <!-- Find the names of all the types involved -->
    <xsl:for-each select="parameter/type | @return">

      <!-- .. sorted, so we can uniqueify them (when I've worked
           out how) .. -->
      <xsl:sort select="."/>

      <!-- .. only using those whose names are those of classes in
           the domain (and not the current class) .. -->

      <xsl:if test="/domain/class/name=. and not(.=$current/name)">

        <xsl:text>with </xsl:text>
        <xsl:value-of select="/domain/name"/>
        <xsl:text>.</xsl:text>
        <xsl:value-of select="."/>
        <xsl:text>;&#10;</xsl:text>
        
      </xsl:if>

    </xsl:for-each>
  </xsl:template>

  <xsl:template mode="operation-body-context" match="*"/>


  <!-- called at domain/class to generate subprogram stubs for a class.
       Since this may be a child type, we handle all the operations
       in the ancestor tree. -->
  <xsl:template name="operation-body-stubs">

    <!-- The classes to be processed this time. The default is the
         current class. -->
    <xsl:param name="parents" select="."/>

    <!-- The operations so far. The default value is "null". -->
    <xsl:param name="operations" select="/.."/>

    <xsl:choose>

      <xsl:when test="$parents">

        <!-- Still something to collect; call self recursively with the
             parent node(s). -->
        <xsl:call-template name="operation-body-stubs">
          <xsl:with-param
            name="parents"
            select="../class[name=../inheritance[child=$parents/name]/parent]"/>
          <xsl:with-param
            name="operations"
            select="$parents/operation[not(name=$operations/name)]
                    | $operations"/>
        </xsl:call-template>

      </xsl:when>

      <xsl:otherwise>

        <!-- $operations contains all the nodes to be processed. -->

        <xsl:apply-templates select="$operations" mode="operation-body-stub">
          <xsl:with-param name="current" select="."/>
        </xsl:apply-templates>

      </xsl:otherwise>

    </xsl:choose>

  </xsl:template>


  <!-- Generate the body stubs of operations (but not parental <<class>
       operations, or access-to-operations, which are realized in the
       Class package).
       The bodies are compilable but generate Program_Error if called. -->
  <xsl:template
    match="class/operation[not(@access)]"
    mode="operation-body-stub">

    <!-- The current class. -->
    <xsl:param name="current"/>

    <xsl:if test="..=$current or not(@class)">
      <xsl:call-template name="subprogram-specification">
        <xsl:with-param name="indent" select="'  '"/>
      </xsl:call-template>
      <xsl:text> is separate;&#10;</xsl:text>
    </xsl:if>

  </xsl:template>

  <xsl:template mode="operation-body-stub" match="*"/>


  <!-- called at domain/class to generate subprogram bodies for a class.
       Since this may be a child type, we handle all the operations
       in the ancestor tree. -->
  <xsl:template name="operation-bodies">

    <!-- The classes to be processed this time. The default is the
         current class. -->
    <xsl:param name="parents" select="."/>

    <!-- The operations so far. The default value is "null". -->
    <xsl:param name="operations" select="/.."/>

    <xsl:choose>

      <xsl:when test="$parents">

        <!-- Still something to collect; call self recursively with the
             parent node(s). -->
        <xsl:call-template name="operation-bodies">
          <xsl:with-param
            name="parents"
            select="../class[name=../inheritance[child=$parents/name]/parent]"/>
          <xsl:with-param
            name="operations"
            select="$parents/operation[not(name=$operations/name)]
                    | $operations"/>
        </xsl:call-template>

      </xsl:when>

      <xsl:otherwise>

        <!-- $operations contains all the nodes to be processed. -->

        <xsl:apply-templates select="$operations" mode="operation-body">
          <xsl:with-param name="current" select="."/>
        </xsl:apply-templates>

      </xsl:otherwise>

    </xsl:choose>

  </xsl:template>


  <!-- Generate the separate bodies of potentially dispatching operations
       (but not access-to-operations, which are realized in the Class package,
       or procedure operations of active classes, which generate a
       call to the corresponding entry).
       The bodies are compilable but concrete ones generate Program_Error
       if called. -->
  <xsl:template
    mode="operation-body"
    match="class/operation[not(@access)]">

    <!-- The current class (not necessarily the one where the operation
         is defined, if we're talking inheritance) -->
    <xsl:param name="current"/>

    <!-- <xsl:message>
      <xsl:value-of select="$current/name"/>.<xsl:value-of select="name"/>
    </xsl:message> -->

    <xsl:choose>

      <xsl:when test="$current=..">

        <!-- The operation is declared in the current class. -->

        <xsl:choose>

          <xsl:when test="@abstract">

            <!-- Abstract in current class; we need to dispatch downward. -->

            <xsl:call-template name="generate-dispatch-to-child">
              <xsl:with-param name="current" select="$current"/>
            </xsl:call-template>

          </xsl:when>

          <xsl:when test="../@active and not(@return)">

            <!-- Concrete task entry in current class; we provide an
                 implementation that calls the entry. -->

            <xsl:call-template name="generate-entry-call">
              <xsl:with-param name="current" select="$current"/>
            </xsl:call-template>

          </xsl:when>

          <xsl:otherwise>

            <!-- Concrete, non-task-entry in current class; we provide a stub
                 implementation. -->

            <xsl:call-template name="generate-body">
              <xsl:with-param name="current" select="$current"/>
            </xsl:call-template>

          </xsl:otherwise>

        </xsl:choose>

      </xsl:when>

      <xsl:otherwise>

        <xsl:choose>

          <xsl:when test="@abstract">

            <!-- Abstract in ancestor class; we need to dispatch
                 downward (if there's anywhere to go). -->

            <xsl:if test="not(/domain/inheritance[parent=$current/name])">
              <xsl:message>
                <xsl:text>CF: no concrete operation for </xsl:text>
                <xsl:value-of select="../name"/>.<xsl:value-of select="name"/>
                <xsl:text> in </xsl:text>
                <xsl:value-of select="$current/name"/>
              </xsl:message>
            </xsl:if>

            <xsl:call-template name="generate-dispatch-to-child">
              <xsl:with-param name="current" select="$current"/>
            </xsl:call-template>

          </xsl:when>

          <xsl:when test="not(@class)">

            <!-- Concrete, non-<<class>> in ancestor class; we need to call
                 the operation in our parent. -->

            <xsl:call-template name="generate-dispatch-to-parent">
              <xsl:with-param name="current" select="$current"/>
            </xsl:call-template>

          </xsl:when>

        </xsl:choose>

      </xsl:otherwise>

    </xsl:choose>

  </xsl:template>

  <xsl:template mode="operation-body" match="*"/>


  <!-- Called from class/operation to generate a subprogram specification.
       Ends without the closing ";" or " is". -->
  <xsl:template name="subprogram-specification">
    <xsl:param name="indent"/>

    <xsl:choose>

      <!-- If there's a return attribute, it's a function. -->
      <xsl:when test="@return">
        <xsl:value-of select="$indent"/>
        <xsl:text>function </xsl:text>
        <xsl:value-of select="name"/>
        <xsl:call-template name="parameter-list">
          <xsl:with-param name="indent" select="concat($indent,'  ')"/>
        </xsl:call-template>
        <xsl:text>&#10;</xsl:text>
        <xsl:value-of select="$indent"/>
        <xsl:text>   return </xsl:text>
        <xsl:call-template name="type-name">
          <xsl:with-param name="type" select="@return"/>
        </xsl:call-template>
      </xsl:when>

      <!-- If there's no return attribute, it's a procedure. -->
      <xsl:otherwise>
        <xsl:value-of select="$indent"/>
        <xsl:text>procedure </xsl:text>
        <xsl:value-of select="name"/>
        <xsl:call-template name="parameter-list">
          <xsl:with-param name="indent" select="concat($indent,'  ')"/>
        </xsl:call-template>
      </xsl:otherwise>

    </xsl:choose>
  </xsl:template>


  <!-- Called from class/operation to generate a subprogram parameter list -->
  <xsl:template name="parameter-list">
    <xsl:param name="indent" select="''"/>

    <!-- In Ada, an empty parameter list is void (not "()" as in C).
         If the operation has parameters, we clearly need a parameter
         list here! Otherwise, we have to check for a Handle; if
         the Class is a singleton, all operations are class operations,
         otherwise it depends on the @class attribute. -->
    <xsl:if
      test="parameter or (not(../@singleton) and not(@class='yes'))">

      <xsl:text>&#10;</xsl:text>
      <xsl:value-of select="$indent"/>
      <xsl:text>(</xsl:text>
      <xsl:if test="not(../@singleton) and not(@class='yes')">
        <xsl:text>This : Handle</xsl:text>
        <xsl:if test="parameter">
          <xsl:text>;&#10; </xsl:text>
          <xsl:value-of select="$indent"/>
        </xsl:if>
      </xsl:if>
      <xsl:apply-templates mode="parameter">
        <xsl:with-param name="indent" select="$indent"/>
      </xsl:apply-templates>
      <xsl:text>)</xsl:text>

    </xsl:if>

  </xsl:template>


  <!-- Called from class/operation to generate a subprogram parameter -->
  <xsl:template match="operation/parameter" mode="parameter">
    <xsl:param name="indent" select="''"/>

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
      <xsl:value-of select="$indent"/>
      <xsl:text> </xsl:text>
    </xsl:if>

  </xsl:template>

  <xsl:template mode="parameter" match="*"/>


  <!-- Called from class/operation to generate a default value.
       Used for default return value for function bodies,
       defaults in initializers. -->
  <xsl:template name="default-value">
    <xsl:param name="type"/>
    <xsl:choose>

      <!-- Date or Time -->
      <xsl:when test="$type='Date' or $type='Time'">
        <xsl:text>Clock</xsl:text>
      </xsl:when>

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


  <!-- Called at class/operation to generate a dispatch-to-child body -->
  <xsl:template name="generate-dispatch-to-child">

    <!-- The current class (not necessarily the one where the operation
         is defined, if we're talking inheritance) -->
    <xsl:param name="current"/>

    <!-- 
         case Get_{relation}_Child_Class (This) is
           when {child-1}_T =>
             {return} {child-1}.{operation}
               (This => {child-1}.Find (({relation}_Child_Of_{parent-abbreviation} => This)){,
                other-parameter-assignments});
         end case;
         -->
    
    <!-- Save the current operation. -->
    <xsl:variable name="op" select="."/>
    
    <!-- XXX won't work with partitioned inheritance -->
    <xsl:variable
      name="rel"
      select="/domain/inheritance[parent=$current/name]"/>
    
    <xsl:for-each select="$rel/child">
      <xsl:text>with </xsl:text>
      <xsl:value-of select="/domain/name"/>
      <xsl:text>.</xsl:text>
      <xsl:value-of select="."/>
      <xsl:text>;&#10;</xsl:text>
    </xsl:for-each>
    
    <xsl:text>separate (</xsl:text>
    <xsl:value-of select="../../name"/>
    <xsl:text>.</xsl:text>
    <xsl:value-of select="$current/name"/>
    <xsl:text>)&#10;</xsl:text>
    <xsl:call-template name="subprogram-specification"/>
    <xsl:text> is&#10;</xsl:text>
    <xsl:text>begin&#10;</xsl:text>
    
    <!-- XXX won't work for partitioned inheritance -->
    <xsl:text>  case Get_</xsl:text>
    <xsl:value-of select="$rel/name"/>
    <xsl:text>_Child_Class (This) is&#10;</xsl:text>
    
    <xsl:for-each select="$rel/child">
      
      <xsl:text>    when </xsl:text>
      <xsl:value-of select="."/>
      <xsl:text>_T =&gt;&#10;</xsl:text>
      
      <xsl:text>      </xsl:text>
      <xsl:if test="$op/@return">
        <xsl:text>return </xsl:text>
      </xsl:if>
      <xsl:value-of select="."/>.<xsl:value-of select="$op/name"/>
      <xsl:text>&#10;</xsl:text>
      
      <xsl:text>        (This =&gt; </xsl:text>
      <xsl:value-of select="."/>
      <xsl:text>.Find ((</xsl:text>
      <xsl:value-of select="$rel/name"/>
      <xsl:text>_Child_Of_</xsl:text>
      <xsl:value-of select="$current/abbreviation"/>
      <xsl:text> =&gt; This))</xsl:text>
      
      <xsl:for-each select="$op/parameter">
        <xsl:text>,&#10;         </xsl:text>
        <xsl:value-of select="name"/>
        <xsl:text> =&gt; </xsl:text>
        <xsl:value-of select="name"/>
      </xsl:for-each>
      
      <xsl:text>);&#10;</xsl:text>
      
    </xsl:for-each>
    
    <xsl:text>  end case;&#10;</xsl:text>
    
    <xsl:text>end </xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>;&#10;</xsl:text>

  </xsl:template>

  
  <!-- Called at class/operation to generate a dispatch-to-parent body -->
  <xsl:template name="generate-dispatch-to-parent">

    <!-- The current class (not necessarily the one where the operation
         is defined, if we're talking inheritance) -->
    <xsl:param name="current"/>

    <!--          
         {return} {parent}.{operation-name}
           (This => Get_{relation}_Child_Of_{parent-abbreviation} (This){,
         other-parameter-assignments});
         -->

    <!-- Save the current operation. -->
    <xsl:variable name="op" select="."/>
    
    <!-- XXX won't work with multiple inheritance -->
    <xsl:variable
      name="rel"
      select="/domain/inheritance[child=$current/name]"/>
    
    <xsl:text>with </xsl:text>
    <xsl:value-of select="/domain/name"/>
    <xsl:text>.</xsl:text>
    <xsl:value-of select="$rel/parent"/>
    <xsl:text>;&#10;</xsl:text>
    
    <xsl:text>separate (</xsl:text>
    <xsl:value-of select="../../name"/>
    <xsl:text>.</xsl:text>
    <xsl:value-of select="$current/name"/>
    <xsl:text>)&#10;</xsl:text>
    <xsl:call-template name="subprogram-specification"/>
    <xsl:text> is&#10;</xsl:text>
    <xsl:text>begin&#10;</xsl:text>
    
    <xsl:text>  </xsl:text>
    <xsl:if test="$op/@return">
      <xsl:text>return </xsl:text>
    </xsl:if>
    
    <xsl:value-of select="$rel/parent"/>
    <xsl:text>.</xsl:text>
    <xsl:value-of select="$op/name"/>
    <xsl:text>&#10;</xsl:text>
    
    <xsl:text>    (This =&gt; Get_</xsl:text>
    <xsl:value-of select="$rel/name"/>
    <xsl:text>_Child_Of_</xsl:text>
    <xsl:value-of select="/domain/class[name=$rel/parent]/abbreviation"/>
    <xsl:text> (This)</xsl:text>
    
    <xsl:for-each select="$op/parameter">
      <xsl:text>,&#10;     </xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text> =&gt; </xsl:text>
      <xsl:value-of select="name"/>
    </xsl:for-each>
    
    <xsl:text>);&#10;</xsl:text>
    
    <xsl:text>end </xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>;&#10;</xsl:text>

  </xsl:template>

  
  <!-- Called at class/operation to generate a body (which compiles,
       but raises Program_Error). -->
  <xsl:template name="generate-body">

    <!-- The current class (not necessarily the one where the operation
         is defined, if we're talking inheritance) -->
    <xsl:param name="current"/>

    <xsl:text>separate (</xsl:text>
    <xsl:value-of select="../../name"/>
    <xsl:text>.</xsl:text>
    <xsl:value-of select="$current/name"/>
    <xsl:text>)&#10;</xsl:text>
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
    <xsl:value-of select="name"/>
    <xsl:text>;&#10;</xsl:text>
    
  </xsl:template>
  

  <!-- Called at class/operation to generate a body which calls the matching
       task entry. -->
  <xsl:template name="generate-entry-call">

    <!-- The current class (not necessarily the one where the operation
         is defined, if we're talking inheritance) -->
    <xsl:param name="current"/>

    <xsl:text>separate (</xsl:text>
    <xsl:value-of select="../../name"/>
    <xsl:text>.</xsl:text>
    <xsl:value-of select="$current/name"/>
    <xsl:text>)&#10;</xsl:text>
    <xsl:call-template name="subprogram-specification"/>
    <xsl:text> is&#10;</xsl:text>
    <xsl:text>begin&#10;</xsl:text>
    
    <xsl:text>  This.The_T.</xsl:text>
    <xsl:value-of select="name"/>
    
    <xsl:if test="parameter">
      
      <xsl:text>&#10;    (</xsl:text>
      
      <xsl:for-each select="parameter">
        <xsl:value-of select="name"/>
        <xsl:text> =&gt; </xsl:text>
        <xsl:value-of select="name"/>
        <xsl:if test="position() &lt; last()">
          <xsl:text>,&#10;     </xsl:text>
        </xsl:if>
      </xsl:for-each>
      
      <xsl:text>)</xsl:text>
      
    </xsl:if>
    
    <xsl:text>;&#10;</xsl:text>
    
    <xsl:text>end </xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>;&#10;</xsl:text>

  </xsl:template>

</xsl:stylesheet>
