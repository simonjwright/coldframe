<!-- $Id: ada-operation.xsl,v d03dfef789ea 2001/06/20 19:15:10 simon $ -->
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

        <xsl:apply-templates select="$operations" mode="operation-spec"/>

      </xsl:otherwise>

    </xsl:choose>

  </xsl:template>


  <!-- Generate subprogram specs (but not access-to-operations). -->
  <xsl:template match="class/operation[not(@access)]" mode="operation-spec">
    <xsl:call-template name="subprogram-specification">
      <xsl:with-param name="indent" select="'  '"/>
    </xsl:call-template>
    <xsl:text>;&#10;</xsl:text>
  </xsl:template>

  <xsl:template mode="operation-spec" match="*"/>


  <!-- Generate subprogram context. -->
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

        <xsl:apply-templates select="$operations" mode="operation-body-stub"/>

      </xsl:otherwise>

    </xsl:choose>

  </xsl:template>


  <!-- Generate the body stubs of operations (but not
       access-to-operations, which are realized in the Class package).
       The bodies are compilable but generate Program_Error if called. -->
  <xsl:template
    match="class/operation[not(@access)]"
    mode="operation-body-stub">

    <xsl:call-template name="subprogram-specification">
      <xsl:with-param name="indent" select="'  '"/>
    </xsl:call-template>
    <xsl:text> is separate;&#10;</xsl:text>

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


  <!-- Generate the separate bodies of <<class>> operations (but not
       access-to-operations, which are realized in the Class package,
       or procedure operations of active classes, which may generate a
       call to the corresponding entry).
       The bodies are compilable but generate Program_Error if called. -->
  <xsl:template
    mode="operation-body"
    match="class[@singleton or not(@active)]/operation[@class and not(@access)]
           | class[@active]/operation[(@return or @class) and not(@access)]">

    <xsl:text>separate (</xsl:text>
    <xsl:value-of select="../../name"/>
    <xsl:text>.</xsl:text>
    <xsl:value-of select="../name"/>
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


  <!-- Generate the separate bodies of potentially dispatching operations
       (but not access-to-operations, which are realized in the Class package,
       or procedure operations of active classes, which generate a
       call to the corresponding entry).
       The bodies are compilable but concrete ones generate Program_Error
       if called. -->
  <xsl:template
    mode="operation-body"
    match="class[not(@active)]/operation[not(@access)]
           | class[@active]/operation[@return and not(@access)]">

    <!-- The current node (not necessarily the one where the operation
         is defined, if we're talking inheritance) -->
    <xsl:param name="current"/>

    <xsl:message>
      <xsl:value-of select="$current/name"/>.<xsl:value-of select="name"/>
    </xsl:message>

    <xsl:text>separate (</xsl:text>
    <xsl:value-of select="../../name"/>
    <xsl:text>.</xsl:text>
    <xsl:value-of select="$current/name"/>
    <xsl:text>)&#10;</xsl:text>
    <xsl:call-template name="subprogram-specification"/>
    <xsl:text> is&#10;</xsl:text>
    <xsl:text>begin&#10;</xsl:text>

    <xsl:choose>

      <xsl:when test="$current=..">

        <!-- The operation is declared in the current class. -->
        <xsl:text>  -- current class --&#10;</xsl:text>

        <xsl:choose>

          <xsl:when test="@abstract">

            <!-- We need to dispatch downward. -->
            <xsl:text>  -- abstract --&#10;</xsl:text>

            <!--
                 case Get_{relation}_Child_Class (This) is
                   when {child-1}_T =>
                     {return} {child-1}.{operation}
                       (This => {child-1}.Find (This){,
                        other-parameter-assignments});
                 end case;
                 -->

          </xsl:when>

          <xsl:otherwise>

            <!-- We provide a stub implementation. -->
            <xsl:text>  -- concrete --&#10;</xsl:text>

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
            
          </xsl:otherwise>

        </xsl:choose>

      </xsl:when>

      <xsl:otherwise>

        <!-- The operation is declared in an ancestor class. -->
        <xsl:text>  -- ancestor class --&#10;</xsl:text>

        <xsl:choose>

          <xsl:when test="@abstract">

            <!-- We need to dispatch downward (if there's anywhere
                 to go). -->

            <xsl:if test="not(/domain/inheritance[parent=$current/name])">
              <xsl:message>
                <xsl:text>CF: no concrete operation for </xsl:text>
                <xsl:value-of select="../name"/>.<xsl:value-of select="name"/>
              </xsl:message>
            </xsl:if>

            <xsl:text>  -- abstract --&#10;</xsl:text>

            <!--
                 case Get_{relation}_Child_Class (This) is
                   when {child-1}_T =>
                     {return} {child-1}.{operation}
                       (This => {child-1}.Find (This){,
                        other-parameter-assignments});
                 end case;
                 -->

          </xsl:when>

          <xsl:otherwise>

            <!-- We need to call the operation in our parent. -->
            <xsl:text>  -- concrete --&#10;</xsl:text>

            <!--
                 {return} {parent}.{operation-name}
                   (This => Get_{relation}_Child_Of_{parent-abbreviation}
                              (This){,
                    other-parameter-assignments});
                 -->

          </xsl:otherwise>

        </xsl:choose>

      </xsl:otherwise>

    </xsl:choose>

    <xsl:text>end </xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>;&#10;</xsl:text>

  </xsl:template>


  <!-- Generate the potentially dispatching separate bodies of procedure
       operations of active classes; concrete procedures make a call to
       the corresponding entry). -->
  <xsl:template
    mode="operation-body"
    match="class[@active]/operation[not(@return) and not(@access)]">

    <!-- The current node (not necessarily the one where the operation
         is defined, if we're talking inheritance) -->
    <xsl:param name="current"/>

    <xsl:message>
      <xsl:text>Active: </xsl:text>
      <xsl:value-of select="$current/name"/>.<xsl:value-of select="name"/>
    </xsl:message>

    <xsl:text>separate (</xsl:text>
    <xsl:value-of select="../../name"/>
    <xsl:text>.</xsl:text>
    <xsl:value-of select="$current/name"/>
    <xsl:text>)&#10;</xsl:text>
    <xsl:call-template name="subprogram-specification"/>
    <xsl:text> is&#10;</xsl:text>
    <xsl:text>begin&#10;</xsl:text>

    <xsl:choose>

      <xsl:when test="$current=..">

        <!-- The operation is declared in the current class. -->
        <xsl:text>  -- current class --&#10;</xsl:text>

        <xsl:choose>

          <xsl:when test="@abstract">

            <!-- We need to dispatch downward. -->
            <xsl:text>  -- abstract --&#10;</xsl:text>

            <!--
                 case Get_{relation}_Child_Class (This) is
                   when {child-1}_T =>
                     {child-1}.{operation}
                       (This => {child-1}.Find (This){,
                        other-parameter-assignments});
                 end case;
                 -->

          </xsl:when>

          <xsl:otherwise>

            <!-- We provide an implementation that calls our task's entry. -->
            <xsl:text>  -- concrete --&#10;</xsl:text>

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

          </xsl:otherwise>

        </xsl:choose>

      </xsl:when>

      <xsl:otherwise>

        <!-- The operation is declared in an ancestor class. -->
        <xsl:text>  -- ancestor class --&#10;</xsl:text>

        <xsl:choose>

          <xsl:when test="@abstract">

            <!-- We need to dispatch downward (if there's anywhere
                 to go). -->
            <xsl:text>  -- abstract --&#10;</xsl:text>

            <!--
                 case Get_{relation}_Child_Class (This) is
                   when {child-1}_T =>
                     {child-1}.{operation}
                       (This => {child-1}.Find (This){,
                        other-parameter-assignments});
                 end case;
                 -->

          </xsl:when>

          <xsl:otherwise>

            <!-- We need to call the operation in our parent. -->
            <xsl:text>  -- concrete --&#10;</xsl:text>

            <!--
                 {parent}.{operation-name}
                   (This => Get_{relation}_Child_Of_{parent-abbreviation}
                              (This){,
                    other-parameter-assignments});
                 -->

          </xsl:otherwise>

        </xsl:choose>

      </xsl:otherwise>

    </xsl:choose>

    <xsl:text>end </xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>;&#10;</xsl:text>

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


</xsl:stylesheet>
