<!-- $Id: ada-class.xsl,v bf3f9baf832f 2001/09/14 19:37:58 simon $ -->
<!-- XSL stylesheet to generate Ada code for Classes. -->
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


  <!-- Generate the class packages (specs). -->
  <xsl:template match="domain/class" mode="class-spec">

    <xsl:message>
      <xsl:text>  .. </xsl:text>
      <xsl:value-of select="name"/>
    </xsl:message>

    <!-- Any context clauses needed for the class package .. -->
    <xsl:call-template name="class-spec-context"/>

    <!-- .. the class package .. -->
    <xsl:if test="not(@public)">
      <!-- only public packages are externally visible -->
      <xsl:text>private </xsl:text>
    </xsl:if>
    <xsl:text>package </xsl:text>
    <xsl:value-of select="../name"/>.<xsl:value-of select="name"/>
    <xsl:text> is&#10;</xsl:text>

    <xsl:choose>

      <xsl:when test="not(@singleton)">

        <!-- .. the Identifier record .. -->
        <xsl:call-template name="identifier-record"/>
        
        <!-- .. the Instance record (indefinite, so it can't be
             allocated; limited, so people can't assign it) .. -->
        <xsl:value-of select="$I"/>
        <xsl:text>type Instance (&lt;&gt;) is limited private;&#10;</xsl:text>
        
        <!-- .. the Handle .. -->
        <xsl:value-of select="$I"/>
        <xsl:text>type Handle is access Instance;&#10;</xsl:text>
        
        <!-- .. the creation, simple find, and deletion operations .. -->
        <xsl:call-template name="create-function-spec"/>
        <xsl:value-of select="$I"/>
        <xsl:text>function Find (With_Identifier : Identifier) return Handle;&#10;</xsl:text>
        <xsl:value-of select="$I"/>
        <xsl:text>procedure Delete (With_Identifier : Identifier);&#10;</xsl:text>
        <xsl:value-of select="$I"/>
        <xsl:text>procedure Delete (This : in out Handle);&#10;</xsl:text>
        
        <!-- .. subtype enumeration support, if required .. -->
        <xsl:call-template name="supertype-specs"/>
        
      </xsl:when>

      <xsl:when test="@singleton and not(@public)">

        <!-- .. the Instance record (indefinite, so it can't be
             allocated; limited, so people can't assign it) .. -->
        <xsl:value-of select="$I"/>
        <xsl:text>type Instance (&lt;&gt;) is limited private;&#10;</xsl:text>
        
        <!-- .. the Handle .. -->
        <xsl:value-of select="$I"/>
        <xsl:text>type Handle is access Instance;&#10;</xsl:text>
        
        <!-- .. the find operation .. -->
        <xsl:value-of select="$I"/>
        <xsl:text>function Find return Handle;&#10;</xsl:text>

      </xsl:when>

    </xsl:choose>

    <!-- .. any access-to-subprogram types (before possible accessors) .. -->
    <xsl:apply-templates mode="access-to-operation"/>

    <!-- .. the attribute access operations .. -->
    <xsl:apply-templates mode="attribute-set-spec"/>
    <xsl:apply-templates mode="attribute-get-spec"/>

    <!-- .. operations .. -->
    <xsl:call-template name="operation-specs"/>

    <!-- .. the private part .. -->
    <xsl:text>private&#10;</xsl:text>
 
    <xsl:if test="@public">
      <xsl:value-of select="$I"/>
      <xsl:text>type Instance;&#10;</xsl:text>
    </xsl:if>

    <xsl:if test="@active">
      <xsl:call-template name="task-spec"/>
    </xsl:if>
    
    <!-- .. the Instance record .. -->
    <xsl:call-template name="instance-record"/>

    <xsl:choose>

      <!-- There can be multiple instances, unless this is a singleton -->
      <xsl:when test="not(@singleton)">
        
        <!-- Use a fixed (effectively, static) storage pool if this is
             a bounded class.
             We have to use the GNAT 'Object_Size because 'Size is the
             minimal number of bits, not necessarily a whole number of
             bytes. -->
        <xsl:if test="@max">
          <xsl:value-of select="$I"/>
          <xsl:text>for Handle'Storage_Size use Instance'Object_Size / 8 * </xsl:text>
          <xsl:value-of select="@max"/>
          <xsl:text>;&#10;</xsl:text>
        </xsl:if>
        
        <!-- .. basic Container instantiation for Maps -->
        <xsl:value-of select="$I"/>
        <xsl:text>package Abstract_Map_Containers is new BC.Containers (Handle);&#10;</xsl:text>
        
        <!-- .. the Hash function spec .. -->
        <xsl:value-of select="$I"/>
        <xsl:text>function Hash (Id : Identifier) return Natural;&#10;</xsl:text>
        
        <!-- .. Container instantiations .. -->
        <xsl:value-of select="$I"/>
        <xsl:text>package Abstract_Maps is new Abstract_Map_Containers.Maps (Identifier);&#10;</xsl:text>
        
        <xsl:choose>
          
          <xsl:when test="@max">
            <!-- Wnen there's a maximum size, use the Bounded version -->
            <xsl:value-of select="$I"/>
            <xsl:text>package Maps is new Abstract_Maps.Bounded&#10;</xsl:text>
            <xsl:value-of select="$IC"/>
            <xsl:text>(Hash =&gt; Hash,&#10;</xsl:text>
            <xsl:value-of select="$IC"/>
            <xsl:text> Buckets =&gt; </xsl:text>
            <xsl:call-template name="hash-buckets"/>
            <xsl:text>,&#10;</xsl:text>
            <xsl:value-of select="$IC"/>
            <xsl:text> Maximum_Size =&gt; </xsl:text>
            <xsl:value-of select="./@max"/>
            <xsl:text>);&#10;</xsl:text>
          </xsl:when>
          
          <xsl:otherwise>
            <!-- Use the Unbounded version -->
            <xsl:value-of select="$I"/>
            <xsl:text>package Maps is new Abstract_Maps.Unbounded&#10;</xsl:text>
            <xsl:value-of select="$IC"/>
            <xsl:text>(Hash =&gt; Hash,&#10;</xsl:text>
            <xsl:value-of select="$IC"/>
            <xsl:text> Buckets =&gt; </xsl:text>
            <xsl:call-template name="hash-buckets"/>
            <xsl:text>,&#10;</xsl:text>
            <xsl:value-of select="$IC"/>
            <xsl:text> Storage_Manager =&gt; ColdFrame.Global_Storage_Pool.Pool_Type,&#10;</xsl:text>
            <xsl:value-of select="$IC"/>
            <xsl:text> Storage =&gt; ColdFrame.Global_Storage_Pool.Pool);&#10;</xsl:text>
          </xsl:otherwise>
          
        </xsl:choose>
        
        <!-- .. the instance container .. -->
        <xsl:value-of select="$I"/>
        <xsl:text>The_Container : Maps.Map;&#10;</xsl:text>
        
      </xsl:when>
      
      <xsl:when test="@singleton">
        <xsl:value-of select="$I"/>
        <xsl:text>This : aliased Instance;&#10;</xsl:text>
      </xsl:when>
      
    </xsl:choose>

    <!-- .. and close. -->
    <xsl:text>end </xsl:text>
    <xsl:value-of select="../name"/>.<xsl:value-of select="name"/>
    <xsl:text>;&#10;</xsl:text>

  </xsl:template>

  <xsl:template mode="class-spec" match="*"/>


  <!-- Called from domain/class to generate context clauses for package
       spec.
       Since this may be a child type, we handle all the operations
       in the ancestor tree as well as the present attributes. -->
  <xsl:template name="class-spec-context">

    <!-- The classes to be processed this time. The default is the
         current class. -->
    <xsl:param name="parents" select="."/>

    <!-- The ancestors so far. The default value is "null". -->
    <xsl:param name="ancestors" select="/.."/>

    <xsl:choose>

      <xsl:when test="$parents">

        <!-- Still something to collect; call self recursively with the
             parent node(s). -->
        <xsl:call-template name="class-spec-context">
          <xsl:with-param
            name="parents"
            select="../class[name=../inheritance[child=$parents/name]/parent]"/>
          <xsl:with-param name="ancestors" select="$parents | $ancestors"/>
        </xsl:call-template>

      </xsl:when>

      <xsl:otherwise>

        <!-- $ancestors contains all the nodes to be processed. -->

        <xsl:if test="attribute[@refers and not(@refers=../name)]">
          <!-- We have an attribute which refers to another class, so
               w're going to use the GNAT "with type" extension. -->
          <!-- XXX what about operations that take parameters of, or
               return, another class? Why bother saying this when we need
               -gnatX anyway? (GNAT 3.14a1) -->
          <!-- <xsl:text>pragma Extensions_Allowed (On);&#10;</xsl:text> -->
        </xsl:if>
        
        <!-- Check for Unbounded_Strings. -->
        <xsl:if test="attribute/type='Unbounded_String'
                      or $ancestors/operation/parameter/type='Unbounded_String'
                      or $ancestors/operation/@return='Unbounded_String'
                      or attribute/type='Text'
                      or $ancestors/operation/parameter/type='Text'
                      or $ancestors/operation/@return='Text'">
          <xsl:text>with Ada.Strings.Unbounded;</xsl:text>
          <xsl:text> use Ada.Strings.Unbounded;&#10;</xsl:text>
        </xsl:if>
        
        <!-- Check for Ada.Calendar. -->
        <xsl:if test="attribute/type='Date'
                      or $ancestors/operation/parameter/type='Date'
                      or $ancestors/operation/@return='Date'
                      or attribute/type='Time'
                      or $ancestors/operation/parameter/type='Time'
                      or $ancestors/operation/@return='Time'">
          <xsl:text>with Ada.Calendar;</xsl:text>
          <xsl:text> use Ada.Calendar;&#10;</xsl:text>
        </xsl:if>
        
        <!-- Choose the appropriate Map (unless this is a singleton). -->
        <xsl:if test="not(@singleton)">
          
          <xsl:choose>
            
            <xsl:when test="./@max">
              <!-- Wnen there's a maximum size, use the Bounded version -->
              <xsl:text>with BC.Containers.Maps.Bounded;&#10;</xsl:text>
            </xsl:when>
            
            <xsl:otherwise>
              <!-- Use the Unbounded version -->
              <!-- We need access to the standard heap storage pool. -->
              <xsl:text>with ColdFrame.Global_Storage_Pool;&#10;</xsl:text>
              <xsl:text>with BC.Containers.Maps.Unbounded;&#10;</xsl:text>
            </xsl:otherwise>
            
          </xsl:choose>
          
        </xsl:if>
        
        <!-- Include necessary "with" or "with type" for referential
             attributes (that refer to other classes). -->
        <!--XXX why doesn't this work always?<xsl:for-each
             select="attribute[@refers
             and not(@refers=preceding::attribute/@refers)]">-->
        <xsl:choose>

          <!-- It seems (26.vi.01) that GNAT 3.14a1 has a problem with
               "with type" and tasks. -->
          <xsl:when test="@active">
            <xsl:for-each
              select="attribute[@refers and not(@refers=../name)]">
              <xsl:sort select="@refers"/>
              <xsl:text>with </xsl:text>
              <xsl:value-of select="../../name"/>
              <xsl:text>.</xsl:text>
              <xsl:value-of select="@refers"/>
              <xsl:text>;&#10;</xsl:text>
            </xsl:for-each>
          </xsl:when>

          <!-- Normally, use "with type" to minimise risk of circularities. -->
          <xsl:otherwise>
            <xsl:for-each
              select="attribute[@refers and not(@refers=../name)]">
              <xsl:sort select="@refers"/>

              <xsl:text>with type </xsl:text>
              <xsl:value-of select="../../name"/>
              <xsl:text>.</xsl:text>
              <xsl:value-of select="@refers"/>
              <xsl:text>.Handle is access;&#10;</xsl:text>

            </xsl:for-each>

          </xsl:otherwise>
        </xsl:choose>
        
        <!-- Handle subprograms. -->
        <xsl:apply-templates
          mode="operation-spec-context"
          select="$ancestors/operation">
          <xsl:with-param name="current" select="."/>
        </xsl:apply-templates>

        <!-- Subtype handles are needed for the subtype selection
             record. -->
        <xsl:call-template name="supertype-spec-context"/>

      </xsl:otherwise>

    </xsl:choose>

  </xsl:template>


  <!-- Called at domain/class to generate any required supertype spec context
       information. -->
  <xsl:template name="supertype-spec-context">
    
    <xsl:variable name="parent-name" select="name"/>

    <xsl:for-each select="../inheritance[parent=$parent-name]/child">
      <xsl:sort select="name"/>

      <!-- XXX may need to take special action not to use "with type" for
           active classes (but only til the GNAT 3.14a1 bug is fixed). -->

      <xsl:text>with type </xsl:text>
      <xsl:value-of select="../../name"/>
      <xsl:text>.</xsl:text>
      <xsl:value-of select="."/>
      <xsl:text>.Handle is access;&#10;</xsl:text>

    </xsl:for-each>

  </xsl:template>

  <!-- Called at domain/class to generate any required supertype body context
       information. -->
  <xsl:template name="supertype-body-context">
    
    <xsl:variable name="parent-name" select="name"/>

    <xsl:for-each select="../inheritance[parent=$parent-name]/child">
      <xsl:sort select="name"/>

      <xsl:text>with </xsl:text>
      <xsl:value-of select="../../name"/>
      <xsl:text>.</xsl:text>
      <xsl:value-of select="."/>
      <xsl:text>;&#10;</xsl:text>

    </xsl:for-each>

  </xsl:template>



  <!-- Called at domain/class to generate any required supertype spec
       information -->
  <xsl:template name="supertype-specs">

    <xsl:variable name="parent-name" select="name"/>

    <xsl:for-each select="../inheritance[parent=$parent-name]">
      <xsl:sort select="name"/>

      <xsl:call-template name="subtype-enumeration"/>
      <xsl:call-template name="subtype-selection"/>

      <xsl:value-of select="$I"/>
      <xsl:text>procedure Set_</xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text>_Child (This : Handle; To_Be : </xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text>_Child);&#10;</xsl:text>
      
      <xsl:value-of select="$I"/>
      <xsl:text>function Get_</xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text>_Child (This : Handle) return </xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text>_Child;&#10;</xsl:text>

    </xsl:for-each>

  </xsl:template>


  <!-- Called at domain/class to generate any required supertype body
       information -->
  <xsl:template name="supertype-bodies">

    <xsl:variable name="parent-name" select="name"/>

    <xsl:for-each select="../inheritance[parent=$parent-name]">
      <xsl:sort select="name"/>

      <xsl:value-of select="$I"/>
      <xsl:text>procedure Set_</xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text>_Child (This : Handle; To_Be : </xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text>_Child) is&#10;</xsl:text>
      <xsl:value-of select="$I"/>
      <xsl:text>begin&#10;</xsl:text>
      <xsl:value-of select="$II"/>
      <xsl:text>This.</xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text>_Current_Child := To_Be;&#10;</xsl:text>
      <xsl:value-of select="$I"/>
      <xsl:text>end Set_</xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text>_Child;&#10;</xsl:text>
      
      <xsl:value-of select="$I"/>
      <xsl:text>function Get_</xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text>_Child (This : Handle) return </xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text>_Child is&#10;</xsl:text>
      <xsl:value-of select="$I"/>
      <xsl:text>begin&#10;</xsl:text>
      <xsl:value-of select="$II"/>
      <xsl:text>return This.</xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text>_Current_Child;&#10;</xsl:text>
      <xsl:value-of select="$I"/>
      <xsl:text>end Get_</xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text>_Child;&#10;</xsl:text>

    </xsl:for-each>

  </xsl:template>


  <!-- Called from domain/inheritance to output the subtype enumeration
       type (sorted), with a "null" value. -->
  <xsl:template name="subtype-enumeration">
    <xsl:value-of select="$I"/>
    <xsl:text>type </xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>_Child_Class is&#10;</xsl:text>
    <xsl:value-of select="$IC"/>
    <xsl:text>(</xsl:text>
    <xsl:for-each select="child">
      <xsl:sort select="."/>
      <xsl:value-of select="."/>
      <xsl:text>_T,&#10;</xsl:text>
      <xsl:value-of select="$IC"/>
      <xsl:text> </xsl:text>
    </xsl:for-each>
    <xsl:text>Null_T);&#10;</xsl:text>
  </xsl:template>


  <!-- Called from domain/inheritance to output the subtype selection
       record. -->
  <xsl:template name="subtype-selection">
    <xsl:value-of select="$I"/>
    <xsl:text>type </xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>_Child (Current : </xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>_Child_Class := Null_T) is record&#10;</xsl:text>
    <xsl:value-of select="$II"/>
    <xsl:text>case Current is&#10;</xsl:text>
    <xsl:for-each select="child">
      <xsl:sort select="."/>
      <xsl:variable name="child" select="."/>
      <xsl:value-of select="$III"/>
      <xsl:text>when </xsl:text>
      <xsl:value-of select="."/>
      <xsl:text>_T =&gt;&#10;</xsl:text>
      <xsl:value-of select="$IIII"/>
      <xsl:value-of select="/domain/class[name=$child]/abbreviation"/>
      <xsl:text> : </xsl:text>
      <xsl:value-of select="."/>
      <xsl:text>.Handle;&#10;</xsl:text>
    </xsl:for-each>
    <xsl:value-of select="$III"/>
    <xsl:text>when Null_T =&gt; null;&#10;</xsl:text>
    <xsl:value-of select="$II"/>
    <xsl:text>end case;&#10;</xsl:text>
    <xsl:value-of select="$I"/>
    <xsl:text>end record;&#10;</xsl:text>
  </xsl:template>


  <!-- Generate the class packages (bodies). -->
  <xsl:template match="domain/class" mode="class-body">

    <xsl:message>
      <xsl:text>  .. </xsl:text>
      <xsl:value-of select="name"/>
    </xsl:message>

    <!-- Any context clauses needed for the class body .. -->
    <xsl:call-template name="class-body-context"/>
    
    <!-- .. start the body .. -->
    <xsl:text>package body </xsl:text>
    <xsl:value-of select="../name"/>.<xsl:value-of select="name"/>
    <xsl:text> is&#10;</xsl:text>
    
    <xsl:if test="@active">
      <xsl:value-of select="$I"/>
      <xsl:text>task body T is separate;&#10;</xsl:text>
    </xsl:if>
    
    <xsl:choose>
      
      <xsl:when test="not(@singleton)">
        
        <!-- .. the creation, simple find, and deletion operations .. -->
        <xsl:call-template name="create-function-body"/>
        <xsl:call-template name="find-function-body"/>
        
        <xsl:value-of select="$I"/>
        <xsl:text>procedure Free is new Ada.Unchecked_Deallocation (Instance, Handle);&#10;</xsl:text>

        <xsl:call-template name="class-delete-procedure-body"/>     
        <xsl:call-template name="delete-procedure-body"/>     
        
        <!-- .. subtype enumeration support, if required .. -->
        <xsl:call-template name="supertype-bodies"/>
        
      </xsl:when>
      
      <xsl:when test="@singleton and not(@public)">
        
        <!-- XXX Uses a GNAT-specific attribute. -->
        <xsl:value-of select="$I"/>
        <xsl:text>function Find return Handle is&#10;</xsl:text>
        <xsl:value-of select="$I"/>
        <xsl:text>begin&#10;</xsl:text>
        <xsl:value-of select="$II"/>
        <xsl:text>return This'Unrestricted_Access;&#10;</xsl:text>
        <xsl:value-of select="$I"/>
        <xsl:text>end Find;&#10;</xsl:text>
        
      </xsl:when>
      
    </xsl:choose>
    
    <!-- .. attribute accessors .. -->
    <xsl:apply-templates mode="attribute-set-body"/>
    <xsl:apply-templates mode="attribute-get-body"/>
    
    <xsl:if test="not(@singleton)">
      
      <!-- .. the hash function stub .. -->
      <xsl:value-of select="$I"/>
      <xsl:text>function Hash (Id : Identifier) return Natural is separate;&#10;</xsl:text>
      
    </xsl:if>
    
    <!-- .. operation stubs .. -->
    <xsl:call-template name="operation-body-stubs"/>
    
    <!-- and close. -->
    <xsl:text>end </xsl:text>
    <xsl:value-of select="../name"/>.<xsl:value-of select="name"/>
    <xsl:text>;&#10;</xsl:text>
    
    <xsl:if test="not(@singleton)">
      
      <!-- Output the separate hash function body. -->
      <xsl:call-template name="hash-function"/>
      
    </xsl:if>
    
    <xsl:if test="@active">
      <xsl:text>separate (</xsl:text>
      <xsl:value-of select="../name"/>.<xsl:value-of select="name"/>
      <xsl:text>)&#10;</xsl:text>
      <xsl:text>task body T is&#10;</xsl:text>
      <xsl:text>begin&#10;</xsl:text>
      <xsl:value-of select="$I"/>
      <xsl:text>raise Program_Error;&#10;</xsl:text>
      <xsl:text>end T;&#10;</xsl:text>
    </xsl:if>

    <!-- Child subprogram bodies for individual operations. -->
    <xsl:call-template name="operation-bodies"/>

  </xsl:template>

  <xsl:template mode="class-body" match="*"/>


  <!-- Called from domain/class to generate context clauses for package
       body.
       There is (GNAT 3.14a1) an issue with "with type", so we include
       corresponding "with"'s for the packages involved. -->
  <xsl:template name="class-body-context">

    <!-- The classes to be processed this time. The default is the
         current class. -->
    <xsl:param name="parents" select="."/>

    <!-- The ancestors so far. The default value is "null". -->
    <xsl:param name="ancestors" select="/.."/>

    <xsl:choose>

      <xsl:when test="$parents">

        <!-- Still something to collect; call self recursively with the
             parent node(s). -->
        <xsl:call-template name="class-body-context">
          <xsl:with-param
            name="parents"
            select="../class[name=../inheritance[child=$parents/name]/parent]"/>
          <xsl:with-param name="ancestors" select="$parents | $ancestors"/>
        </xsl:call-template>

      </xsl:when>

      <xsl:otherwise>

        <!-- $ancestors contains all the nodes to be processed. -->

        <xsl:if test="not(@singleton)">
          <!-- We'll need to free memory. -->
          <xsl:text>with Ada.Unchecked_Deallocation;&#10;</xsl:text>
        </xsl:if>

        <!-- Withs for attributes of the current class -->
        <xsl:for-each
          select="attribute[@refers and not(@refers=../name)]">
          <xsl:sort select="@refers"/>
          <xsl:text>with </xsl:text>
          <xsl:value-of select="../../name"/>
          <xsl:text>.</xsl:text>
          <xsl:value-of select="@refers"/>
          <xsl:text>;&#10;</xsl:text>
        </xsl:for-each>

        <!-- Withs for subprograms of this and ancestor classes -->

        <xsl:apply-templates
          mode="operation-body-context"
          select="$ancestors/operation">
          <xsl:with-param name="current" select="."/>
        </xsl:apply-templates>

        <!-- Complete subtype handles, needed for the subtype selection
             record. -->
        <xsl:call-template name="supertype-body-context"/>

      </xsl:otherwise>

    </xsl:choose>


  </xsl:template>


  <!-- Called to assign values to attributes on creation (identifier
       attributes only) -->
  <xsl:template
    match="attribute[@identifier]"
    mode="identifier-element-assignment">
    <xsl:value-of select="$II"/>
    <xsl:text>Result.</xsl:text>
    <xsl:call-template name="attribute-name"/>
    <xsl:text>&#10;</xsl:text>
    <xsl:value-of select="$IIC"/>
    <xsl:text>:= With_Identifier.</xsl:text>
    <xsl:call-template name="attribute-name"/>
    <xsl:text>;&#10;</xsl:text>
  </xsl:template>

  <xsl:template mode="identifier-element-assignment" match="*"/>


  <!-- Called to generate access-to-subprogram types. -->
  <xsl:template
    match="operation[@access]"
    mode="access-to-operation">
    <xsl:value-of select="$I"/>
    <xsl:text>type </xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text> is access </xsl:text>
    <xsl:choose>
      <xsl:when test="@result">
        <xsl:text>function</xsl:text>
        <xsl:call-template name="parameter-list">
          <xsl:with-param name="indent" select="$I"/>
        </xsl:call-template>
        <xsl:text>&#10;</xsl:text>
        <xsl:value-of select="$II"/>
        <xsl:text>return </xsl:text>
        <xsl:call-template name="type-name">
          <xsl:with-param name="type" select="@return"/>
          <xsl:with-param name="class" select=".."/>
        </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>procedure</xsl:text>
        <xsl:call-template name="parameter-list">
          <xsl:with-param name="indent" select="$IC"/>
        </xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:text>;&#10;</xsl:text>
  </xsl:template>

  <xsl:template match="*" mode="access-to-operation"/>


  <!-- Called from domain/class to generate the Create function spec. -->
  <xsl:template name="create-function-spec">
    <xsl:choose>

      <xsl:when test="count(attribute[@identifier])=1
                      and attribute[@identifier]/type='Autonumber'">
        <xsl:value-of select="$I"/>
        <xsl:text>function Create return Handle;&#10;</xsl:text>
      </xsl:when>

      <xsl:otherwise>
        <xsl:if test="attribute/type='Autonumber'">
          <xsl:message terminate="yes">
            <xsl:text>CF: invalid use of Autonumber in </xsl:text>
            <xsl:value-of select="name"/>
          </xsl:message>
        </xsl:if>
        <xsl:value-of select="$I"/>
        <xsl:text>function Create (With_Identifier : Identifier) return Handle;&#10;</xsl:text>
      </xsl:otherwise>

    </xsl:choose>
  </xsl:template>


  <!-- Called from domain/class to generate the Create function body. -->
  <xsl:template name="create-function-body">
    <xsl:choose>

      <xsl:when test="count(attribute[@identifier])=1
                      and attribute[@identifier]/type='Autonumber'">
        <xsl:variable name="id" select="attribute[@identifier]/name"/>
        <xsl:value-of select="$I"/>
        <xsl:text>Next_Identifier : Integer := 0;&#10;</xsl:text>
        <xsl:value-of select="$I"/>
        <xsl:text>function Create return Handle is&#10;</xsl:text>
        <xsl:value-of select="$II"/>
        <xsl:text>Result : Handle;&#10;</xsl:text>
        <xsl:value-of select="$II"/>
        <xsl:text>Id : Identifier;&#10;</xsl:text>
        <xsl:value-of select="$I"/>
        <xsl:text>begin&#10;</xsl:text>
        <xsl:value-of select="$II"/>
        <xsl:text>Result := new Instance;&#10;</xsl:text>
        <xsl:value-of select="$II"/>
        <xsl:text>Result.</xsl:text>
        <xsl:value-of select="$id"/>
        <xsl:text> := Next_Identifier;&#10;</xsl:text>
        <xsl:value-of select="$II"/>
        <xsl:text>Id.</xsl:text>
        <xsl:value-of select="$id"/>
        <xsl:text> := Next_Identifier;&#10;</xsl:text>
        <xsl:value-of select="$II"/>
        <xsl:text>Next_Identifier := Next_Identifier + 1;&#10;</xsl:text>
        <xsl:value-of select="$II"/>
        <xsl:text>Maps.Bind (The_Container, Id, Result);&#10;</xsl:text>
        <xsl:call-template name="set-parent-child-info">
          <xsl:with-param name="handle" select="'Result'"/>
        </xsl:call-template>
        <xsl:value-of select="$II"/>
        <xsl:text>return Result;&#10;</xsl:text>
        <xsl:value-of select="$I"/>
        <xsl:text>end Create;&#10;</xsl:text>
      </xsl:when>

      <xsl:otherwise>
        <xsl:value-of select="$I"/>
        <xsl:text>function Create (With_Identifier : Identifier) return Handle is&#10;</xsl:text>
        <xsl:value-of select="$II"/>
        <xsl:text>Result : Handle;&#10;</xsl:text>
        <xsl:value-of select="$I"/>
        <xsl:text>begin&#10;</xsl:text>
        <xsl:value-of select="$II"/>
        <xsl:text>Result := new Instance;&#10;</xsl:text>
        <xsl:apply-templates
          select="attribute[@identifier]"
          mode="identifier-element-assignment"/>
        <xsl:value-of select="$II"/>
        <xsl:text>Maps.Bind (The_Container, With_Identifier, Result);&#10;</xsl:text>
        <xsl:call-template name="set-parent-child-info">
          <xsl:with-param name="handle" select="'Result'"/>
        </xsl:call-template>
        <xsl:value-of select="$II"/>
        <xsl:text>return Result;&#10;</xsl:text>
        <xsl:value-of select="$I"/>
        <xsl:text>end Create;&#10;</xsl:text>
      </xsl:otherwise>

    </xsl:choose>
  </xsl:template>


  <!-- Called from domain/class, within the Create function, to
       set the parents' Current Child record. -->
  <xsl:template name="set-parent-child-info">
    <xsl:param name="handle" select="'This'"/>

    <!--
         {parent}.Set_{relation}_Child
           ({handle}.{relation}_Parent,
            (Current => {parent}.{child}_T),
             {abbrev} => {handle});
         -->
    
    <!-- Save the current class -->
    <xsl:variable name="current" select="."/>

    <xsl:for-each select="/domain/inheritance[child=$current/name]">
      <xsl:sort select="name"/>

      <xsl:value-of select="$II"/>
      <xsl:value-of select="parent"/>
      <xsl:text>.Set_</xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text>_Child&#10;</xsl:text>
      <xsl:value-of select="$IIC"/>
      <xsl:text>(</xsl:text>
      <xsl:value-of select="$handle"/>
      <xsl:text>.</xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text>_Parent,&#10;</xsl:text>
      <xsl:value-of select="$IIC"/>
      <xsl:text> (Current =&gt; </xsl:text>
      <xsl:value-of select="parent"/>
      <xsl:text>.</xsl:text>
      <xsl:value-of select="$current/name"/>
      <xsl:text>_T, </xsl:text>
      <xsl:value-of select="$current/abbreviation"/>
      <xsl:text> =&gt; </xsl:text>
      <xsl:value-of select="$handle"/>
      <xsl:text>));&#10;</xsl:text>
    </xsl:for-each>

  </xsl:template>


  <!-- Called from domain/class to create the Find function body. -->
  <xsl:template name="find-function-body">
    <xsl:value-of select="$I"/>
    <xsl:text>function Find (With_Identifier : Identifier) return Handle is&#10;</xsl:text>
    <xsl:value-of select="$I"/>
    <xsl:text>begin&#10;</xsl:text>
    <xsl:value-of select="$II"/>
    <xsl:text>if Maps.Is_Bound (The_Container, With_Identifier) then&#10;</xsl:text>
    <xsl:value-of select="$III"/>
    <xsl:text>return Maps.Item_Of (The_Container, With_Identifier);&#10;</xsl:text>
    <xsl:value-of select="$II"/>
    <xsl:text>else&#10;</xsl:text>
    <xsl:value-of select="$III"/>
    <xsl:text>return null;&#10;</xsl:text>
    <xsl:value-of select="$II"/>
    <xsl:text>end if;&#10;</xsl:text>
    <xsl:value-of select="$I"/>
    <xsl:text>end Find;&#10;</xsl:text>
  </xsl:template>


  <!-- Called from domain/class to create the class delete
       procedure body. -->
  <xsl:template name="class-delete-procedure-body">
    <xsl:value-of select="$I"/>
    <xsl:text>procedure Delete (With_Identifier : Identifier) is&#10;</xsl:text>
    <xsl:value-of select="$II"/>
    <xsl:text>H : Handle;&#10;</xsl:text>
    <xsl:value-of select="$I"/>
    <xsl:text>begin&#10;</xsl:text>
    <xsl:value-of select="$II"/>
    <xsl:text>H := Maps.Item_Of (The_Container, With_Identifier);&#10;</xsl:text>
    <xsl:call-template name="subtype-deletion">
      <xsl:with-param name="handle" select="'H'"/>
    </xsl:call-template>
    <xsl:call-template name="perform-finalization">
      <xsl:with-param name="handle" select="'H'"/>
    </xsl:call-template>
    <xsl:call-template name="clear-parent-child-info">
      <xsl:with-param name="handle" select="'H'"/>
    </xsl:call-template>
    <xsl:value-of select="$II"/>
    <xsl:text>Maps.Unbind (The_Container, With_Identifier);&#10;</xsl:text>
    <xsl:value-of select="$II"/>
    <xsl:text>Free (H);&#10;</xsl:text>
    <xsl:value-of select="$I"/>
    <xsl:text>end Delete;&#10;</xsl:text>
  </xsl:template>


  <!-- Called from domain/class to create the instance delete
       procedure body. -->
  <xsl:template name="delete-procedure-body">
    <xsl:value-of select="$I"/>
    <xsl:text>procedure Delete (This : in out Handle) is&#10;</xsl:text>
    <xsl:value-of select="$I"/>
    <xsl:text>begin&#10;</xsl:text>
    <!-- This check is because of what seems to be a GNAT error for
         fixed-size storage pools; the wrong exception is raised. -->
    <xsl:value-of select="$II"/>
    <xsl:text>if This = null then&#10;</xsl:text>
    <xsl:value-of select="$III"/>
    <xsl:text>raise Constraint_Error;&#10;</xsl:text>
    <xsl:value-of select="$II"/>
    <xsl:text>end if;&#10;</xsl:text>
    <xsl:call-template name="subtype-deletion">
      <xsl:with-param name="handle" select="'This'"/>      
    </xsl:call-template>
    <xsl:call-template name="perform-finalization">
      <xsl:with-param name="handle" select="'This'"/>      
    </xsl:call-template>
    <xsl:call-template name="clear-parent-child-info">
      <xsl:with-param name="handle" select="'This'"/>
    </xsl:call-template>
    <xsl:value-of select="$II"/>
    <xsl:text>Maps.Unbind&#10;</xsl:text>
    <xsl:value-of select="$IIC"/>
    <xsl:text>(The_Container,&#10;</xsl:text>
    <xsl:value-of select="$IIC"/>
    <xsl:text> (</xsl:text>
    <xsl:for-each select="attribute[@identifier]">
      <xsl:call-template name="attribute-name"/>
      <xsl:text> =&gt; This.</xsl:text>
      <xsl:call-template name="attribute-name"/>
      <xsl:if test="position() &lt; last()">
        <xsl:text>,&#10;  </xsl:text>
        <xsl:value-of select="$IIC"/>
      </xsl:if>
    </xsl:for-each>
    <xsl:text>));&#10;</xsl:text>
    <xsl:value-of select="$II"/>
    <xsl:text>Free (This);&#10;</xsl:text>
    <xsl:value-of select="$I"/>
    <xsl:text>end Delete;&#10;</xsl:text>
  </xsl:template>


  <!-- Called from domain/class, within one of the Delete procedures,
       to delete any current children in inheritance relationships. -->
  <xsl:template name="subtype-deletion">
    <xsl:param name="handle" select="'This'"/>

    <!--
         case {handle}.{relation}_Current_Child.Current is
           when {child-1}_T =>
             {child-1}.Delete ({handle}.{relation}_Current_Child.{child-1-abbrev};
           when Null_T => null;
         end case;
         -->

    <!-- Save the current class -->
    <xsl:variable name="current" select="."/>

    <!-- XXX won't work with partitioned inheritance -->
    <xsl:variable
      name="rel"
      select="/domain/inheritance[parent=$current/name]"/>

    <xsl:if test="$rel">
      
      <xsl:value-of select="$II"/>
      <xsl:text>case </xsl:text>
      <xsl:value-of select="$handle"/>
      <xsl:text>.</xsl:text>
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
        <xsl:value-of select="."/>
        <xsl:text>.Delete (</xsl:text>
        <xsl:value-of select="$handle"/>
        <xsl:text>.</xsl:text>
        <xsl:value-of select="$rel/name"/>
        <xsl:text>_Current_Child.</xsl:text>
        <xsl:value-of select="/domain/class[name=$child]/abbreviation"/>
        <xsl:text>);&#10;</xsl:text>

      </xsl:for-each>
      
      <xsl:value-of select="$III"/>
      <xsl:text>when Null_T =&gt; null;&#10;</xsl:text>

      <xsl:value-of select="$II"/>
      <xsl:text>end case;&#10;</xsl:text>

    </xsl:if>

  </xsl:template>
  

  <!-- Called from domain/class, within the Delete procedure, to
       null the parents' Current Child record. -->
  <xsl:template name="clear-parent-child-info">
    <xsl:param name="handle" select="'This'"/>

    <!--
         {parent}.Set_{relation}_Child
           ({handle}.{relation}_Parent, (Current => {parent}.Null_T));
         -->
    
    <!-- Save the current class -->
    <xsl:variable name="current" select="."/>

    <xsl:for-each select="/domain/inheritance[child=$current/name]">
      <xsl:sort select="name"/>

      <xsl:value-of select="$II"/>
      <xsl:value-of select="parent"/>
      <xsl:text>.Set_</xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text>_Child&#10;</xsl:text>
      <xsl:value-of select="$IIC"/>
      <xsl:text>(</xsl:text>
      <xsl:value-of select="$handle"/>
      <xsl:text>.</xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text>_Parent, </xsl:text>
      <xsl:text>(Current =&gt; </xsl:text>
      <xsl:value-of select="parent"/>
      <xsl:text>.Null_T));&#10;</xsl:text>
    </xsl:for-each>

  </xsl:template>


  <!-- Called from domain/class, within the Delete procedures, to
       call any finalization procedures. -->
  <xsl:template name="perform-finalization">
    <xsl:param name="handle" select="'This'"/>

    <xsl:for-each select="operation[@finalize
                          and not(@return)
                          and not(@class)
                          and not(parameter)]">
      <xsl:sort select="name"/>

      <xsl:value-of select="$II"/>
      <xsl:value-of select="name"/>
      <xsl:text> (</xsl:text>
      <xsl:value-of select="$handle"/>
      <xsl:text>);&#10;</xsl:text>
    </xsl:for-each>

  </xsl:template>


  <!-- Called from domain/class to generate the separate hash function. -->
  <xsl:template name="hash-function">
    <xsl:if test="attribute/type/@identifier/..='Unbounded_String'
                  or attribute/type/@identifier/..='Text'">
      <xsl:text>with ColdFrame.String_Hash;&#10;</xsl:text>
    </xsl:if>
    <xsl:text>separate (</xsl:text>
    <xsl:value-of select="../name"/>.<xsl:value-of select="name"/>
    <xsl:text>)&#10;</xsl:text>
    <xsl:text>function Hash (Id : Identifier) return Natural is&#10;</xsl:text>
    <xsl:text>begin&#10;</xsl:text>
    <xsl:value-of select="$I"/>
    <xsl:text>return 0;&#10;</xsl:text>
    <xsl:text>end Hash;&#10;</xsl:text>
  </xsl:template>


  <!-- Called from domain/class to compute the number of hash buckets. -->
  <xsl:template name="hash-buckets">
    <xsl:choose>

      <xsl:when test="./@max">
        <xsl:variable name="max" select="./@max"/>
        <xsl:choose>
          <xsl:when test="$max &lt; 11">
            <xsl:text>1</xsl:text>
          </xsl:when>
          <xsl:when test="$max &lt; 51">
            <xsl:text>5</xsl:text>
          </xsl:when>
          <xsl:when test="$max &lt; 101">
            <xsl:text>11</xsl:text>
          </xsl:when>
          <xsl:when test="$max &lt; 501">
            <xsl:text>29</xsl:text>
          </xsl:when>
          <xsl:otherwise>
            <xsl:text>43</xsl:text>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:when>

      <xsl:otherwise>
        <xsl:text>19</xsl:text>
      </xsl:otherwise>

    </xsl:choose>

  </xsl:template>


  <!-- Called from domain/class to generate a task spec. -->
  <xsl:template name="task-spec">
    <!--
         task type t (this : access instance) is
           entry e (parameters);
         end t;
         -->
    <xsl:value-of select="$I"/>
    <xsl:text>task type T (This : access Instance) is&#10;</xsl:text>
    <xsl:apply-templates mode="task-entry" select="operation"/>
    <xsl:value-of select="$I"/>
    <xsl:text>end T;&#10;</xsl:text>
  </xsl:template>


  <!-- Generate task entry specs. -->
  <xsl:template
    mode="task-entry"
    match="operation[not(@return) and not(@class)]">

    <!-- Checking for operations whose profile matches an accessor. -->
    <xsl:variable name="n" select="name"/>
    <xsl:variable name="att-to-set"
      select="../attribute[concat('Set_',name)=$n]"/>

    <xsl:if test="not($generate-accessors='defined')
                  or not(count(parameter)=1)
                  or not($att-to-set/type=parameter/type)">
      
      <xsl:value-of select="$I"/>
      <xsl:text>entry </xsl:text>
      <xsl:value-of select="name"/>
      <xsl:call-template name="entry-parameter-list">
        <xsl:with-param name="indent" select="$IIC"/>
      </xsl:call-template>
      <xsl:text>;&#10;</xsl:text>
      
    </xsl:if>

  </xsl:template>

  <xsl:template mode="task-entry" match="*"/>


  <!-- Called from class/operation to generate an entry parameter list -->
  <xsl:template name="entry-parameter-list">
    <xsl:param name="indent" select="''"/>

    <!-- In Ada, an empty parameter list is void (not "()" as in C).
         If the operation has parameters, we clearly need a parameter
         list here! -->
    <xsl:if test="parameter">

      <xsl:text>&#10;</xsl:text>
      <xsl:value-of select="$indent"/>
      <xsl:text>(</xsl:text>
      <xsl:apply-templates mode="parameter">
        <xsl:with-param name="indent" select="$indent"/>
      </xsl:apply-templates>
      <xsl:text>)</xsl:text>

    </xsl:if>

  </xsl:template>


</xsl:stylesheet>
