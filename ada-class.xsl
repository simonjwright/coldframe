<!-- $Id: ada-class.xsl,v e15478df6eb7 2001/05/02 19:33:40 simon $ -->
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

    <!-- determine if this is a supertype (if there is an inheritance
         relationship with this class as the parent) -->
    <xsl:variable name="name" select="name"/>
    <xsl:variable
      name="is-supertype"
      select="boolean(../inheritance[parent=$name])"/>

    <!-- Any context clauses needed for the class package .. -->
    <xsl:call-template name="class-spec-context"/>

    <!-- .. the class package .. -->
    <xsl:if test="not(@interface)">
      <!-- only interface packages are externally visible -->
      <xsl:text>private </xsl:text>
    </xsl:if>
    <xsl:text>package </xsl:text>
    <xsl:value-of select="../name"/>.<xsl:value-of select="name"/>
    <xsl:text> is&#10;</xsl:text>

    <xsl:if test="not(@singleton)">

      <!-- .. the Identifier record .. -->
      <xsl:call-template name="identifier-record"/>

      <!-- .. the Instance record (indefinite, so it can't be
           allocated) .. -->
      <xsl:text>  type Instance (&lt;&gt;) is private;&#10;</xsl:text>

      <!-- .. the Handle .. -->
      <xsl:text>  type Handle is access Instance;&#10;</xsl:text>

      <!-- .. the creation, simple find, and deletion operations .. -->
      <xsl:text>  function Create (With_Identifier : Identifier) return Handle;&#10;</xsl:text>
      <xsl:text>  function Find (With_Identifier : Identifier) return Handle;&#10;</xsl:text>
      <xsl:text>  procedure Delete (With_Identifier : Identifier);&#10;</xsl:text>
      <xsl:text>  procedure Delete (This : in out Handle);&#10;</xsl:text>

      <!-- .. subtype enumeration support, if required .. -->
      <xsl:if test="$is-supertype">
        <xsl:call-template name="subtype-enumeration"/>
        <xsl:text>  procedure Set_Child_Class (This : Handle; To_Be : Child_Class);&#10;</xsl:text>
        <xsl:text>  function Get_Child_Class (This : Handle) return Child_Class;&#10;</xsl:text>
      </xsl:if>

    </xsl:if>

    <!-- .. the attribute access operations .. -->
    <xsl:apply-templates mode="attribute-set-spec"/>
    <xsl:apply-templates mode="attribute-get-spec"/>

    <!-- .. the private part .. -->
    <xsl:text>private&#10;</xsl:text>
    
    <xsl:choose>    
 
      <!-- There can be multiple instances, unless this is a singleton -->
      <xsl:when test="not(@singleton)">

        <!-- .. the Instance record .. -->
        <xsl:call-template name="instance-record">
          <xsl:with-param name="is-supertype" select="$is-supertype"/>
        </xsl:call-template>

        <!-- Use a fixed (effectively, static) storage pool if this is
             a bounded class.
             We have to use the GNAT 'Object_Size because 'Size is the
             minimal number of bits, not necessarily a whole number of
             bytes. -->
        <xsl:if test="@max">
          <xsl:text>  for Handle'Storage_Size use Instance'Object_Size / 8 * </xsl:text>
          <xsl:value-of select="@max"/>
          <xsl:text>;&#10;</xsl:text>
        </xsl:if>

        <!-- .. basic Container instantiation for Maps -->
        <xsl:text>  package Abstract_Map_Containers is new BC.Containers (Handle);&#10;</xsl:text>

        <!-- .. the Hash function spec .. -->
        <xsl:text>  function Hash (Id : Identifier) return Natural;&#10;</xsl:text>
        
        <!-- .. Container instantiations .. -->
        <xsl:text>  package Abstract_Maps is new Abstract_Map_Containers.Maps (Identifier);&#10;</xsl:text>

        <xsl:choose>

          <xsl:when test="@max">
            <!-- Wnen there's a maximum size, use the Bounded version -->
            <xsl:text>  package Maps is new Abstract_Maps.Bounded&#10;</xsl:text>
            <xsl:text>     (Hash =&gt; Hash,&#10;</xsl:text>
            <xsl:text>      Buckets =&gt; </xsl:text>
            <xsl:call-template name="hash-buckets"/>
            <xsl:text>,&#10;</xsl:text>
            <xsl:text>      Maximum_Size =&gt; </xsl:text>
            <xsl:value-of select="./@max"/>
            <xsl:text>);&#10;</xsl:text>
          </xsl:when>

          <xsl:otherwise>
            <!-- Use the Unbounded version -->
            <xsl:text>  package Maps is new Abstract_Maps.Unbounded&#10;</xsl:text>
            <xsl:text>     (Hash =&gt; Hash,&#10;</xsl:text>
            <xsl:text>      Buckets =&gt; </xsl:text>
            <xsl:call-template name="hash-buckets"/>
            <xsl:text>,&#10;</xsl:text>
            <xsl:text>      Storage_Manager =&gt; ColdFrame.Global_Storage_Pool.Pool_Type,&#10;</xsl:text>
            <xsl:text>      Storage =&gt; ColdFrame.Global_Storage_Pool.Pool);&#10;</xsl:text>
          </xsl:otherwise>

        </xsl:choose>

        <!-- .. the instance container .. -->
        <xsl:text>  The_Container : Maps.Map;&#10;</xsl:text>
        
      </xsl:when>

      <!-- if there is no identifier, there can only be one instance -->
      <xsl:when test="attribute">
        <xsl:call-template name="instance-record"/>
        <xsl:text>  This : Instance;&#10;</xsl:text>
      </xsl:when>

    </xsl:choose>

    <!-- .. and close. -->
    <xsl:text>end </xsl:text>
    <xsl:value-of select="../name"/>.<xsl:value-of select="name"/>
    <xsl:text>;&#10;</xsl:text>

  </xsl:template>

  <xsl:template mode="class-spec" match="*"/>


  <!-- Called from domain/class to generate context clauses for package
       spec -->
  <xsl:template name="class-spec-context">

    <xsl:if test="attribute/@refers">
      <!-- We're going to use the GNAT "with type" extension -->
      <xsl:text>pragma Extensions_Allowed (On);&#10;</xsl:text>
    </xsl:if>

    <xsl:if test="attribute/type='Unbounded_String'
                  or operation/parameter/type='Unbounded_String'
                  or attribute/type='Text'
                  or operation/parameter/type='Text'">
      <!-- All the above imply use of Unbounded_Strings. -->
      <xsl:text>with Ada.Strings.Unbounded;</xsl:text>
      <xsl:text> use Ada.Strings.Unbounded;&#10;</xsl:text>
    </xsl:if>

    <xsl:if test="attribute/type='Date'
                  or operation/parameter/type='Date'
                  or attribute/type='Time'
                  or operation/parameter/type='Time'">
      <!-- The above imply use of Ada.Calendar. -->
      <xsl:text>with Ada.Calendar;</xsl:text>
      <xsl:text> use Ada.Calendar;&#10;</xsl:text>
    </xsl:if>

    <xsl:if test="not(@singleton)">


      <!-- Choose the appropriate Map -->

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

    <!--XXX why doesn't this work always?<xsl:for-each
      select="attribute[@refers
              and not(@refers=preceding::attribute/@refers)]">-->
    <xsl:for-each
      select="attribute[@refers]">
      <!-- We need the GNAT 'with type' extension to include the
           handles (for non-singleton classes) -->
      <xsl:sort select="@refers"/>
      <xsl:text>with type </xsl:text>
      <xsl:value-of select="../../name"/>
      <xsl:text>.</xsl:text>
      <xsl:value-of select="@refers"/>
      <xsl:text>.Handle is access;&#10;</xsl:text>
    </xsl:for-each>

  </xsl:template>


  <!-- Called from a supertype domain/class to output the subtype enumeration
       type (sorted) -->
  <xsl:template name="subtype-enumeration">
    <xsl:variable name="name" select="name"/>
    <xsl:text>  type Child_Class is&#10;</xsl:text>
    <xsl:text>     (</xsl:text>
    <xsl:for-each select="../inheritance[parent=$name]/child">
      <xsl:sort select="."/>
      <xsl:value-of select="."/>
      <xsl:text>_T</xsl:text>
      <xsl:if test="position() &lt; last()">
        <xsl:text>,&#10;      </xsl:text>
      </xsl:if>
    </xsl:for-each>
    <xsl:text>);&#10;</xsl:text>
  </xsl:template>


  <!-- Generate the class packages (bodies). -->
  <xsl:template match="domain/class" mode="class-body">

    <!-- If it's a singleton with no attributes, there's no body. -->
    <!-- XXX If it's not a singleton and has no attributes, it doesn't
         matter very much if it won't compile. -->
    <xsl:if test="not(@singleton) or attribute">

      <!-- determine if this is a supertype (if there is an inheritance
           relationship with this class as the parent) -->
      <xsl:variable name="name" select="name"/>
      <xsl:variable
        name="is-supertype"
        select="boolean(../inheritance[parent=$name])"/>
      
      <!-- Any context clauses needed for the class body .. -->
      <xsl:call-template name="class-body-context"/>
      
      <!-- .. start the body .. -->
      <xsl:text>package body </xsl:text>
      <xsl:value-of select="../name"/>.<xsl:value-of select="name"/>
      <xsl:text> is&#10;</xsl:text>
      
      <xsl:if test="not(@singleton)">
        
        <!-- .. the creation, simple find, and deletion operations .. -->
        <xsl:text>  function Create (With_Identifier : Identifier) return Handle is&#10;</xsl:text>
        <xsl:text>    Result : Handle;&#10;</xsl:text>
        <xsl:text>  begin&#10;</xsl:text>
        <xsl:text>    Result := new Instance;&#10;</xsl:text>
        <xsl:apply-templates
          select="attribute[@identifier='yes']"
          mode="identifier-element-assignment"/>
        <xsl:text>    Maps.Bind (The_Container, With_Identifier, Result);&#10;</xsl:text>
        <xsl:text>    return Result;&#10;</xsl:text>
        <xsl:text>  end Create;&#10;</xsl:text>
        
        <xsl:text>  function Find (With_Identifier : Identifier) return Handle is&#10;</xsl:text>
        <xsl:text>  begin&#10;</xsl:text>
        <xsl:text>    if Maps.Is_Bound (The_Container, With_Identifier) then&#10;</xsl:text>
        <xsl:text>      return Maps.Item_Of (The_Container, With_Identifier);&#10;</xsl:text>
        <xsl:text>    else&#10;</xsl:text>
        <xsl:text>      return null;&#10;</xsl:text>
        <xsl:text>    end if;&#10;</xsl:text>
        <xsl:text>  end Find;&#10;</xsl:text>

        <xsl:text>  procedure Free is new Ada.Unchecked_Deallocation (Instance, Handle);&#10;</xsl:text>        
        <xsl:text>  procedure Delete (With_Identifier : Identifier) is&#10;</xsl:text>
        <xsl:text>    H : Handle;&#10;</xsl:text>
        <xsl:text>  begin&#10;</xsl:text>
        <xsl:text>    H := Maps.Item_Of (The_Container, With_Identifier);&#10;</xsl:text>
        <xsl:text>    Maps.Unbind (The_Container, With_Identifier);&#10;</xsl:text>
        <xsl:text>    Free (H);&#10;</xsl:text>
        <xsl:text>  end Delete;&#10;</xsl:text>
        
        <xsl:text>  procedure Delete (This : in out Handle) is&#10;</xsl:text>
        <xsl:text>  begin&#10;</xsl:text>
        <!-- This check is because of what seems to be a GNAT error for
             fixed-size storage pools. -->
        <xsl:text>    if This = null then&#10;</xsl:text>
        <xsl:text>      raise Constraint_Error;&#10;</xsl:text>
        <xsl:text>    end if;&#10;</xsl:text>
        <xsl:text>    Maps.Unbind&#10;</xsl:text>
        <xsl:text>      (The_Container,&#10;</xsl:text>
        <xsl:text>       (</xsl:text>
        <xsl:for-each select="attribute[@identifier='yes']">
          <xsl:call-template name="attribute-name"/>
          <xsl:text> =&gt; This.</xsl:text>
          <xsl:call-template name="attribute-name"/>
          <xsl:if test="position() &lt; last()">
            <xsl:text>,&#10;      </xsl:text>
          </xsl:if>
        </xsl:for-each>
        <xsl:text>));&#10;</xsl:text>
        <xsl:text>    Free (This);&#10;</xsl:text>
        <xsl:text>  end Delete;&#10;</xsl:text>
        
        <!-- .. subtype enumeration support, if required .. -->
        <xsl:if test="$is-supertype">
          <xsl:text>  procedure Set_Child_Class (This : Handle; To_Be : Child_Class) is&#10;</xsl:text>
          <xsl:text>  begin&#10;</xsl:text>
          <xsl:text>    This.Current_Child := To_Be;&#10;</xsl:text>
          <xsl:text>  end Set_Child_Class;&#10;</xsl:text>
          <xsl:text>  function Get_Child_Class (This : Handle) return Child_Class is&#10;</xsl:text>
          <xsl:text>  begin&#10;</xsl:text>
          <xsl:text>    return This.Current_Child;&#10;</xsl:text>
          <xsl:text>  end Get_Child_Class;&#10;</xsl:text>
        </xsl:if>
        
      </xsl:if>
      
      <!-- .. attribute accessors .. -->
      <xsl:apply-templates mode="attribute-set-body"/>
      <xsl:apply-templates mode="attribute-get-body"/>
        
      <xsl:if test="not(@singleton)">
        
        <!-- .. the hash function stub .. -->
        <xsl:text>  function Hash (Id : Identifier) return Natural is separate;&#10;</xsl:text>
        
      </xsl:if>
      
      <!-- and close. -->
      <xsl:text>end </xsl:text>
      <xsl:value-of select="../name"/>.<xsl:value-of select="name"/>
      <xsl:text>;&#10;</xsl:text>

      <xsl:if test="not(@singleton)">
        
        <!-- Output the separate hash function body. -->
        <xsl:call-template name="hash-function"/>
        
      </xsl:if>
      
    </xsl:if>

  </xsl:template>

  <xsl:template mode="class-body" match="*"/>


  <!-- Called from domain/class to generate context clauses for package
       body. -->
  <xsl:template name="class-body-context">

    <xsl:if test="attribute/@refers">
      <!-- We're going to use the GNAT "with type" extension.
           This pragma should only be needed on the spec, but GNAT
           3.14a requires it here too. -->
      <xsl:text>pragma Extensions_Allowed (On);&#10;</xsl:text>
    </xsl:if>

      <xsl:if test="not(@singleton)">
        <!-- We'll need to free memory. -->
        <xsl:text>with Ada.Unchecked_Deallocation;&#10;</xsl:text>
      </xsl:if>

  </xsl:template>


  <!-- Called to assign values to attributes on creation (identifier
       attributes only) -->
  <xsl:template
    match="attribute[@identifier='yes']"
    mode="identifier-element-assignment">
    <xsl:text>    Result.</xsl:text>
    <xsl:call-template name="attribute-name"/>
    <xsl:text> := With_Identifier.</xsl:text>
    <xsl:call-template name="attribute-name"/>
    <xsl:text>;&#10;</xsl:text>
  </xsl:template>

  <xsl:template mode="identifier-element-assignment" match="*"/>


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
    <xsl:text>  return 0;&#10;</xsl:text>
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


</xsl:stylesheet>
