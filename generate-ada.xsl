<!-- $Id: generate-ada.xsl,v b0f2e9c45e67 2001/04/01 10:04:17 simon $ -->
<!-- XSL stylesheet to generate Ada code. -->
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

  <xsl:strip-space elements="*"/>

  <xsl:output method="text"/>


  <!-- Generate the top-level package for the domain, then all the
       others. -->
  <xsl:template match="domain">

    <!-- Any context clause needed for top-level package .. -->
    <xsl:apply-templates mode="domain-context"/>

    <!-- .. the top-level package spec .. -->
    <xsl:text>package </xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text> is&#10;</xsl:text>

    <!-- .. any specially-declared types .. -->
    <xsl:apply-templates select="type" mode="domain-type"/>

    <!-- .. and close. -->
    <xsl:text>end </xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>;&#10;</xsl:text>

    <!-- Any support packages for specially-declared types. -->
    <xsl:apply-templates select="type" mode="domain-type-support"/>

    <!-- Package specs for individual classes. -->
    <xsl:apply-templates select="class" mode="class-spec"/>

    <!-- Package bodies for individual classes. -->
    <xsl:apply-templates select="class" mode="class-body"/>

    <!-- Collection support packages. -->
    <xsl:apply-templates select="class" mode="collection-support"/>

    <!-- Child subprogram specs for individual operations. -->
    <xsl:apply-templates select="class/operation" mode="operation-spec"/>

    <!-- Child subprogram bodies for individual operations. -->
    <xsl:apply-templates select="class/operation" mode="operation-body"/>

  </xsl:template>


  <!-- Generate domain context clauses. -->
  <xsl:template mode="domain-context" match="domain/type">
    <xsl:choose>

      <xsl:when test="string/max">
      <!-- string/max implies an instantiation of Ada.Strings.Bounded -->
        <xsl:text>with Ada.Strings.Bounded;</xsl:text>
        <xsl:text> use Ada.Strings.Bounded;&#10;</xsl:text>
      </xsl:when>

    </xsl:choose>
  </xsl:template>


  <!-- Generate domain Types entries (not for standard types). -->
  <xsl:template mode="domain-type" match="domain/type">
    <xsl:if test="not(standard)">
      <xsl:choose>

        <xsl:when test="enumeration">
          <xsl:text>  type </xsl:text>
          <xsl:value-of select="name"/>
          <xsl:text> is </xsl:text>
          <xsl:text>&#10;    (</xsl:text>
          <xsl:for-each select="enumeration/literal">
            <xsl:value-of select="."/>
            <xsl:if test="position() &lt; last()">
              <xsl:text>,&#10;     </xsl:text>
            </xsl:if>
          </xsl:for-each>
          <xsl:text>);&#10;</xsl:text>
        </xsl:when>

        <xsl:when test="integer">
          <xsl:text>  type </xsl:text>
          <xsl:value-of select="name"/>
          <xsl:text> is range </xsl:text>
          <xsl:value-of select="integer/lower"/>
          <xsl:text> .. </xsl:text>
          <xsl:value-of select="integer/upper"/>
          <xsl:text>;&#10;</xsl:text>
        </xsl:when>

        <xsl:when test="real">
          <xsl:text>  type </xsl:text>
          <xsl:value-of select="name"/>
          <xsl:text> is digits </xsl:text>
          <xsl:value-of select="real/digits"/>
          <xsl:text> range </xsl:text>
          <xsl:value-of select="real/lower"/>
          <xsl:text> .. </xsl:text>
          <xsl:value-of select="real/upper"/>
          <xsl:text>;&#10;</xsl:text>
        </xsl:when>

        <!-- sets are implemented as class Collections; no action here -->
        <xsl:when test="set"/>

        <xsl:when test="string">
          <xsl:text>  package </xsl:text>
          <xsl:value-of select="name"/>
          <xsl:text>_Package is&#10;</xsl:text>
          <xsl:text>     new Generic_Bounded_Length (Max =&gt; </xsl:text>
          <xsl:value-of select="string/max"/>
          <xsl:text>);&#10;</xsl:text>
          <xsl:text>  subtype </xsl:text>
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
    </xsl:if>
  </xsl:template>


  <!-- Generate domain Types support entries (not for standard types).
       We do these as child packages in case they're not actually 
       needed. -->
  <xsl:template mode="domain-type-support" match="domain/type">
    <xsl:if test="not(standard)">
      <xsl:choose>

        <xsl:when test="enumeration"/>

        <xsl:when test="integer"/>

        <xsl:when test="real"/>

        <xsl:when test="set"/>

        <xsl:when test="string">
          <xsl:text>with Architecture.String_Hash;&#10;</xsl:text>
          <xsl:text>function </xsl:text>
          <xsl:value-of select="../name"/>
          <xsl:text>.</xsl:text>
          <xsl:value-of select="name"/>
          <xsl:text>_Hash is&#10;</xsl:text>
          <xsl:text>   new Architecture.String_Hash.Bounded_Hash (</xsl:text>
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
            <xsl:text>      Storage_Manager =&gt; Architecture.Global_Storage_Pool.Pool_Type,&#10;</xsl:text>
            <xsl:text>      Storage =&gt; Architecture.Global_Storage_Pool.Pool);&#10;</xsl:text>
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
          <xsl:text>with Architecture.Global_Storage_Pool;&#10;</xsl:text>
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


  <!-- Called from domain/class to generate get specs -->
  <xsl:template
    match="class/attribute"
    mode="attribute-get-spec">

    <!-- Get function -->
    <xsl:text>  function Get_</xsl:text>
    <xsl:call-template name="attribute-name"/>

    <!-- If this isn't a singleton, we need a handle parameter -->
    <xsl:if test="not(../@singleton)">
      <xsl:text> (This : Handle)</xsl:text>
    </xsl:if>

    <xsl:text> return </xsl:text>
    <xsl:call-template name="attribute-type"/>
    <xsl:text>;&#10;</xsl:text>

  </xsl:template>


  <!-- Called from domain/class to generate set specs (non-
       identifier attributes only) -->
  <xsl:template
    match="class/attribute[not(@identifier='yes')]"
    mode="attribute-set-spec">

    <!-- Set procedure -->
    <xsl:text>  procedure Set_</xsl:text>
    <xsl:call-template name="attribute-name"/>
    <xsl:text> (</xsl:text>

    <!-- If this isn't a singleton, we need a handle parameter -->
    <xsl:if test="not(../@singleton)">
      <xsl:text>This : Handle; </xsl:text>
    </xsl:if>

    <xsl:text>To_Be : </xsl:text>
    <xsl:call-template name="attribute-type"/>
    <xsl:text>);&#10;</xsl:text>

  </xsl:template>


  <!-- Generate the class packages (bodies). -->
  <xsl:template match="domain/class" mode="class-body">

    <!-- If there are no attributes, there's no body -->
    <xsl:if test="attribute">

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


  <!-- Called from domain/class to generate context clauses for package
       body. -->
  <xsl:template name="class-body-context">
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


  <!-- Called from domain/class to generate get bodies -->
  <xsl:template
    match="class/attribute"
    mode="attribute-get-body">

    <!-- Get function -->
    <xsl:text>  function Get_</xsl:text>
    <xsl:call-template name="attribute-name"/>

    <!-- If this isn't a singleton, we need a handle parameter -->
    <xsl:if test="not(../@singleton)">
      <xsl:text> (This : Handle)</xsl:text>
    </xsl:if>

    <xsl:text> return </xsl:text>
    <xsl:call-template name="attribute-type"/>
    <xsl:text> is&#10;</xsl:text>
    <xsl:text>  begin&#10;</xsl:text>
    <xsl:text>    return This.</xsl:text>
    <xsl:call-template name="attribute-name"/>
    <xsl:text>;&#10;</xsl:text>
    <xsl:text>  end Get_</xsl:text>
    <xsl:call-template name="attribute-name"/>
    <xsl:text>;&#10;</xsl:text>

  </xsl:template>


  <!-- Called from domain/class to generate set bodies (non-
       identifier attributes only) -->
  <xsl:template
    match="class/attribute[not(@identifier='yes')]"
    mode="attribute-set-body">

    <!-- Set procedure -->
    <xsl:text>  procedure Set_</xsl:text>
    <xsl:call-template name="attribute-name"/>
    <xsl:text> (</xsl:text>

    <!-- If this isn't a singleton, we need a handle parameter -->
    <xsl:if test="not(../@singleton)">
      <xsl:text>This : Handle; </xsl:text>
    </xsl:if>

    <xsl:text>To_Be : </xsl:text>
    <xsl:call-template name="attribute-type"/>
    <xsl:text>) is&#10;</xsl:text>
    <xsl:text>  begin&#10;</xsl:text>
    <xsl:text>    This.</xsl:text>
    <xsl:call-template name="attribute-name"/>
    <xsl:text> := To_Be;&#10;</xsl:text>
    <xsl:text>  end Set_</xsl:text>
    <xsl:call-template name="attribute-name"/>
    <xsl:text>;&#10;</xsl:text>

  </xsl:template>


  <!-- Called from domain/class to generate the separate hash function. -->
  <xsl:template name="hash-function">
    <!-- XXX This needs to recognise Bounded String usage, too -->
    <xsl:if test="attribute/type/@identifier/..='Unbounded_String'
                  or attribute/type/@identifier/..='Text'">
      <xsl:text>with Architecture.String_Hash;&#10;</xsl:text>
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


  <!-- Generate child subprogram specs. -->
  <xsl:template match="class/operation" mode="operation-spec">
    <xsl:apply-templates select="." mode="operation-context"/>
    <xsl:call-template name="subprogram-specification"/>
    <xsl:text>;&#10;</xsl:text>
  </xsl:template>


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


  <!-- Called to generate Collection support packages (only for
       non-singleton classes). -->
  <xsl:template match="class[not(@singleton)]" mode="collection-support">
    <xsl:apply-templates select="." mode="collection-support-spec"/>
    <xsl:apply-templates select="." mode="collection-support-body"/>
  </xsl:template>


  <!-- Called to generate Collection support package specs. -->
  <xsl:template match="class" mode="collection-support-spec">

    <!-- Make the name of the parent class (Domain.Class) -->
    <xsl:variable name="class">
      <xsl:value-of select="../name"/>
      <xsl:text>.</xsl:text>
      <xsl:value-of select="name"/>
    </xsl:variable>

    <!-- Abstract Containers package -->
    <xsl:text>with BC.Containers;&#10;</xsl:text>
    <xsl:text>package </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Abstract_Containers&#10;</xsl:text>
    <xsl:text>   is new BC.Containers (Handle);&#10;</xsl:text>

    <!-- Abstract Collections package -->
    <xsl:text>with BC.Containers.Collections;&#10;</xsl:text>
    <xsl:text>with </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Abstract_Containers;&#10;</xsl:text>
    <xsl:text>package </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Abstract_Collections&#10;</xsl:text>
    <xsl:text>   is new </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Abstract_Containers.Collections;&#10;</xsl:text>

    <!-- Concrete Collections package -->
    <xsl:choose>

      <xsl:when test="./@max">
        <!-- Wnen there's a maximum size, use the Bounded version -->
        <xsl:text>with BC.Containers.Collections.Bounded;&#10;</xsl:text>
        <xsl:text>with </xsl:text>
        <xsl:value-of select="$class"/>
        <xsl:text>.Abstract_Collections;&#10;</xsl:text>
        <xsl:text>package </xsl:text>
        <xsl:value-of select="$class"/>
        <xsl:text>.Collections&#10;</xsl:text>
        <xsl:text>   is new Abstract_Collections.Bounded (Maximum_Size =&gt; </xsl:text>
        <xsl:value-of select="./@max"/>
        <xsl:text>);&#10;</xsl:text>
      </xsl:when>

      <xsl:otherwise>
        <!-- Use the Unbounded version -->
        <xsl:text>with BC.Containers.Collections.Unbounded;&#10;</xsl:text>
        <xsl:text>with </xsl:text>
        <xsl:value-of select="$class"/>
        <xsl:text>.Abstract_Collections;&#10;</xsl:text>
        <xsl:text>package </xsl:text>
        <xsl:value-of select="$class"/>
        <xsl:text>.Collections&#10;</xsl:text>
        <xsl:text>   is new Abstract_Collections.Unbounded&#10;</xsl:text>
        <xsl:text>     (Storage_Manager =&gt; Architecture.Global_Storage_Pool.Pool_Type,&#10;</xsl:text>
        <xsl:text>      Storage =&gt; Architecture.Global_Storage_Pool.Pool);&#10;</xsl:text>
      </xsl:otherwise>

    </xsl:choose>

    <!-- Function to return a Collection of all the Instances -->
    <xsl:text>with </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Collections;&#10;</xsl:text>
    <xsl:text>function </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.All_Instances&#10;</xsl:text>
    <xsl:text>   return </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Collections.Collection;&#10;</xsl:text>

    <!-- Generic filter function to return a Collection of selected
         Instances -->
    <xsl:text>with </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Collections;&#10;</xsl:text>
    <xsl:text>generic&#10;</xsl:text>
    <xsl:text>  with function Pass (This : Handle) return Boolean is &lt;&gt;;&#10;</xsl:text>
    <xsl:text>function </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Selection_Function&#10;</xsl:text>
    <xsl:text>   return </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Collections.Collection;&#10;</xsl:text>

    <!-- Generic filter function for collections of Instances -->
    <xsl:text>with </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Collections;&#10;</xsl:text>
    <xsl:text>generic&#10;</xsl:text>
    <xsl:text>  with function Pass (This : Handle) return Boolean is &lt;&gt;;&#10;</xsl:text>
    <xsl:text>function </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Filter_Function&#10;</xsl:text>
    <xsl:text>   (The_Collection : </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Collections.Collection)&#10;</xsl:text>
    <xsl:text>   return </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Collections.Collection;&#10;</xsl:text>

  </xsl:template>


  <!-- Called to generate Collection support package bodies. -->
  <xsl:template match="class" mode="collection-support-body">

    <!-- Make the name of the parent class (Domain.Class) -->
    <xsl:variable name="class">
      <xsl:value-of select="../name"/>
      <xsl:text>.</xsl:text>
      <xsl:value-of select="name"/>
    </xsl:variable>

    <!-- Function to return a Collection of all the Instances -->
    <xsl:text>with BC.Copy;&#10;</xsl:text>
    <xsl:text>with </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Abstract_Containers;&#10;</xsl:text>
    <xsl:text>function </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.All_Instances&#10;</xsl:text>
    <xsl:text>   return </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Collections.Collection is&#10;</xsl:text>
    <xsl:text>  use </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Collections;&#10;</xsl:text>
    <xsl:text>  procedure Copy_Instances is new BC.Copy&#10;</xsl:text>
    <xsl:text>     (Item =&gt; Handle,&#10;</xsl:text>
    <xsl:text>      Source =&gt; Abstract_Map_Containers,&#10;</xsl:text>
    <xsl:text>      From =&gt; Maps.Map,&#10;</xsl:text>
    <xsl:text>      Target =&gt; Abstract_Containers,&#10;</xsl:text>
    <xsl:text>      To =&gt; Collection,&#10;</xsl:text>
    <xsl:text>      Clear =&gt; Collections.Clear,&#10;</xsl:text>
    <xsl:text>      Add =&gt; Collections.Append);&#10;</xsl:text>
    <xsl:text>  Result : Collection;&#10;</xsl:text>
    <xsl:text>begin&#10;</xsl:text>
    <xsl:text>  Copy_Instances (The_Container, Result);&#10;</xsl:text>
    <xsl:text>  return Result;&#10;</xsl:text>
    <xsl:text>end </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.All_Instances;&#10;</xsl:text>

    <!-- Generic filter function to return a Collection of selected
         Instances -->
    <xsl:text>with BC.Filter;&#10;</xsl:text>
    <xsl:text>with </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Abstract_Containers;&#10;</xsl:text>
    <xsl:text>function </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Selection_Function&#10;</xsl:text>
    <xsl:text>   return </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Collections.Collection is&#10;</xsl:text>
    <xsl:text>  use </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Collections;&#10;</xsl:text>
    <xsl:text>  procedure Filter is new BC.Filter&#10;</xsl:text>
    <xsl:text>     (Item =&gt; Handle,&#10;</xsl:text>
    <xsl:text>      Source =&gt; Abstract_Map_Containers,&#10;</xsl:text>
    <xsl:text>      From =&gt; Maps.Map,&#10;</xsl:text>
    <xsl:text>      Target =&gt; Abstract_Containers,&#10;</xsl:text>
    <xsl:text>      To =&gt; Collection,&#10;</xsl:text>
    <xsl:text>      Pass =&gt; Pass,&#10;</xsl:text>
    <xsl:text>      Clear =&gt; Collections.Clear,&#10;</xsl:text>
    <xsl:text>      Add =&gt; Collections.Append);&#10;</xsl:text>
    <xsl:text>  Result : Collection;&#10;</xsl:text>
    <xsl:text>begin&#10;</xsl:text>
    <xsl:text>  Filter (The_Container, Result);&#10;</xsl:text>
    <xsl:text>  return Result;&#10;</xsl:text>
    <xsl:text>end </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Selection_Function;&#10;</xsl:text>

    <!-- Generic filter function for collections of Instances -->
    <xsl:text>with BC.Filter;&#10;</xsl:text>
    <xsl:text>with </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Abstract_Containers;&#10;</xsl:text>
    <xsl:text>function </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Filter_Function&#10;</xsl:text>
    <xsl:text>   (The_Collection : </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Collections.Collection)&#10;</xsl:text>
    <xsl:text>   return </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Collections.Collection is&#10;</xsl:text>
    <xsl:text>  use </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Collections;&#10;</xsl:text>
    <xsl:text>  procedure Filter is new BC.Filter&#10;</xsl:text>
    <xsl:text>     (Item =&gt; Handle,&#10;</xsl:text>
    <xsl:text>      Source =&gt; Abstract_Containers,&#10;</xsl:text>
    <xsl:text>      From =&gt; Collection,&#10;</xsl:text>
    <xsl:text>      Target =&gt; Abstract_Containers,&#10;</xsl:text>
    <xsl:text>      To =&gt; Collection,&#10;</xsl:text>
    <xsl:text>      Pass =&gt; Pass,&#10;</xsl:text>
    <xsl:text>      Clear =&gt; Collections.Clear,&#10;</xsl:text>
    <xsl:text>      Add =&gt; Collections.Append);&#10;</xsl:text>
    <xsl:text>  Result : Collection;&#10;</xsl:text>
    <xsl:text>begin&#10;</xsl:text>
    <xsl:text>  Filter (The_Collection, Result);&#10;</xsl:text>
    <xsl:text>  return Result;&#10;</xsl:text>
    <xsl:text>end </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Filter_Function;&#10;</xsl:text>

  </xsl:template>


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


  <!-- Called from domain/class to generate the actual identifier
       record for the class. -->
  <xsl:template name="identifier-record">
    <xsl:choose>

      <!-- Output only identifier attributes. -->
      <xsl:when test="count(attribute[@identifier='yes']) &gt; 0">
        <xsl:text>  type Identifier is record&#10;</xsl:text>
        <xsl:apply-templates
          mode="instance-record-component"
          select="attribute[@identifier='yes']"/>
        <xsl:text>  end record;&#10;</xsl:text>
      </xsl:when>

      <xsl:otherwise>
        <xsl:text>  type Identifier is null record;&#10;</xsl:text>
      </xsl:otherwise>

    </xsl:choose>
  </xsl:template>


  <!-- Called from domain/class to generate the actual instance
       record for the class. -->
  <xsl:template name="instance-record">
    <xsl:param name="is-supertype"/>
    <xsl:choose>

      <!-- Output all attributes. -->
      <xsl:when test="count(attribute) &gt; 0">
        <xsl:text>  type Instance is record&#10;</xsl:text>
        <xsl:apply-templates
          mode="instance-record-component"
          select="attribute"/>
        <xsl:if test="$is-supertype">
          <xsl:text>    Current_Child : Child_Class;&#10;</xsl:text>
        </xsl:if>
        <xsl:text>  end record;&#10;</xsl:text>
      </xsl:when>

      <xsl:otherwise>
        <xsl:text>  type Instance is null record;&#10;</xsl:text>
      </xsl:otherwise>

    </xsl:choose>
  </xsl:template>


  <!-- Generate the individual components of the class identifier 
       or instance record. -->
  <xsl:template match="attribute" mode="instance-record-component">
    <xsl:text>    </xsl:text>
    <xsl:call-template name="attribute-name"/>
    <xsl:text> : </xsl:text>
    <xsl:call-template name="attribute-type"/>
    <xsl:if test="initial">
      <xsl:text> := </xsl:text>
      <xsl:value-of select="initial"/>
    </xsl:if>
    <xsl:text>;&#10;</xsl:text>
  </xsl:template>


  <!-- Generate attribute name. Called at class/attribute.
       If this is an anonymous referential attribute, we make up its
       name from the abbreviation of the supplier, the word _Handle_,
       and the relationship name.
       If not, just use the <name> element. -->
  <xsl:template name="attribute-name">
    <xsl:choose>
      <xsl:when test="@refers and not(name)">
        <xsl:variable name="target-class" select="@refers"/>
        <xsl:value-of select="/domain/class[name=$target-class]/abbreviation"/>
        <xsl:text>_Handle_</xsl:text>
        <xsl:value-of select="@relation"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="name"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>


  <!-- Generate attribute type. Called at class/attribute -->
  <xsl:template name="attribute-type">
    <xsl:choose>
      <xsl:when test="@refers">
        <xsl:value-of select="../../name"/>
        <xsl:text>.</xsl:text>
        <xsl:value-of select="@refers"/>
        <xsl:text>.Handle</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:call-template name="type-name">
          <xsl:with-param name="type" select="type"/>
        </xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>


  <!-- Handle special type name conversions. -->
  <xsl:template name="type-name">
    <xsl:param name="type"/>
    <xsl:choose>

      <!-- Text maps to Unbounded_String. -->
      <xsl:when test="$type='Text'">
        <xsl:text>Unbounded_String</xsl:text>
      </xsl:when>

      <!-- Class -->
      <xsl:when test="/domain/class/name=$type">
        <xsl:value-of select="/domain/name"/>
        <xsl:text>.</xsl:text>
        <xsl:value-of select="$type"/>
        <xsl:text>.Handle</xsl:text>
      </xsl:when>

      <!-- Set (only works for class instances) -->
      <xsl:when test="/domain/type[name=$type]/set">
        <xsl:variable name="type-name" select="/domain/type[name=$type]"/>
        <xsl:if test="$type-name/set">
          <xsl:value-of select="/domain/name"/>
          <xsl:text>.</xsl:text>
          <xsl:value-of select="$type-name/set"/>
          <xsl:text>.Collections.Collection</xsl:text>
        </xsl:if>
      </xsl:when>

      <xsl:otherwise>
        <xsl:value-of select="$type"/>
      </xsl:otherwise>

    </xsl:choose>
  </xsl:template>


  <!-- Catch unspecified default matches -->
  <xsl:template match="*"/>


  <!-- Catch unspecified mode="xxx" matches -->
  <xsl:template mode="attribute-get-body" match="*"/>
  <xsl:template mode="attribute-set-body" match="*"/>
  <xsl:template mode="attribute-get-spec" match="*"/>
  <xsl:template mode="attribute-set-spec" match="*"/>
  <xsl:template mode="class-spec" match="*"/>
  <xsl:template mode="class-body" match="*"/>
  <xsl:template mode="collection-support" match="*"/>
  <xsl:template mode="collection-support-body" match="*"/>
  <xsl:template mode="collection-support-spec" match="*"/>
  <xsl:template mode="domain-context" match="*"/>
  <xsl:template mode="domain-type" match="*"/>
  <xsl:template mode="domain-type-support" match="*"/>
  <xsl:template mode="identifier-element-assignment" match="*"/>
  <xsl:template mode="instance-record-component" match="*"/>
  <xsl:template mode="operation-body" match="*"/>
  <xsl:template mode="operation-context" match="*"/>
  <xsl:template mode="operation-spec" match="*"/>
  <xsl:template mode="parameter" match="*"/>


</xsl:stylesheet>
