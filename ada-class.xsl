<!-- $Id: ada-class.xsl,v 547c6ddc37be 2004/04/22 16:41:01 simon $ -->
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

     As a special exception, when portions of this file are copied by
     a stylesheet processor into an output file, this file does not by
     itself cause the resulting file to be covered by the GNU General
     Public License.  This exception does not however invalidate any
     other reasons why the output file might be covered by the GNU
     Public License.
     -->

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="1.1">


  <!-- Generate the class packages (specs). -->
  <xsl:template match="domain/class" mode="class-spec">

    <!-- Calculate the maximum number of instances. -->
    <xsl:variable name="max">
      <xsl:call-template name="number-of-instances"/>
    </xsl:variable>

    <!-- Determine whether an array can be used. -->
    <xsl:variable name="array">
      <xsl:call-template name="can-use-array"/>
    </xsl:variable>

    <xsl:call-template name="progress-message">
      <xsl:with-param name="m">
        <xsl:text>  .. </xsl:text>
        <xsl:value-of select="name"/>
      </xsl:with-param>
    </xsl:call-template>

    <xsl:call-template name="do-not-edit"/>
    <xsl:call-template name="identification-info"/>

    <!-- Commentary. -->
    <xsl:value-of select="$blank-line"/>
    <xsl:call-template name="commentary">
      <xsl:with-param name="separate-pars" select="$blank-line"/>
    </xsl:call-template>

    <!-- Suppress style checks. -->
    <xsl:value-of select="$blank-line"/>
    <xsl:text>pragma Style_Checks (Off);&#10;</xsl:text>

    <!-- Any context clauses needed for the class package .. -->
    <xsl:call-template name="class-spec-context"/>

    <!-- .. the class package .. -->
    <xsl:if test="not(@public or @visible)">
      <!-- only public and "visible" (for test support) packages are
           externally visible -->
      <xsl:text>private </xsl:text>
    </xsl:if>
    <xsl:text>package </xsl:text>
    <xsl:value-of select="../name"/>.<xsl:value-of select="name"/>
    <xsl:text> is&#10;</xsl:text>
    <xsl:value-of select="$blank-line"/>

    <!--  .. the Identifier record .. -->
    <xsl:if test="not(@singleton)">
        <xsl:call-template name="identifier-record"/>
        <xsl:value-of select="$blank-line"/>
    </xsl:if>

    <xsl:if test="not(@public)">

      <!-- .. the Instance record (indefinite, so it can't be
           allocated; limited, so people can't assign it) .. -->

      <xsl:value-of select="$I"/>
      <xsl:choose>
        <xsl:when test="statemachine">
          <xsl:text>type Instance (&lt;&gt;)&#10;</xsl:text>
          <xsl:value-of select="$I"/>
          <xsl:text>is new ColdFrame.Project.Events.Instance_Base with private;&#10;</xsl:text>
        </xsl:when>
        <xsl:otherwise>
          <xsl:text>type Instance (&lt;&gt;) is new ColdFrame.Instances.Instance_Base with private;&#10;</xsl:text>
        </xsl:otherwise>
      </xsl:choose>

      <!-- .. the Handle .. -->
      <xsl:value-of select="$I"/>
      <xsl:text>type Handle is access all Instance;&#10;</xsl:text>
      <xsl:value-of select="$blank-line"/>

   </xsl:if>

   <!-- .. the public creation and deletion operations .. -->
   <xsl:if test="not(@singleton)">

     <xsl:call-template name="create-function-spec"/>
     <xsl:value-of select="$blank-line"/>

     <xsl:value-of select="$I"/>
     <xsl:text>procedure Delete (With_Identifier : Identifier);&#10;</xsl:text>
     <xsl:value-of select="$blank-line"/>

     <xsl:value-of select="$I"/>
     <xsl:text>procedure Delete (This : in out Handle);&#10;</xsl:text>
     <xsl:value-of select="$blank-line"/>

   </xsl:if>

   <!-- .. the non-public singleton find operation .. -->
   <xsl:if test="@max=1 and not(@public)">
     <xsl:value-of select="$I"/>
     <xsl:text>function Find return Handle;&#10;</xsl:text>
     <xsl:value-of select="$blank-line"/>
   </xsl:if>

   <!-- .. the non-singleton find operation .. -->
   <xsl:if test="not(@singleton)">
     <xsl:value-of select="$I"/>
     <xsl:text>function Find (With_Identifier : Identifier) return Handle;&#10;</xsl:text>
     <xsl:value-of select="$blank-line"/>
   </xsl:if>

   <!-- .. subtype enumeration support, if required .. -->
   <xsl:if test="not(@singleton)">
     <xsl:call-template name="supertype-specs"/>
   </xsl:if>

   <!-- .. any access-to-subprogram types (before possible accessors) .. -->
   <xsl:apply-templates mode="access-to-operation"/>

   <!-- .. the attribute access operations .. -->
   <xsl:apply-templates mode="attribute-set-spec"/>
   <xsl:apply-templates mode="attribute-get-spec"/>

   <!-- .. state machine: event types .. -->
   <xsl:call-template name="event-type-specs"/>

   <!-- .. operations .. -->
   <xsl:call-template name="operation-specs"/>

   <!-- .. renaming operations .. -->
   <xsl:call-template name="renaming-operation-specs"/>

   <!-- .. the private part .. -->
   <xsl:text>private&#10;</xsl:text>
   <xsl:value-of select="$blank-line"/>

   <xsl:if test="@public">
     <xsl:value-of select="$I"/>
     <xsl:text>type Instance;&#10;</xsl:text>
     <xsl:value-of select="$I"/>
     <xsl:text>type Handle is access all Instance;&#10;</xsl:text>
     <xsl:value-of select="$blank-line"/>
   </xsl:if>

   <xsl:if test="@active">
     <xsl:call-template name="task-spec"/>
   </xsl:if>

   <xsl:if test="statemachine">
     <xsl:call-template name="state-machine-states"/>
   </xsl:if>

   <!-- .. the Instance record .. -->
   <xsl:call-template name="instance-record"/>
   <xsl:value-of select="$blank-line"/>

   <!-- .. the State_Image function spec .. -->
   <xsl:if test="statemachine">
     <xsl:call-template name="state-image-spec"/>
   </xsl:if>

   <!-- .. the actual container .. -->
   <xsl:choose>

     <xsl:when test="$max = 1">
       <!-- Only one possible instance .. -->

       <!-- .. fix the storage pool for Handle .. -->
       <!--
            use type System.Storage_Elements.Storage_Offset;
            Storage_Pool : ColdFrame.Project.Storage_Pools.Bounded_Pool
              (Pool_Size => Instance'Max_Size_In_Storage_Elements,
               Elmt_Size => Instance'Max_Size_In_Storage_Elements,
               Alignment => Instance'Alignment);
            for Handle'Storage_Pool use Storage_Pool;
            -->
       <xsl:value-of select="$I"/>
       <xsl:text>use type System.Storage_Elements.Storage_Offset;&#10;</xsl:text>
       <xsl:value-of select="$I"/>
       <xsl:text>Storage_Pool : ColdFrame.Project.Storage_Pools.Bounded_Pool&#10;</xsl:text>
       <xsl:value-of select="$IC"/>
       <xsl:text>(Pool_Size => Instance'Max_Size_In_Storage_Elements,&#10;</xsl:text>
       <xsl:value-of select="$IC"/>
       <xsl:text> Elmt_Size => Instance'Max_Size_In_Storage_Elements,&#10;</xsl:text>
       <xsl:value-of select="$IC"/>
       <xsl:text> Alignment => Instance'Alignment);&#10;</xsl:text>
       <xsl:value-of select="$I"/>
       <xsl:text>for Handle'Storage_Pool use Storage_Pool;&#10;</xsl:text>
       <xsl:value-of select="$blank-line"/>

       <!-- .. use a simple pointer .. -->
       <xsl:value-of select="$I"/>
       <xsl:text>This : Handle;&#10;</xsl:text>
       <xsl:value-of select="$blank-line"/>
     </xsl:when>

     <xsl:when test="$array='yes'">
       <!-- Use an array. -->

       <!-- Create the storage pool for Handle. -->
       <xsl:choose>

         <xsl:when test="$max &lt;= $max-bounded-container">
           <!--
                use type System.Storage_Elements.Storage_Offset;
                Storage_Pool : ColdFrame.Project.Storage_Pools.Bounded_Pool
                  (Pool_Size => Instance'Max_Size_In_Storage_Elements * {max},
                   Elmt_Size => Instance'Max_Size_In_Storage_Elements,
                   Alignment => Instance'Alignment);
                for Handle'Storage_Pool use Storage_Pool;
                -->
           <xsl:value-of select="$I"/>
           <xsl:text>use type System.Storage_Elements.Storage_Offset;&#10;</xsl:text>
           <xsl:value-of select="$I"/>
           <xsl:text>Storage_Pool : ColdFrame.Project.Storage_Pools.Bounded_Pool&#10;</xsl:text>
           <xsl:value-of select="$IC"/>
           <xsl:text>(Pool_Size => Instance'Max_Size_In_Storage_Elements * </xsl:text>
           <xsl:value-of select="$max"/>
           <xsl:text>,&#10;</xsl:text>
           <xsl:value-of select="$IC"/>
           <xsl:text> Elmt_Size => Instance'Max_Size_In_Storage_Elements,&#10;</xsl:text>
           <xsl:value-of select="$IC"/>
           <xsl:text> Alignment => Instance'Alignment);&#10;</xsl:text>
           <xsl:value-of select="$I"/>
           <xsl:text>for Handle'Storage_Pool use Storage_Pool;&#10;</xsl:text>
           <xsl:value-of select="$blank-line"/>
         </xsl:when>

         <!-- .. or use the standard pool ..-->
         <xsl:otherwise>
           <!--
                for Handle'Storage_Pool use ColdFrame.Project.Storage_Pools.Unbounded_Pool;
                -->
           <xsl:value-of select="$I"/>
           <xsl:text>for Handle'Storage_Pool use ColdFrame.Project.Storage_Pools.Unbounded_Pool;&#10;</xsl:text>
           <xsl:value-of select="$blank-line"/>
         </xsl:otherwise>

       </xsl:choose>

       <!-- The instance container.
            The_Container : array ({identifying-attribute-type}) of Handle;
            -->
       <xsl:value-of select="$I"/>
       <xsl:text>The_Container : array (</xsl:text>
       <xsl:value-of select="attribute[@identifier]/type"/>
       <xsl:text>) of Handle;&#10;</xsl:text>
       <xsl:value-of select="$blank-line"/>

     </xsl:when>

     <xsl:otherwise>
       <!-- Use a Map. -->

       <!-- .. the Instance_Identifier_Equality function spec .. -->
       <xsl:value-of select="$I"/>
       <xsl:text>function Instance_Identifier_Equality (L, R : Instance) return Boolean;&#10;</xsl:text>
       <xsl:value-of select="$blank-line"/>

       <!-- .. the Instance_Hash function spec .. -->
       <xsl:value-of select="$I"/>
       <xsl:text>function Instance_Hash (I : Instance) return Natural;&#10;</xsl:text>
       <xsl:value-of select="$blank-line"/>

       <xsl:choose>

         <xsl:when test="$max &lt;= $max-bounded-container">
           <!--
                use type System.Storage_Elements.Storage_Offset;
                Storage_Pool : ColdFrame.Project.Storage_Pools.Bounded_Pool
                  (Pool_Size => Instance'Max_Size_In_Storage_Elements * {max},
                   Elmt_Size => Instance'Max_Size_In_Storage_Elements,
                   Alignment => Instance'Alignment);
                for Handle'Storage_Pool use Storage_Pool;
                -->
           <xsl:value-of select="$I"/>
           <xsl:text>use type System.Storage_Elements.Storage_Offset;&#10;</xsl:text>
           <xsl:value-of select="$I"/>
           <xsl:text>Storage_Pool : ColdFrame.Project.Storage_Pools.Bounded_Pool&#10;</xsl:text>
           <xsl:value-of select="$IC"/>
           <xsl:text>(Pool_Size => Instance'Max_Size_In_Storage_Elements * </xsl:text>
           <xsl:value-of select="$max"/>
           <xsl:text>,&#10;</xsl:text>
           <xsl:value-of select="$IC"/>
           <xsl:text> Elmt_Size => Instance'Max_Size_In_Storage_Elements,&#10;</xsl:text>
           <xsl:value-of select="$IC"/>
           <xsl:text> Alignment => Instance'Alignment);&#10;</xsl:text>
           <xsl:value-of select="$I"/>
           <xsl:text>for Handle'Storage_Pool use Storage_Pool;&#10;</xsl:text>
           <xsl:value-of select="$blank-line"/>
         </xsl:when>

         <!-- .. or use the standard pool ..-->
         <xsl:otherwise>
           <!--
                for Handle'Storage_Pool use ColdFrame.Project.Storage_Pools.Unbounded_Pool;
                -->
           <xsl:value-of select="$I"/>
           <xsl:text>for Handle'Storage_Pool use ColdFrame.Project.Storage_Pools.Unbounded_Pool;&#10;</xsl:text>
           <xsl:value-of select="$blank-line"/>
         </xsl:otherwise>

       </xsl:choose>

       <!-- .. the instance container .. -->
       <xsl:choose>

         <xsl:when test="$max &lt;= $max-bounded-container">
           <!-- Wnen the size isn't too big, use the Bounded version -->
           <xsl:value-of select="$I"/>
           <xsl:text>package Maps renames ColdFrame.Instances.Bounded_Maps;&#10;</xsl:text>
           <xsl:value-of select="$blank-line"/>
           <xsl:value-of select="$I"/>
           <xsl:text>The_Container : Maps.Unconstrained_Map&#10;</xsl:text>
           <xsl:value-of select="$IC"/>
           <xsl:text>(Number_Of_Buckets =&gt; </xsl:text>
           <xsl:call-template name="hash-buckets"/>
           <xsl:text>,&#10;</xsl:text>
           <xsl:value-of select="$IC"/>
           <xsl:text> Maximum_Size =&gt; </xsl:text>
           <xsl:value-of select="$max"/>
           <xsl:text>);&#10;</xsl:text>
           <xsl:value-of select="$blank-line"/>
         </xsl:when>

         <xsl:otherwise>
           <!-- Use the Unbounded version -->
           <xsl:value-of select="$I"/>
           <xsl:text>package Maps renames ColdFrame.Instances.Unbounded_Maps;&#10;</xsl:text>
           <xsl:value-of select="$blank-line"/>
           <xsl:value-of select="$I"/>
           <xsl:text>The_Container : Maps.Unconstrained_Map&#10;</xsl:text>

           <xsl:value-of select="$IC"/>
           <xsl:text>(Number_Of_Buckets =&gt; </xsl:text>
           <xsl:call-template name="hash-buckets"/>
           <xsl:text>);&#10;</xsl:text>
           <xsl:value-of select="$blank-line"/>
         </xsl:otherwise>

       </xsl:choose>

     </xsl:otherwise>

   </xsl:choose>

   <!-- .. private Create, Delete operations for singletons ..-->
   <xsl:if test="@singleton">

     <xsl:call-template name="create-function-spec"/>
     <xsl:value-of select="$blank-line"/>

     <xsl:value-of select="$I"/>
     <xsl:text>procedure Delete (This : in out Handle);&#10;</xsl:text>
     <xsl:value-of select="$blank-line"/>

    </xsl:if>

    <!-- .. event handlers .. -->
    <xsl:apply-templates mode="event-handler-specs" select="event">
      <xsl:sort select="name"/>
    </xsl:apply-templates>

    <!-- .. Autonumber support .. -->
    <xsl:if test="count(attribute[@identifier])=1
                  and attribute[@identifier]/type='Autonumber'">
      <xsl:value-of select="$I"/>
      <xsl:text>Next_Identifier : Long_Long_Integer := 0;&#10;</xsl:text>
      <xsl:value-of select="$blank-line"/>
    </xsl:if>

    <!-- .. any <<class>> attributes .. -->
    <xsl:if test="attribute/@class">
      <xsl:for-each select="attribute[@class]">
        <xsl:call-template name="single-record-component">
          <xsl:with-param name="indent" select="$I"/>
        </xsl:call-template>
      </xsl:for-each>
      <xsl:value-of select="$blank-line"/>
    </xsl:if>

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

        <!-- Calculate the maximum number of instances. -->
        <xsl:variable name="max">
          <xsl:call-template name="number-of-instances"/>
        </xsl:variable>

        <!-- Determine whether an array can be used. -->
        <xsl:variable name="array">
          <xsl:call-template name="can-use-array"/>
        </xsl:variable>

        <!-- Always need storage management. -->
        <xsl:text>with ColdFrame.Project.Storage_Pools;&#10;</xsl:text>
        <!-- Need storage offset arithmetic for bounded classes. -->
        <xsl:if test="$max &lt;= $max-bounded-container">
          <xsl:text>with System.Storage_Elements;&#10;</xsl:text>
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

        <!-- Check for Calendar. -->
        <xsl:if test="attribute/type='Date'
                      or $ancestors/operation/parameter/type='Date'
                      or $ancestors/operation/@return='Date'
                      or attribute/type='Time'
                      or $ancestors/operation/parameter/type='Time'
                      or $ancestors/operation/@return='Time'">
          <xsl:text>with ColdFrame.Project.Calendar;</xsl:text>
          <xsl:text> use type ColdFrame.Project.Calendar.Time;&#10;</xsl:text>
        </xsl:if>

        <!-- If this class has any events at all, or a statemachine
             (presumably without events!), or any Timers, include
             event support. -->
        <xsl:if test="event or statemachine or attribute/type='Timer'">
          <xsl:text>with ColdFrame.Project.Events;&#10;</xsl:text>
        </xsl:if>

        <!-- If the maximum numer of instances is more than 1 and we aren't
             using an array (so that a Map is needed), or if there are
             attributes/operations involving _other_ classes, or the special
             Counterpart, or this is the parent in an inheritance
             relationship, need support for standard Instances as well. -->
        <xsl:variable name="counterpart">
          <!-- Need an element to make a nodeset next. -->
          <xsl:element name="name">Counterpart</xsl:element>
        </xsl:variable>
        <xsl:variable
          name="other-classes"
          select="$counterpart/name
                  | /domain/class[name != current()/name]/name"/>
        <xsl:if test="($max &gt; 1 and $array = 'no')
                      or not(statemachine)
                      or attribute[type=$other-classes]
                      or attribute[@refers=$other-classes]
                      or operation/parameter[type=$other-classes]
                      or operation[@return=$other-classes]
                      or name=../inheritance/parent">
          <xsl:text>with ColdFrame.Instances;&#10;</xsl:text>
        </xsl:if>

        <xsl:if test="@active">

          <!-- Need Ada.Unchecked_Deallocation. -->
          <xsl:text>with Ada.Unchecked_Deallocation;&#10;</xsl:text>

          <!-- If the (active) class has a priority specified, need System. -->
          <xsl:if test="@priority">
            <xsl:text>with System;&#10;</xsl:text>
          </xsl:if>

        </xsl:if>

      </xsl:otherwise>

    </xsl:choose>

  </xsl:template>


  <!-- Called at domain/class to generate any required supertype spec
       information -->
  <xsl:template name="supertype-specs">

    <xsl:variable name="parent-name" select="name"/>

    <xsl:for-each select="../inheritance[parent=$parent-name]">
      <xsl:sort select="name"/>

      <xsl:call-template name="subtype-enumeration"/>
      <xsl:value-of select="$blank-line"/>

      <xsl:call-template name="subtype-selection"/>
      <xsl:value-of select="$blank-line"/>

      <xsl:value-of select="$I"/>
      <xsl:text>--  Private use only&#10;</xsl:text>
      <xsl:value-of select="$I"/>
      <xsl:text>procedure Set_</xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text>_Child&#10;</xsl:text>
      <xsl:value-of select="$IC"/>
      <xsl:text>(This : Handle;&#10;</xsl:text>
      <xsl:value-of select="$IC"/>
      <xsl:text> To_Be : </xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text>_Child);&#10;</xsl:text>

      <!--
      <xsl:value-of select="$I"/>
      <xsl:text>pragma Inline_Always (Set_</xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text>_Child);&#10;</xsl:text>
      -->
      <xsl:value-of select="$blank-line"/>

      <xsl:value-of select="$I"/>
      <xsl:text>--  Consider using dispatching operations instead&#10;</xsl:text>
      <xsl:value-of select="$I"/>
      <xsl:text>function Get_</xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text>_Child&#10;</xsl:text>
      <xsl:value-of select="$IC"/>
      <xsl:text>(This : Handle)&#10;</xsl:text>
      <xsl:value-of select="$IC"/>
      <xsl:text>return </xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text>_Child;&#10;</xsl:text>
      <!-- Can't inline function returning unconstrained type. -->
      <xsl:value-of select="$blank-line"/>

    </xsl:for-each>

  </xsl:template>


  <!-- Called at domain/class to generate any required supertype body
       information -->
  <xsl:template name="supertype-bodies">

    <xsl:variable name="parent-name" select="name"/>

    <xsl:for-each select="../inheritance[parent=$parent-name]">
      <xsl:sort select="name"/>

      <!--
           procedure Set_{rel}_Child
             (This : Handle;
              To_Be : {rel}_Child) is
           begin
           if To_Be.Current /= Null_T
             and then This.{rel}_Current_Child.Current /= Null_T then
                 raise ColdFrame.Exceptions.Existing_Child;
              end if;
              This.{rel}_Current_Child := To_Be;
           end Set_{rel}_Child;
           -->

      <xsl:value-of select="$I"/>
      <xsl:text>procedure Set_</xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text>_Child&#10;</xsl:text>
      <xsl:value-of select="$IC"/>
      <xsl:text>(This : Handle;&#10;</xsl:text>
      <xsl:value-of select="$IC"/>
      <xsl:text> To_Be : </xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text>_Child) is&#10;</xsl:text>
      <xsl:value-of select="$I"/>
      <xsl:text>begin&#10;</xsl:text>
      <xsl:value-of select="$II"/>
      <xsl:text>if To_Be.Current /= Null_T&#10;</xsl:text>
      <xsl:value-of select="$IIC"/>
      <xsl:text>and then This.</xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text>_Current_Child.Current /= Null_T then&#10;</xsl:text>
      <xsl:value-of select="$III"/>
      <xsl:text>raise ColdFrame.Exceptions.Existing_Child;&#10;</xsl:text>
      <xsl:value-of select="$II"/>
      <xsl:text>end if;&#10;</xsl:text>
      <xsl:value-of select="$II"/>
      <xsl:text>This.</xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text>_Current_Child := To_Be;&#10;</xsl:text>
      <xsl:value-of select="$I"/>
      <xsl:text>end Set_</xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text>_Child;&#10;</xsl:text>
      <xsl:value-of select="$blank-line"/>

      <!--
           function Get_{rel}_Child
             (This : Handle)
             return {rel}_Child is
           begin
              return This.{rel}_Current_Child;
           end Get_{rel}_Child;
           -->

      <xsl:value-of select="$I"/>
      <xsl:text>function Get_</xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text>_Child&#10;</xsl:text>
      <xsl:value-of select="$IC"/>
      <xsl:text>(This : Handle)&#10;</xsl:text>
      <xsl:value-of select="$IC"/>
      <xsl:text>return </xsl:text>
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
      <xsl:value-of select="$blank-line"/>

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
    <xsl:text>_Child&#10;</xsl:text>
    <xsl:value-of select="$IC"/>
    <xsl:text>(Current : </xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>_Child_Class := Null_T)&#10;</xsl:text>
    <xsl:value-of select="$I"/>
    <xsl:text>is record&#10;</xsl:text>
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
      <xsl:text> : ColdFrame.Instances.Handle;&#10;</xsl:text>
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

    <xsl:call-template name="progress-message">
      <xsl:with-param name="m">
        <xsl:text>  .. </xsl:text>
        <xsl:value-of select="name"/>
      </xsl:with-param>
    </xsl:call-template>

    <!-- Calculate the maximum number of instances. -->
    <xsl:variable name="max">
      <xsl:call-template name="number-of-instances"/>
    </xsl:variable>

    <!-- Determine whether an array can be used. -->
    <xsl:variable name="array">
      <xsl:call-template name="can-use-array"/>
    </xsl:variable>

    <xsl:call-template name="do-not-edit"/>
    <xsl:call-template name="identification-info"/>

    <!-- Suppress style checks. -->
    <xsl:value-of select="$blank-line"/>
    <xsl:text>pragma Style_Checks (Off);&#10;</xsl:text>

    <!-- Any context clauses needed for the class body .. -->
    <xsl:call-template name="class-body-context"/>

    <!-- .. start the body .. -->
    <xsl:text>package body </xsl:text>
    <xsl:value-of select="../name"/>.<xsl:value-of select="name"/>
    <xsl:text> is&#10;</xsl:text>
    <xsl:value-of select="$blank-line"/>

    <!-- .. the task stub for active classes .. -->
    <xsl:if test="@active">
      <xsl:value-of select="$I"/>
      <xsl:text>pragma Style_Checks (On);&#10;</xsl:text>
      <xsl:value-of select="$I"/>
      <xsl:text>task body T is separate;&#10;</xsl:text>
      <xsl:value-of select="$I"/>
      <xsl:text>pragma Style_Checks (Off);&#10;</xsl:text>
      <xsl:value-of select="$blank-line"/>
    </xsl:if>

    <!-- .. the set-the-identifier operation .. -->
    <!-- XXX what's the logic here? -->
    <xsl:if test="not(@singleton) and
                  ($max &gt; 1 or
                   count(attribute[@identifier]) &gt; 1 or
                   not (attribute[@identifier]/type = 'Autonumber'))">
      <xsl:call-template name="set-identifier-procedure"/>
      <xsl:value-of select="$blank-line"/>
    </xsl:if>

    <!-- .. the creation and deletion operations .. -->
    <xsl:call-template name="create-function-body"/>
    <xsl:value-of select="$blank-line"/>

    <xsl:value-of select="$I"/>
    <xsl:text>procedure Free is new Ada.Unchecked_Deallocation (Instance, Handle);&#10;</xsl:text>
    <xsl:value-of select="$blank-line"/>

    <xsl:if test="not(@singleton)">
      <xsl:call-template name="class-delete-procedure-body"/>
      <xsl:value-of select="$blank-line"/>
    </xsl:if>

    <xsl:call-template name="delete-procedure-body"/>
    <xsl:value-of select="$blank-line"/>

    <!-- .. the find operations .. -->
    <xsl:if test="@max=1 and not(@public)">
      <xsl:call-template name="find-single-instance-function-body"/>
      <xsl:value-of select="$blank-line"/>
    </xsl:if>

   <xsl:if test="not(@singleton)">
      <xsl:call-template name="find-function-body"/>
      <xsl:value-of select="$blank-line"/>
    </xsl:if>

    <!-- .. subtype enumeration support, if required .. -->
    <xsl:call-template name="supertype-bodies"/>

    <!-- .. attribute accessors .. -->
    <xsl:apply-templates mode="attribute-set-body"/>
    <xsl:apply-templates mode="attribute-get-body"/>

    <xsl:if test="not(@singleton)">

      <xsl:if test="$max &gt; 1 and $array = 'no'">

        <!-- .. the Instance_Identifier_Equality function body .. -->
        <xsl:call-template name="instance-identifier-equality-body"/>

        <!-- .. the hash function stub .. -->
        <xsl:value-of select="$I"/>
        <xsl:text>pragma Style_Checks (On);&#10;</xsl:text>
        <xsl:value-of select="$I"/>
        <xsl:text>function Instance_Hash (I : Instance) return Natural is separate;&#10;</xsl:text>
        <xsl:value-of select="$I"/>
        <xsl:text>pragma Style_Checks (Off);&#10;</xsl:text>
        <xsl:value-of select="$blank-line"/>

      </xsl:if>

    </xsl:if>

    <!-- .. operation stubs .. -->
    <xsl:call-template name="operation-body-stubs"/>

    <!-- .. state image body .. -->
    <xsl:if test="statemachine">
      <xsl:call-template name="state-image-body"/>
    </xsl:if>

    <!-- .. <<message>> event handler bodies .. -->
    <xsl:apply-templates mode="event-handler-bodies" select="event[@class]">
      <xsl:sort select="name"/>
    </xsl:apply-templates>

    <!-- .. <<event>> event handler bodies .. -->
    <xsl:apply-templates
      mode="event-handler-bodies"
      select="statemachine/event">
      <xsl:sort select="name"/>
    </xsl:apply-templates>

    <!-- and close. -->
    <xsl:text>end </xsl:text>
    <xsl:value-of select="../name"/>.<xsl:value-of select="name"/>
    <xsl:text>;&#10;</xsl:text>

    <xsl:if test="$max &gt; 1 and $array = 'no'">

      <!-- Output the separate hash function body. -->
      <xsl:call-template name="hash-function-body"/>

    </xsl:if>

    <xsl:if test="@active">

      <!-- Output the separate task body. -->

      <xsl:call-template name="should-edit"/>
      <xsl:call-template name="identification-info"/>
      <xsl:value-of select="$blank-line"/>
      <xsl:text>separate (</xsl:text>
      <xsl:value-of select="../name"/>.<xsl:value-of select="name"/>
      <xsl:text>)&#10;</xsl:text>
      <xsl:text>task body T is&#10;</xsl:text>
      <xsl:text>begin&#10;</xsl:text>
      <xsl:value-of select="$I"/>
      <xsl:text>raise Program_Error;&#10;</xsl:text>
      <xsl:text>end T;&#10;</xsl:text>
    </xsl:if>

    <!-- Separate subprogram bodies for individual operations. -->
    <xsl:call-template name="operation-separate-bodies"/>

  </xsl:template>

  <xsl:template mode="class-body" match="*"/>


  <!-- Called from domain/class to generate context clauses for package
       body. -->
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

        <!-- Calculate the maximum number of instances. -->
        <xsl:variable name="max">
          <xsl:call-template name="number-of-instances"/>
        </xsl:variable>

        <!-- Determine whether an array can be used. -->
        <xsl:variable name="array">
          <xsl:call-template name="can-use-array"/>
        </xsl:variable>

        <!-- We'll need to free memory. -->
        <xsl:text>with Ada.Unchecked_Deallocation;&#10;</xsl:text>

        <!-- We always need Coldframe exceptions.
             We need BC exceptions if we're using a Map. -->
        <xsl:if test="$max &gt; 1 and $array = 'no'">
          <xsl:text>with BC;&#10;</xsl:text>
        </xsl:if>
        <xsl:text>with ColdFrame.Exceptions;&#10;</xsl:text>

        <!-- Any additions to the context for state machines. -->
        <xsl:call-template name="state-body-context"/>

        <xsl:variable name="current" select="."/>
        <xsl:variable name="name" select="name"/>

        <!-- We want to output only one "with" for each package.
             The strategy is to sort the list pf packages and only
             output unique members. To do this, we need a nodeset. -->

        <xsl:variable name="withs">

          <!-- Withs for referential attributes of the current class -->
          <xsl:for-each
            select="attribute[@refers and not(@refers=$name)]">
            <xsl:element name="with">
              <xsl:value-of select="@refers"/>
            </xsl:element>
          </xsl:for-each>

          <!-- Withs for subprograms of this and ancestor classes.
               We only want classes (not including the current class). -->
          <xsl:for-each select="$ancestors/operation/parameter/type
                                | $ancestors/operation/@return">
            <xsl:if test="/domain/class/name=. and not(.=$name)">
              <xsl:element name="with">
                <xsl:value-of select="."/>
              </xsl:element>
            </xsl:if>
          </xsl:for-each>

          <!-- Withs for child classes. Needed for the subtype selection
               record. -->
          <xsl:for-each select="../inheritance[parent=$name]/child">
            <xsl:element name="with">
              <xsl:value-of select="."/>
            </xsl:element>
          </xsl:for-each>

        </xsl:variable>

        <xsl:variable name="d" select="/domain/name"/>

        <!-- Sort, uniqueify and output. -->
        <!-- XXX Saxon 6.5.1 allows this result tree fragment to be
             implicitly converted to a node set if the version is 1.1,
             so I've changed this file to require 1.1.
             Should consider saxon:node-set() or exsl:node-set(). -->
        <xsl:for-each select="$withs/with">
          <xsl:sort/>
          <xsl:if test="not (.=preceding-sibling::node())">
            <xsl:text>with </xsl:text>
            <xsl:value-of select="$d"/>
            <xsl:text>.</xsl:text>
            <xsl:value-of select="."/>
            <xsl:text>;&#10;</xsl:text>
          </xsl:if>
        </xsl:for-each>

      </xsl:otherwise>

    </xsl:choose>

  </xsl:template>


  <!-- Called to generate the Set_Identifier procedure . -->
  <xsl:template name="set-identifier-procedure">
    <xsl:value-of select="$I"/>
    <xsl:text>procedure Set_Identifier (H : Handle; With_Identifier : Identifier);&#10;</xsl:text>
    <xsl:value-of select="$I"/>
    <xsl:text>pragma Inline_Always (Set_Identifier);&#10;</xsl:text>
    <xsl:value-of select="$I"/>
    <xsl:text>procedure Set_Identifier (H : Handle; With_Identifier : Identifier) is&#10;</xsl:text>
    <xsl:value-of select="$I"/>
    <xsl:text>begin&#10;</xsl:text>

    <xsl:for-each select="attribute[@identifier]">
      <xsl:value-of select="$II"/>
      <xsl:text>H.</xsl:text>
      <xsl:call-template name="attribute-name"/>
      <xsl:text>&#10;</xsl:text>
      <xsl:value-of select="$IIC"/>
      <xsl:text>:= With_Identifier.</xsl:text>
      <xsl:call-template name="attribute-name"/>
      <xsl:text>;&#10;</xsl:text>
    </xsl:for-each>

    <xsl:value-of select="$I"/>
    <xsl:text>end Set_Identifier;&#10;</xsl:text>
  </xsl:template>


  <!-- Called to assign values to attributes on creation (identifier
       attributes only) -->
  <xsl:template
    match="attribute[@identifier]"
    mode="identifier-element-assignment">
    <!--
         Result.{attr}
         := With_Identifier.{attr};
         -->

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


  <!-- Called from domain/class to generate the Create function spec. -->
  <xsl:template name="create-function-spec">

    <xsl:if test="../association/associative=current()/name">
      <xsl:value-of select="$I"/>
      <xsl:text>--  Private use only, use Link&#10;</xsl:text>
    </xsl:if>

    <xsl:choose>

      <xsl:when test="@singleton">
        <xsl:value-of select="$I"/>
        <xsl:text>function Create return Handle;&#10;</xsl:text>
      </xsl:when>

      <xsl:when test="count(attribute[@identifier])=1
                      and attribute[@identifier]/type='Autonumber'">
        <xsl:value-of select="$I"/>
        <xsl:text>function Create return Handle;&#10;</xsl:text>
      </xsl:when>

      <xsl:otherwise>
        <xsl:if test="attribute/type='Autonumber'">
          <xsl:call-template name="log-error"/>
          <xsl:message>
            <xsl:text>Error: invalid use of Autonumber in </xsl:text>
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

    <!-- Calculate the maximum number of instances. -->
    <xsl:variable name="max">
      <xsl:call-template name="number-of-instances"/>
    </xsl:variable>

    <!-- Determine whether an array can be used. -->
    <xsl:variable name="array">
      <xsl:call-template name="can-use-array"/>
    </xsl:variable>

    <!-- The heading .. -->
    <xsl:choose>

      <xsl:when test="@singleton">
        <xsl:value-of select="$I"/>
        <xsl:text>function Create return Handle is&#10;</xsl:text>
      </xsl:when>

      <xsl:when test="count(attribute[@identifier])=1
                      and attribute[@identifier]/type='Autonumber'">
        <xsl:value-of select="$I"/>
        <xsl:text>function Create return Handle is&#10;</xsl:text>
      </xsl:when>

      <xsl:otherwise>
        <xsl:value-of select="$I"/>
        <xsl:text>function Create (With_Identifier : Identifier) return Handle is&#10;</xsl:text>
      </xsl:otherwise>

    </xsl:choose>

    <!-- .. check for domain initialization, for non-singletons .. -->
    <xsl:if test="not(@singleton)">
      <xsl:value-of select="$II"/>
      <xsl:text>pragma Assert&#10;</xsl:text>
      <xsl:value-of select="$IIC"/>
      <xsl:text>(Domain_Initialized,&#10;</xsl:text>
      <xsl:value-of select="$IIC"/>
      <xsl:text> "</xsl:text>
      <xsl:value-of select="../name"/>
      <xsl:text> not initialized");&#10;</xsl:text>
    </xsl:if>

    <!-- .. the result .. -->
    <xsl:value-of select="$II"/>
    <xsl:text>Result : Handle;&#10;</xsl:text>

    <!-- .. check that referential attributes are non-null and of the
         correct type .. -->
    <xsl:if test="attribute[@identifier and @refers]">

      <!--
           use type ColdFrame.Instances.Handle;
           pragma Assert
             (With_Identifier.{attr} /= null);
           pragma Assert
             (With_Identifier.{attr}.all
              in {attr-class}.Instance'Class);
           -->

      <xsl:value-of select="$II"/>
      <xsl:text>use type ColdFrame.Instances.Handle;&#10;</xsl:text>
      <xsl:for-each select="attribute[@identifier and @refers]">
        <xsl:value-of select="$II"/>
        <xsl:text>pragma Assert&#10;</xsl:text>
        <xsl:value-of select="$IIC"/>
        <xsl:text>(With_Identifier.</xsl:text>
        <xsl:call-template name="attribute-name"/>
        <xsl:text> /= null);&#10;</xsl:text>

        <xsl:value-of select="$II"/>
        <xsl:text>pragma Assert&#10;</xsl:text>
        <xsl:value-of select="$IIC"/>
        <xsl:text>(With_Identifier.</xsl:text>
        <xsl:call-template name="attribute-name"/>
        <xsl:text>.all&#10;</xsl:text>
        <xsl:value-of select="$IIC"/>
        <xsl:text> in </xsl:text>
        <xsl:value-of select="@refers"/>
        <xsl:text>.Instance'Class);&#10;</xsl:text>

      </xsl:for-each>
    </xsl:if>

    <xsl:value-of select="$I"/>
    <xsl:text>begin&#10;</xsl:text>

    <!-- .. Create the new instance .. -->
    <xsl:value-of select="$II"/>
    <xsl:text>Result := new Instance;&#10;</xsl:text>

    <!-- .. set up identifying attributes .. -->
    <xsl:choose>

      <xsl:when test="@singleton"/>

      <xsl:when test="count(attribute[@identifier])=1
                      and attribute[@identifier]/type='Autonumber'">
        <xsl:variable name="id" select="attribute[@identifier]/name"/>
        <xsl:value-of select="$II"/>
        <xsl:text>Result.</xsl:text>
        <xsl:value-of select="$id"/>
        <xsl:text> := Next_Identifier;&#10;</xsl:text>
        <xsl:value-of select="$II"/>
        <xsl:text>Next_Identifier := Next_Identifier + 1;&#10;</xsl:text>
      </xsl:when>

      <xsl:otherwise>
        <xsl:value-of select="$II"/>
        <xsl:text>Set_Identifier (Result, With_Identifier);&#10;</xsl:text>
      </xsl:otherwise>

    </xsl:choose>

    <!-- .. initialize inheritance .. -->
    <xsl:call-template name="set-parent-child-info">
      <xsl:with-param name="handle" select="'Result'"/>
    </xsl:call-template>

    <!-- .. store the new instance .. -->
    <xsl:choose>

      <xsl:when test="$max=1">
        <!-- No need to check for This not being null, we'd have already
             had Storage_Error. -->
        <xsl:value-of select="$II"/>
        <xsl:text>This := Result;&#10;</xsl:text>
      </xsl:when>

      <xsl:when test="$array='yes'">
        <!-- This test could be done earlier, but it's in the same place
             as the equivalent in the Map case. -->
        <xsl:value-of select="$II"/>
        <xsl:text>if The_Container (With_Identifier.</xsl:text>
        <xsl:value-of select="attribute[@identifier]/name"/>
        <xsl:text>) /= null then&#10;</xsl:text>
        <xsl:value-of select="$III"/>
        <xsl:text>raise ColdFrame.Exceptions.Duplicate;&#10;</xsl:text>
        <xsl:value-of select="$II"/>
        <xsl:text>end if;&#10;</xsl:text>
        <xsl:value-of select="$II"/>
        <xsl:text>The_Container (With_Identifier.</xsl:text>
        <xsl:value-of select="attribute[@identifier]/name"/>
        <xsl:text>) := Result;&#10;</xsl:text>
      </xsl:when>

      <xsl:otherwise>
        <xsl:value-of select="$II"/>
        <xsl:text>Maps.Bind&#10;</xsl:text>
        <xsl:value-of select="$IIC"/>
        <xsl:text>(The_Container,&#10;</xsl:text>
        <xsl:value-of select="$IIC"/>
        <xsl:text> ColdFrame.Instances.Handle (Result),&#10;</xsl:text>
        <xsl:value-of select="$IIC"/>
        <xsl:text> ColdFrame.Instances.Handle (Result));&#10;</xsl:text>
      </xsl:otherwise>

    </xsl:choose>

    <xsl:if test="@active">
      <!-- .. allocate the task .. -->
      <xsl:value-of select="$II"/>
      <xsl:text>Result.The_T := new T (Result);&#10;</xsl:text>
    </xsl:if>

    <!-- .. initialize state machine .. -->
    <!-- (after we've stored the new instance, in case it's a singleton,
         which means that Enter_{next-state} requires This to be set up) -->
    <xsl:call-template name="initialize-state-machine"/>

    <!-- .. return it .. -->
    <xsl:value-of select="$II"/>
    <xsl:text>return Result;&#10;</xsl:text>


    <!-- .. handle exceptions .. -->
    <xsl:if test="$max &gt; 1 and $array='no'">
      <xsl:value-of select="$I"/>
      <xsl:text>exception&#10;</xsl:text>
      <xsl:value-of select="$II"/>
      <xsl:text>when BC.Duplicate =&gt; raise ColdFrame.Exceptions.Duplicate;&#10;</xsl:text>
    </xsl:if>


    <!-- .. and close. -->
    <xsl:value-of select="$I"/>
    <xsl:text>end Create;&#10;</xsl:text>


  </xsl:template>


  <!-- Called from domain/class, within the Create function, to
       set the parents' Current Child record. -->
  <xsl:template name="set-parent-child-info">
    <xsl:param name="handle" select="'set-parent-child-info-handle'"/>

    <!--
         {parent}.Set_{relation}_Child
           ({parent}.Handle ({handle}.{relation}_Parent),
            (Current => {parent}.{child}_T,
             {abbrev} => ColdFrame.Instances.Handle ({handle})));
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
      <xsl:value-of select="parent"/>
      <xsl:text>.Handle (</xsl:text>
      <xsl:value-of select="$handle"/>
      <xsl:text>.</xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text>_Parent),&#10;</xsl:text>
      <xsl:value-of select="$IIC"/>
      <xsl:text> (Current =&gt; </xsl:text>
      <xsl:value-of select="parent"/>
      <xsl:text>.</xsl:text>
      <xsl:value-of select="$current/name"/>
      <xsl:text>_T,&#10;</xsl:text>
      <xsl:value-of select="$IIC"/>
      <xsl:text>  </xsl:text>
      <xsl:value-of select="$current/abbreviation"/>
      <xsl:text> =&gt; ColdFrame.Instances.Handle (</xsl:text>
      <xsl:value-of select="$handle"/>
      <xsl:text>)));&#10;</xsl:text>
    </xsl:for-each>

  </xsl:template>


  <!-- Called from domain/class to create the class delete
       procedure body.
       Will never be called for singletons. -->
  <xsl:template name="class-delete-procedure-body">

    <!-- Calculate the maximum number of instances. -->
    <xsl:variable name="max">
      <xsl:call-template name="number-of-instances"/>
    </xsl:variable>

    <!-- Determine whether an array can be used. -->
    <xsl:variable name="array">
      <xsl:call-template name="can-use-array"/>
    </xsl:variable>

    <xsl:value-of select="$I"/>
    <xsl:text>procedure Delete (With_Identifier : Identifier) is&#10;</xsl:text>

    <xsl:if test="$max &gt; 1">
      <xsl:value-of select="$II"/>
      <xsl:text>This : Handle;&#10;</xsl:text>
    </xsl:if>

    <xsl:choose>

      <xsl:when test="$max = 1">

        <xsl:value-of select="$I"/>
        <xsl:text>begin&#10;</xsl:text>

        <!-- Check there is an instance. -->
        <xsl:value-of select="$II"/>
        <xsl:text>if This = null then&#10;</xsl:text>
        <xsl:value-of select="$III"/>
        <xsl:text>raise ColdFrame.Exceptions.Not_Found;&#10;</xsl:text>
        <xsl:value-of select="$II"/>
        <xsl:text>end if;&#10;</xsl:text>

        <!-- Check the ID is correct -->
        <!--
             if ({attr} => This.{attr},
                 {attr} => This.{attr}) /= With_Identifier then
                raise ColdFrame.Exceptions.Not_Found;
             end if;
             -->
        <xsl:value-of select="$II"/>
        <xsl:text>if (</xsl:text>
        <xsl:for-each select="attribute[@identifier]">
          <xsl:call-template name="attribute-name"/>
          <xsl:text> =&gt; This.</xsl:text>
          <xsl:call-template name="attribute-name"/>
          <xsl:if test="position() &lt; last()">
            <xsl:text>,&#10;    </xsl:text>
            <xsl:value-of select="$II"/>
          </xsl:if>
        </xsl:for-each>
        <xsl:text>) /= With_Identifier then&#10;</xsl:text>
        <xsl:value-of select="$III"/>
        <xsl:text>raise ColdFrame.Exceptions.Not_Found;&#10;</xsl:text>
        <xsl:value-of select="$II"/>
        <xsl:text>end if;&#10;</xsl:text>

      </xsl:when>

      <xsl:when test="$array='yes'">

        <xsl:value-of select="$I"/>
        <xsl:text>begin&#10;</xsl:text>

        <xsl:value-of select="$II"/>
        <xsl:text>This := The_Container (With_Identifier.</xsl:text>
        <xsl:value-of select="attribute[@identifier]/name"/>
        <xsl:text>);&#10;</xsl:text>
        <xsl:value-of select="$II"/>
        <xsl:text>if This = null then&#10;</xsl:text>
        <xsl:value-of select="$III"/>
        <xsl:text>raise ColdFrame.Exceptions.Not_Found;&#10;</xsl:text>
        <xsl:value-of select="$II"/>
        <xsl:text>end if;&#10;</xsl:text>

      </xsl:when>

      <xsl:otherwise>

        <xsl:value-of select="$II"/>
        <xsl:text>Key : aliased Instance;&#10;</xsl:text>

        <xsl:value-of select="$I"/>
        <xsl:text>begin&#10;</xsl:text>

        <xsl:value-of select="$II"/>
        <xsl:text>Set_Identifier (Key'Unchecked_Access, With_Identifier);&#10;</xsl:text>

        <xsl:value-of select="$II"/>
        <xsl:text>if not Maps.Is_Bound (The_Container, Key'Unchecked_Access) then&#10;</xsl:text>
        <xsl:value-of select="$III"/>
        <xsl:text>raise ColdFrame.Exceptions.Not_Found;&#10;</xsl:text>
        <xsl:value-of select="$II"/>
        <xsl:text>end if;&#10;</xsl:text>

        <xsl:value-of select="$II"/>
        <xsl:text>This := Handle (Maps.Item_Of (The_Container, Key'Unchecked_Access));&#10;</xsl:text>

      </xsl:otherwise>

    </xsl:choose>

    <xsl:call-template name="subtype-deletion">
      <xsl:with-param name="handle" select="'This'"/>
    </xsl:call-template>
    <xsl:call-template name="perform-finalization">
      <xsl:with-param name="handle" select="'This'"/>
    </xsl:call-template>
    <xsl:call-template name="clear-parent-child-info">
      <xsl:with-param name="handle" select="'This'"/>
    </xsl:call-template>

    <xsl:if test="@active">
      <xsl:value-of select="$II"/>
      <xsl:text>abort This.The_T.all;&#10;</xsl:text>
      <xsl:value-of select="$II"/>
      <xsl:text>Free (This.The_T);&#10;</xsl:text>
    </xsl:if>

    <!-- Finalize any Timers. -->
    <xsl:for-each select="attribute[type='Timer']">
      <xsl:value-of select="$II"/>
      <xsl:text>ColdFrame.Project.Events.Finalize (This.</xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text>);&#10;</xsl:text>
    </xsl:for-each>

    <xsl:choose>

      <xsl:when test="$max = 1"/>

      <xsl:when test="$array = 'yes'">
        <xsl:value-of select="$II"/>
        <xsl:text>The_Container (With_Identifier.</xsl:text>
        <xsl:value-of select="attribute[@identifier]/name"/>
        <xsl:text>) := null;&#10;</xsl:text>
      </xsl:when>

      <xsl:otherwise>
        <xsl:value-of select="$II"/>
        <xsl:text>Maps.Unbind (The_Container, Key'Unchecked_Access);&#10;</xsl:text>
      </xsl:otherwise>

    </xsl:choose>

    <xsl:if test="statemachine">
      <!-- Clean up any events. -->
      <xsl:value-of select="$II"/>
      <xsl:text>ColdFrame.Project.Events.Finalize (This);&#10;</xsl:text>
      <!-- Check it's OK to delete the instance (ie, not in event handler).
           This will leak memory, but we were in a fatal situation anyway.
           -->
      <xsl:value-of select="$II"/>
      <xsl:text>Check_Deletable (This);&#10;</xsl:text>
    </xsl:if>

    <!-- Finalize the instance. -->
    <xsl:value-of select="$II"/>
    <xsl:text>Free (This);&#10;</xsl:text>

    <xsl:value-of select="$I"/>
    <xsl:text>end Delete;&#10;</xsl:text>

  </xsl:template>


  <!-- Called from domain/class to create the instance delete
       procedure body. -->
  <xsl:template name="delete-procedure-body">

    <!-- Calculate the maximum number of instances. -->
    <xsl:variable name="max">
      <xsl:call-template name="number-of-instances"/>
    </xsl:variable>

    <!-- Determine whether an array can be used. -->
    <xsl:variable name="array">
      <xsl:call-template name="can-use-array"/>
    </xsl:variable>

    <xsl:value-of select="$I"/>
    <xsl:text>procedure Delete (This : in out Handle) is&#10;</xsl:text>
    <xsl:value-of select="$I"/>
    <xsl:text>begin&#10;</xsl:text>

    <!-- This check is because of what seems to be a GNAT (3.14) error for
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
      <xsl:with-param name="report" select="1"/>
    </xsl:call-template>
    <xsl:call-template name="clear-parent-child-info">
      <xsl:with-param name="handle" select="'This'"/>
    </xsl:call-template>

    <xsl:if test="@active">
      <xsl:value-of select="$II"/>
      <xsl:text>abort This.The_T.all;&#10;</xsl:text>
      <xsl:value-of select="$II"/>
      <xsl:text>Free (This.The_T);&#10;</xsl:text>
    </xsl:if>

    <!-- Finalize any Timers. -->
    <xsl:for-each select="attribute[type='Timer']">
      <xsl:value-of select="$II"/>
      <xsl:text>ColdFrame.Project.Events.Finalize (This.</xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text>);&#10;</xsl:text>
    </xsl:for-each>

    <xsl:choose>

      <xsl:when test="$max = 1">

        <!-- Check the "container" .. -->
        <!--
             if {class}.This /= This then
                raise ColdFrame.Exceptions.Not_Found;
             end if;
             -->
        <xsl:value-of select="$II"/>
        <xsl:text>if </xsl:text>
        <xsl:value-of select="name"/>
        <xsl:text>.This /= This then&#10;</xsl:text>
        <xsl:value-of select="$III"/>
        <xsl:text>raise ColdFrame.Exceptions.Not_Found;&#10;</xsl:text>
        <xsl:value-of select="$II"/>
        <xsl:text>end if;&#10;</xsl:text>

        <!-- Remove from the "container" .. -->
        <xsl:value-of select="$II"/>
        <xsl:value-of select="name"/>
        <xsl:text>.This := null;&#10;</xsl:text>

        <xsl:if test="statemachine">
          <!-- Clean up any events. -->
          <xsl:value-of select="$II"/>
          <xsl:text>ColdFrame.Project.Events.Finalize (This);&#10;</xsl:text>
          <!-- Check it's OK to delete the instance (ie, not in event
               handler).
               This will leak memory, but we were in a fatal situation anyway.
               -->
          <xsl:value-of select="$II"/>
          <xsl:text>Check_Deletable (This);&#10;</xsl:text>
        </xsl:if>

        <!-- .. and free the instance. -->
        <xsl:value-of select="$II"/>
        <xsl:text>Free (This);&#10;</xsl:text>

      </xsl:when>

      <xsl:when test="$array='yes'">

        <!-- Check the "container" .. -->
        <!--
             if The_Container (This.{id-attr}) /= This then
                raise ColdFrame.Exceptions.Not_Found;
             end if;
             -->
        <xsl:value-of select="$II"/>
        <xsl:text>if The_Container (This.</xsl:text>
        <xsl:value-of select="attribute[@identifier]/name"/>
        <xsl:text>) /= This then&#10;</xsl:text>
        <xsl:value-of select="$III"/>
        <xsl:text>raise ColdFrame.Exceptions.Not_Found;&#10;</xsl:text>
        <xsl:value-of select="$II"/>
        <xsl:text>end if;&#10;</xsl:text>

        <!-- Remove from the container .. -->
        <xsl:value-of select="$II"/>
        <xsl:text>The_Container (This.</xsl:text>
        <xsl:value-of select="attribute[@identifier]/name"/>
        <xsl:text>) := null;&#10;</xsl:text>

        <!-- .. clean up any events .. -->
        <xsl:if test="statemachine">
          <xsl:value-of select="$II"/>
          <xsl:text>ColdFrame.Project.Events.Finalize (This);&#10;</xsl:text>
        </xsl:if>

        <!-- .. and free the instance. -->
        <xsl:value-of select="$II"/>
        <xsl:text>Free (This);&#10;</xsl:text>

      </xsl:when>

      <xsl:otherwise>

        <!-- Remove from the container .. -->
        <xsl:value-of select="$II"/>
        <xsl:text>Maps.Unbind&#10;</xsl:text>
        <xsl:value-of select="$IIC"/>
        <xsl:text>(The_Container,&#10;</xsl:text>
        <xsl:value-of select="$IIC"/>
        <xsl:text> ColdFrame.Instances.Handle (This));&#10;</xsl:text>

        <xsl:if test="statemachine">
          <!-- Clean up any events. -->
          <xsl:value-of select="$II"/>
          <xsl:text>ColdFrame.Project.Events.Finalize (This);&#10;</xsl:text>
          <!-- Check it's OK to delete the instance (ie, not in event
               handler).
               This will leak memory, but we were in a fatal situation anyway.
               -->
          <xsl:value-of select="$II"/>
          <xsl:text>Check_Deletable (This);&#10;</xsl:text>
        </xsl:if>

        <!-- .. and free the instance. -->
        <xsl:value-of select="$II"/>
        <xsl:text>Free (This);&#10;</xsl:text>

      </xsl:otherwise>

    </xsl:choose>

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
             {child-1}.Delete
               ({child-1}.Handle
                  ({handle}.{relation}_Current_Child.{child-1-abbrev}));
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
        <xsl:text>.Delete&#10;</xsl:text>
        <xsl:value-of select="$IIIIC"/>
        <xsl:text>(</xsl:text>
        <xsl:value-of select="."/>
        <xsl:text>.Handle&#10;</xsl:text>
        <xsl:value-of select="$IIIIIC"/>
        <xsl:text>(</xsl:text>
        <xsl:value-of select="$handle"/>
        <xsl:text>.</xsl:text>
        <xsl:value-of select="$rel/name"/>
        <xsl:text>_Current_Child.</xsl:text>
        <xsl:value-of select="/domain/class[name=$child]/abbreviation"/>
        <xsl:text>));&#10;</xsl:text>

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
           ({parent}.Handle ({handle}.{relation}_Parent),
            (Current => {parent}.Null_T));
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
      <xsl:value-of select="parent"/>
      <xsl:text>.Handle (</xsl:text>
      <xsl:value-of select="$handle"/>
      <xsl:text>.</xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text>_Parent),&#10;</xsl:text>
      <xsl:value-of select="$IIC"/>
      <xsl:text> (Current =&gt; </xsl:text>
      <xsl:value-of select="parent"/>
      <xsl:text>.Null_T));&#10;</xsl:text>
    </xsl:for-each>

  </xsl:template>


  <!-- Called from domain/class, within the Delete procedures, to
       call any finalization procedures. -->
  <xsl:template name="perform-finalization">
    <xsl:param name="handle" select="'This'"/>
    <xsl:param name="report" select="/.."/>
    <!-- non-null for error message output. -->

    <xsl:for-each select="operation[@finalize]">
      <xsl:sort select="name"/>

      <xsl:choose>

        <xsl:when test="$report
                        and (@abstract or @return or @class or parameter)">
          <xsl:call-template name="log-error"/>
          <xsl:message>
            <xsl:text>Error: illegal finalize operation </xsl:text>
            <xsl:value-of select="../name"/>.<xsl:value-of select="name"/>
          </xsl:message>
        </xsl:when>

        <xsl:otherwise>
          <xsl:value-of select="$II"/>
          <xsl:value-of select="name"/>
          <xsl:text> (</xsl:text>
          <xsl:value-of select="$handle"/>
          <xsl:text>);&#10;</xsl:text>
        </xsl:otherwise>

      </xsl:choose>

    </xsl:for-each>

  </xsl:template>


  <!-- Called from domain/class to create the Find function body for
       non-singletons. -->
  <xsl:template name="find-function-body">

    <!--
         function Find (With_Identifier : Identifier) return Handle is
         begin
         -->

    <!-- Calculate the maximum number of instances. -->
    <xsl:variable name="max">
      <xsl:call-template name="number-of-instances"/>
    </xsl:variable>

    <!-- Determine whether an array can be used. -->
    <xsl:variable name="array">
      <xsl:call-template name="can-use-array"/>
    </xsl:variable>

    <xsl:value-of select="$I"/>
    <xsl:text>function Find (With_Identifier : Identifier) return Handle is&#10;</xsl:text>

    <xsl:choose>

      <xsl:when test="$max=1">

        <xsl:value-of select="$I"/>
        <xsl:text>begin&#10;</xsl:text>

        <!--
             if This = null then
                return null;
             elsif ({attr} => This.{attr},
                    {attr} => This.{attr}) /= With_Identifier then
                return null;
             else
                return This;
             end if;
             -->

        <xsl:value-of select="$II"/>
        <xsl:text>if This = null then&#10;</xsl:text>
        <xsl:value-of select="$III"/>
        <xsl:text>return null;&#10;</xsl:text>
        <xsl:value-of select="$II"/>
        <xsl:text>elsif (</xsl:text>
        <xsl:for-each select="attribute[@identifier]">
          <xsl:call-template name="attribute-name"/>
          <xsl:text> =&gt; This.</xsl:text>
          <xsl:call-template name="attribute-name"/>
          <xsl:if test="position() &lt; last()">
            <xsl:text>,&#10;       </xsl:text>
            <xsl:value-of select="$II"/>
          </xsl:if>
        </xsl:for-each>
        <xsl:text>) /= With_Identifier then&#10;</xsl:text>
        <xsl:value-of select="$III"/>
        <xsl:text>return null;&#10;</xsl:text>
        <xsl:value-of select="$II"/>
        <xsl:text>else&#10;</xsl:text>
        <xsl:value-of select="$III"/>
        <xsl:text>return This;&#10;</xsl:text>
        <xsl:value-of select="$II"/>
        <xsl:text>end if;&#10;</xsl:text>

      </xsl:when>

      <xsl:when test="$array='yes'">

        <xsl:value-of select="$I"/>
        <xsl:text>begin&#10;</xsl:text>

        <!--
             return The_Container (With_Identifier.{id-attr});
             -->

        <xsl:value-of select="$II"/>
        <xsl:text>return The_Container (With_Identifier.</xsl:text>
        <xsl:value-of select="attribute[@identifier]/name"/>
        <xsl:text>);&#10;</xsl:text>

      </xsl:when>

      <xsl:otherwise>

        <!--
                Key : aliased Instance;
             begin
                Set_Identifier (Key'Unchecked_Access, With_Identifier);
                if Maps.Is_Bound (The_Container, Key'Unchecked_Access) then
                   return Handle (Maps.Item_Of (The_Container, Key'Unchecked_Access));
                else
                   return null;
                end if;
             -->

        <xsl:value-of select="$II"/>
        <xsl:text>Key : aliased Instance;&#10;</xsl:text>

        <xsl:value-of select="$I"/>
        <xsl:text>begin&#10;</xsl:text>

        <xsl:value-of select="$II"/>
        <xsl:text>Set_Identifier (Key'Unchecked_Access, With_Identifier);&#10;</xsl:text>

        <xsl:value-of select="$II"/>
        <xsl:text>if Maps.Is_Bound (The_Container, Key'Unchecked_Access) then&#10;</xsl:text>
        <xsl:value-of select="$III"/>
        <xsl:text>return Handle (Maps.Item_Of (The_Container, Key'Unchecked_Access));&#10;</xsl:text>
        <xsl:value-of select="$II"/>
        <xsl:text>else&#10;</xsl:text>
        <xsl:value-of select="$III"/>
        <xsl:text>return null;&#10;</xsl:text>
        <xsl:value-of select="$II"/>
        <xsl:text>end if;&#10;</xsl:text>

      </xsl:otherwise>

    </xsl:choose>

    <!--
         end Find;
         -->
    <xsl:value-of select="$I"/>
    <xsl:text>end Find;&#10;</xsl:text>

  </xsl:template>


  <!-- Called from domain/class to create the Find function body for
       singletons. -->
  <xsl:template name="find-single-instance-function-body">

    <!--
         function Find return Handle is
         begin
            return This;
         end Find;
         -->

    <xsl:value-of select="$I"/>
    <xsl:text>function Find return Handle is&#10;</xsl:text>
    <xsl:value-of select="$I"/>
    <xsl:text>begin&#10;</xsl:text>
    <xsl:value-of select="$II"/>
    <xsl:text>return This;&#10;</xsl:text>
    <xsl:value-of select="$I"/>
    <xsl:text>end Find;&#10;</xsl:text>

  </xsl:template>


  <!-- Generates class initializations. -->
  <xsl:template
    mode="class-initialization"
    match="class[attribute[@class and initial] or @singleton]">

    <xsl:call-template name="do-not-edit"/>
    <xsl:call-template name="identification-info"/>
    <xsl:text>procedure </xsl:text>
    <xsl:value-of select="../name"/>
    <xsl:text>.</xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>.Class_Initialize;&#10;</xsl:text>

    <xsl:call-template name="do-not-edit"/>
    <xsl:call-template name="identification-info"/>
    <xsl:text>procedure </xsl:text>
    <xsl:value-of select="../name"/>
    <xsl:text>.</xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>.Class_Initialize is&#10;</xsl:text>

    <xsl:if test="@singleton">
      <xsl:value-of select="$I"/>
      <xsl:text>H : Handle;&#10;</xsl:text>
      <xsl:value-of select="$I"/>
      <xsl:text>pragma Warnings (Off, H);&#10;</xsl:text>
    </xsl:if>

    <xsl:text>begin&#10;</xsl:text>

    <xsl:for-each select="attribute[@class and initial]">

      <xsl:value-of select="$I"/>
      <xsl:value-of select="name"/>
      <xsl:text> := </xsl:text>
      <xsl:value-of select="initial"/>
      <xsl:text>;&#10;</xsl:text>

    </xsl:for-each>

     <xsl:if test="@singleton">
      <xsl:value-of select="$I"/>
      <xsl:text>H := Create;&#10;</xsl:text>
    </xsl:if>

   <xsl:text>end </xsl:text>
    <xsl:value-of select="../name"/>
    <xsl:text>.</xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>.Class_Initialize;&#10;</xsl:text>

  </xsl:template>

  <xsl:template mode="class-initialization" match="*"/>


  <!-- Called from domain/class to generate the instance identifier
       equality function. -->
  <xsl:template name="instance-identifier-equality-body">

    <!-- collect all the identifying attributes -->
    <xsl:variable name="identifiers" select="attribute[@identifier]"/>

    <xsl:value-of select="$I"/>
    <xsl:text>function Instance_Identifier_Equality (L, R : Instance) return Boolean is&#10;</xsl:text>

    <xsl:if test="$identifiers/@refers">
      <xsl:value-of select="$II"/>
      <xsl:text>use type ColdFrame.Instances.Handle;&#10;</xsl:text>
    </xsl:if>

    <xsl:value-of select="$I"/>
    <xsl:text>begin&#10;</xsl:text>

    <xsl:for-each select="$identifiers">
      <xsl:variable name="name">
        <xsl:call-template name="attribute-name"/>
      </xsl:variable>
      <xsl:value-of select="$II"/>
      <xsl:text>if L.</xsl:text>
      <xsl:value-of select="$name"/>
      <xsl:text> /= R.</xsl:text>
      <xsl:value-of select="$name"/>
      <xsl:text> then&#10;</xsl:text>
      <xsl:value-of select="$III"/>
      <xsl:text>return False;&#10;</xsl:text>
      <xsl:value-of select="$II"/>
      <xsl:text>end if;&#10;</xsl:text>
    </xsl:for-each>

    <xsl:value-of select="$II"/>
    <xsl:text>return True;&#10;</xsl:text>

    <xsl:value-of select="$I"/>
    <xsl:text>end Instance_Identifier_Equality;&#10;</xsl:text>
    <xsl:value-of select="$blank-line"/>

  </xsl:template>


  <!-- Called from domain/class to generate the separate hash function. -->
  <xsl:template name="hash-function-body">

    <xsl:call-template name="should-not-edit"/>
    <xsl:call-template name="identification-info"/>

    <!-- collect all the identifying attributes -->
    <xsl:variable name="identifiers" select="attribute[@identifier]"/>

    <xsl:variable
      name="bounded-string-types"
      select="/domain/type[name=$identifiers/type and string/max]"/>

    <xsl:for-each select="$bounded-string-types">
      <xsl:sort select="name"/>
      <xsl:text>with </xsl:text>
      <xsl:value-of select="../name"/>
      <xsl:text>.</xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text>_Hash;&#10;</xsl:text>
    </xsl:for-each>

    <xsl:if test="$identifiers/@refers">
      <xsl:text>with ColdFrame.Hash.Instance_Access_Hash;&#10;</xsl:text>
    </xsl:if>

    <!-- fixed-length strings -->
    <xsl:if test="/domain/type[name=$identifiers/type and string/fixed]">
      <xsl:text>with ColdFrame.Hash.Strings.Standard;&#10;</xsl:text>
    </xsl:if>

    <!-- unbounded strings -->
    <xsl:if test="$identifiers/type='Unbounded_String'
                  or $identifiers/type='Text'">
      <xsl:text>with ColdFrame.Hash.Strings.Unbounded;&#10;</xsl:text>
    </xsl:if>

    <xsl:text>separate (</xsl:text>
    <xsl:value-of select="../name"/>.<xsl:value-of select="name"/>
    <xsl:text>)&#10;</xsl:text>
    <xsl:text>function Instance_Hash (I : Instance) return Natural is&#10;</xsl:text>
    <xsl:value-of select="$I"/>
    <xsl:text>type M is mod 2**31;&#10;</xsl:text>
    <xsl:value-of select="$I"/>
    <xsl:text>Result : M := 0;&#10;</xsl:text>
    <xsl:text>begin&#10;</xsl:text>

    <xsl:for-each select="$identifiers">
      <xsl:sort select="name"/>

      <xsl:if test="position() &gt; 1">
        <xsl:value-of select="$I"/>
        <xsl:text>Result := Result * 10019;&#10;</xsl:text>
      </xsl:if>

      <xsl:variable name="type-name" select="type"/>
      <xsl:variable name="type" select="/domain/type[name=$type-name]"/>

      <xsl:choose>

        <xsl:when test="type='Autonumber'">
          <!-- Must be the only identifying attribute. -->
          <xsl:value-of select="$I"/>
          <xsl:text>Result := Result xor M (I.</xsl:text>
          <xsl:value-of select="name"/>
          <xsl:text> mod 2**31);&#10;</xsl:text>
        </xsl:when>

        <xsl:when test="$type/enumeration or type='Boolean'
                        or $type/integer or type='Integer'
                        or type='Natural' or type='Positive'
                        or $type/@hash">

          <!--
               Result := Result xor {type-name}'Pos (I.{name});
               -->

          <xsl:value-of select="$I"/>
          <xsl:text>Result := Result xor </xsl:text>
          <xsl:value-of select="$type-name"/>
          <xsl:text>'Pos (I.</xsl:text>
          <xsl:value-of select="name"/>
          <xsl:text>);&#10;</xsl:text>

        </xsl:when>

        <xsl:when test="$type/string/max">

          <!-- A Bounded_String. We specify the domain name because GNAT
               wants it ..
               Result := Result xor
                 M ({domain-name}.{type-name}_Hash (I.{name}));
               -->

          <xsl:value-of select="$I"/>
          <xsl:text>Result := Result xor&#10;</xsl:text>
          <xsl:value-of select="$IC"/>
          <xsl:text>M (</xsl:text>
          <xsl:value-of select="../../name"/>
          <xsl:text>.</xsl:text>
          <xsl:value-of select="$type-name"/>
          <xsl:text>_Hash (I.</xsl:text>
          <xsl:value-of select="name"/>
          <xsl:text>));&#10;</xsl:text>

        </xsl:when>

        <xsl:when test="$type/string/fixed">

          <!-- A standard string ..
               Result := Result xor
                 M (ColdFrame.Hash.Strings.Standard (I.{name}));
               -->

          <xsl:value-of select="$I"/>
          <xsl:text>Result := Result xor&#10;</xsl:text>
          <xsl:value-of select="$IC"/>
          <xsl:text>M (ColdFrame.Hash.Strings.Standard (I.</xsl:text>
          <xsl:value-of select="name"/>
          <xsl:text>));&#10;</xsl:text>

        </xsl:when>

        <xsl:when test="type='Unbounded_String' or type='Text'">

          <!--
               Result := Result
                 xor M (ColdFrame.Hash.Strings.Unbounded (I.{name}));
               -->

          <xsl:value-of select="$I"/>
          <xsl:text>Result := Result xor&#10;</xsl:text>
          <xsl:value-of select="$IC"/>
          <xsl:text>M (ColdFrame.Hash.Strings.Unbounded (I.</xsl:text>
          <xsl:value-of select="name"/>
          <xsl:text>));&#10;</xsl:text>

        </xsl:when>

        <xsl:when test="@refers">

          <!--
               Result := Result xor M
                 (ColdFrame.Hash.Instance_Access_Hash
                  (I.{name}));
               -->

          <xsl:value-of select="$I"/>
          <xsl:text>Result := Result xor M&#10;</xsl:text>
          <xsl:value-of select="$IC"/>
          <xsl:text>(ColdFrame.Hash.Instance_Access_Hash&#10;</xsl:text>
          <xsl:value-of select="$IC"/>
          <xsl:text> (I.</xsl:text>
          <xsl:call-template name="attribute-name">
            <xsl:with-param name="a" select="."/>
          </xsl:call-template>
          <xsl:text>));&#10;</xsl:text>

        </xsl:when>

        <xsl:otherwise>
          <xsl:message>
            <xsl:text>Warning: </xsl:text>
            <xsl:value-of select="../name"/>
            <xsl:text>: no rule to hash </xsl:text>
            <xsl:value-of select="$type-name"/>
          </xsl:message>
        </xsl:otherwise>

      </xsl:choose>

    </xsl:for-each>

    <xsl:value-of select="$I"/>
    <xsl:text>return Natural (Result);&#10;</xsl:text>

    <xsl:text>end Instance_Hash;&#10;</xsl:text>

  </xsl:template>


  <!-- Called from domain/class to compute the number of hash buckets. -->
  <xsl:template name="hash-buckets">

    <xsl:variable name="max">
      <xsl:call-template name="number-of-instances"/>
    </xsl:variable>

    <xsl:choose>

      <xsl:when test="$max &lt; $max-hash-buckets">
        <xsl:value-of select="$max"/>
      </xsl:when>

      <xsl:otherwise>
        <xsl:value-of select="$max-hash-buckets"/>
      </xsl:otherwise>

    </xsl:choose>
  </xsl:template>


  <!-- Called from domain/class to generate a task spec. -->
  <xsl:template name="task-spec">
    <!--
         use type System.Priority;
         task type T (This : access Instance) is
           pragma Task_Name ("{domain}.{name}");
           pragma Priority (System.Default_Priority + ({priority}));
           pragma Storage_Size ({stack});
           entry {e} ({parameters});
         end T;
         type T_P is access T;
         procedure Free is new Ada.Unchecked_Deallocation (T, T_P);
         -->
    <xsl:if test="@priority">
      <xsl:value-of select="$I"/>
      <xsl:text>use type System.Priority;&#10;</xsl:text>
    </xsl:if>
    <xsl:value-of select="$I"/>
    <xsl:text>task type T (This : access Instance) is&#10;</xsl:text>
    <xsl:value-of select="$II"/>
    <xsl:text>pragma Task_Name (&quot;</xsl:text>
    <xsl:value-of select="../name"/>
    <xsl:text>.</xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>&quot;);&#10;</xsl:text>
    <xsl:if test="@priority">
      <xsl:value-of select="$II"/>
      <xsl:text>pragma Priority (System.Default_Priority + (</xsl:text>
      <xsl:value-of select="@priority"/>
      <xsl:text>));&#10;</xsl:text>
    </xsl:if>
    <xsl:if test="@stack">
      <xsl:value-of select="$II"/>
      <xsl:text>pragma Storage_Size (</xsl:text>
      <xsl:value-of select="@stack"/>
      <xsl:text>);&#10;</xsl:text>
    </xsl:if>
    <xsl:apply-templates mode="task-entry" select="operation">
      <xsl:sort select="name"/>
    </xsl:apply-templates>
    <xsl:value-of select="$I"/>
    <xsl:text>end T;&#10;</xsl:text>
    <xsl:value-of select="$I"/>
    <xsl:text>type T_P is access T;&#10;</xsl:text>
    <xsl:value-of select="$I"/>
    <xsl:text>procedure Free is new Ada.Unchecked_Deallocation (T, T_P);&#10;</xsl:text>
    <xsl:value-of select="$blank-line"/>
  </xsl:template>


  <!-- Generate task entry specs. -->
  <xsl:template mode="task-entry" match="operation[@entry]">

    <xsl:value-of select="$II"/>
    <xsl:text>entry </xsl:text>
    <xsl:value-of select="name"/>
    <xsl:call-template name="entry-parameter-list">
      <xsl:with-param name="indent" select="$IIC"/>
    </xsl:call-template>
    <xsl:text>;&#10;</xsl:text>

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
