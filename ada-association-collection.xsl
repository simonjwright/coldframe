<!-- $Id: ada-association-collection.xsl,v 55643a9356e6 2003/09/06 06:49:24 simon $ -->
<!-- XSL stylesheet to generate Ada code for Associations. -->
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

<!-- Generates support for navigation from collections (navigation from
     single handles is in ada-association.xsl). -->

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="1.0">

  <!-- Generate specs for Association packages. -->
  <xsl:template match="domain/association" mode="association-collection-spec">

    <xsl:if test="not(/domain/class[name=current()/role/classname]/@singleton)">

      <xsl:call-template name="do-not-edit"/>
      <xsl:call-template name="identification-info"/>

      <!-- Context clauses. -->
      <xsl:call-template name="association-collection-spec-context"/>
      
      <xsl:text>package </xsl:text>
      <xsl:value-of select="../name"/>.<xsl:value-of select="name"/>
      <xsl:text>.From_Collections is&#10;</xsl:text>
      <xsl:value-of select="$blank-line"/>
      
      <!-- .. navigations .. -->
      <xsl:call-template name="navigation-collection-specs"/>
      
      <!-- .. and close. -->
      <xsl:text>end </xsl:text>
      <xsl:value-of select="../name"/>.<xsl:value-of select="name"/>
      <xsl:text>.From_Collections;&#10;</xsl:text>
      
    </xsl:if>

  </xsl:template>

  <xsl:template match="*" mode="association-collection-spec"/>


  <!-- Called at domain/association to generate context clauses for the
       spec. -->
  <xsl:template name="association-collection-spec-context">

    <xsl:text>with </xsl:text>
    <xsl:value-of select="../name"/>
    <xsl:text>.</xsl:text>
    <xsl:value-of select="role[1]/classname"/>
    <xsl:text>.Collections;&#10;</xsl:text>

    <xsl:text>with </xsl:text>
    <xsl:value-of select="../name"/>
    <xsl:text>.</xsl:text>
    <xsl:value-of select="role[2]/classname"/>
    <xsl:text>.Collections;&#10;</xsl:text>

    <xsl:if test="associative">
      <xsl:text>with </xsl:text>
      <xsl:value-of select="../name"/>
      <xsl:text>.</xsl:text>
      <xsl:value-of select="associative"/>
      <xsl:text>.Collections;&#10;</xsl:text>      
    </xsl:if>

  </xsl:template>


  <!-- Generate bodies for Association packages. -->
  <xsl:template match="domain/association" mode="association-collection-body">

    <xsl:if test="not(/domain/class[name=current()/role/classname]/@singleton)">
      
      <xsl:call-template name="do-not-edit"/>
      <xsl:call-template name="identification-info"/>

      <!-- Context clauses. -->
      <xsl:call-template name="association-collection-body-context"/>
      
      <xsl:text>package body </xsl:text>
      <xsl:value-of select="../name"/>.<xsl:value-of select="name"/>
      <xsl:text>.From_Collections is&#10;</xsl:text>
      <xsl:value-of select="$blank-line"/>
      
      <!-- .. navigations .. -->
      <xsl:call-template name="navigation-collection-bodies"/>
      
      <!-- .. and close. -->
      <xsl:text>end </xsl:text>
      <xsl:value-of select="../name"/>.<xsl:value-of select="name"/>
      <xsl:text>.From_Collections;&#10;</xsl:text>
      
    </xsl:if>

  </xsl:template>

  <xsl:template match="*" mode="association-collection-body"/>


  <!-- Called at domain/association to generate context clauses for the
       body. -->
  <xsl:template name="association-collection-body-context">

    <xsl:text>with </xsl:text>
    <xsl:value-of select="../name"/>
    <xsl:text>.</xsl:text>
    <xsl:value-of select="role[1]/classname"/>
    <xsl:text>.Abstract_Containers;&#10;</xsl:text>
    <xsl:if test="role[2]/@multiple">
      <xsl:text>with </xsl:text>
      <xsl:value-of select="../name"/>
      <xsl:text>.</xsl:text>
      <xsl:value-of select="role[1]/classname"/>
      <xsl:text>.Sets;&#10;</xsl:text>      
    </xsl:if>

    <xsl:text>with </xsl:text>
    <xsl:value-of select="../name"/>
    <xsl:text>.</xsl:text>
    <xsl:value-of select="role[2]/classname"/>
    <xsl:text>.Abstract_Containers;&#10;</xsl:text>
    <xsl:if test="role[1]/@multiple">
      <xsl:text>with </xsl:text>
      <xsl:value-of select="../name"/>
      <xsl:text>.</xsl:text>
      <xsl:value-of select="role[2]/classname"/>
      <xsl:text>.Sets;&#10;</xsl:text>
    </xsl:if>

    <xsl:if test="associative">
      <xsl:text>with </xsl:text>
      <xsl:value-of select="../name"/>
      <xsl:text>.</xsl:text>
      <xsl:value-of select="associative"/>
      <xsl:text>.Abstract_Containers;&#10;</xsl:text>    
    </xsl:if>

  </xsl:template>


  <!-- Called at domain/association to generate the navigation function
       specs. -->
  <xsl:template name="navigation-collection-specs">

    <xsl:variable name="role-1" select="role[1]"/>
    <xsl:variable
      name="singleton-1"
      select="/domain/class[name=$role-1/classname]/@singleton"/>
    <xsl:variable name="role-2" select="role[2]"/>
    <xsl:variable
      name="singleton-2"
      select="/domain/class[name=$role-2/classname]/@singleton"/>

    <!-- First direction : from collection -->

    <xsl:if test="not($singleton-1) and not($singleton-2)">
      <xsl:call-template name="navigation-collection-specification">
        <xsl:with-param name="role-a" select="role[1]"/>
        <xsl:with-param name="role-b" select="role[2]"/>
      </xsl:call-template>
      <xsl:text>;&#10;</xsl:text>
      <xsl:value-of select="$blank-line"/>
    </xsl:if>
    
    <xsl:if test="associative">

      <xsl:if test="not($singleton-1)">
        <xsl:call-template
          name="navigation-collection-to-associative-specification">
          <xsl:with-param name="role-a" select="role[1]"/>
          <xsl:with-param name="role-b" select="role[2]"/>
          <xsl:with-param name="assoc" select="associative"/>
        </xsl:call-template>
        <xsl:text>;&#10;</xsl:text>
        <xsl:value-of select="$blank-line"/>
      </xsl:if>
      
      <xsl:if test="not($singleton-2)">
        <xsl:call-template
          name="navigation-collection-from-associative-specification">
          <xsl:with-param name="role-a" select="role[1]"/>
          <xsl:with-param name="role-b" select="role[2]"/>
          <xsl:with-param name="assoc" select="associative"/>
        </xsl:call-template>
        <xsl:text>;&#10;</xsl:text>
        <xsl:value-of select="$blank-line"/>
      </xsl:if>
      
    </xsl:if>
    
    <!-- Second direction: from collection -->
    
    <xsl:if test="not($singleton-1) and not($singleton-2)">
      <xsl:call-template name="navigation-collection-specification">
        <xsl:with-param name="role-a" select="role[2]"/>
        <xsl:with-param name="role-b" select="role[1]"/>
      </xsl:call-template>
      <xsl:text>;&#10;</xsl:text>
      <xsl:value-of select="$blank-line"/>
    </xsl:if>
    
    <xsl:if test="associative">
      
      <xsl:if test="not($singleton-2)">
        <xsl:call-template
          name="navigation-collection-to-associative-specification">
          <xsl:with-param name="role-a" select="role[2]"/>
          <xsl:with-param name="role-b" select="role[1]"/>
          <xsl:with-param name="assoc" select="associative"/>
        </xsl:call-template>
        <xsl:text>;&#10;</xsl:text>
        <xsl:value-of select="$blank-line"/>
      </xsl:if>
       
      <xsl:if test="not($singleton-1)">
        <xsl:call-template
          name="navigation-collection-from-associative-specification">
          <xsl:with-param name="role-a" select="role[2]"/>
          <xsl:with-param name="role-b" select="role[1]"/>
          <xsl:with-param name="assoc" select="associative"/>
        </xsl:call-template>
        <xsl:text>;&#10;</xsl:text>
        <xsl:value-of select="$blank-line"/>
      </xsl:if>
        
    </xsl:if>

  </xsl:template>


  <!-- Called at domain/association to generate the navigation function
       bodies. -->
  <xsl:template name="navigation-collection-bodies">

    <xsl:variable name="role-1" select="role[1]"/>
    <xsl:variable
      name="singleton-1"
      select="/domain/class[name=$role-1/classname]/@singleton"/>
    <xsl:variable name="role-2" select="role[2]"/>
    <xsl:variable
      name="singleton-2"
      select="/domain/class[name=$role-2/classname]/@singleton"/>

    <!-- First direction : from collection -->

    <xsl:if test="not($singleton-1) and not($singleton-2)">
      <xsl:call-template name="navigation-collection-body">
        <xsl:with-param name="role-a" select="role[1]"/>
        <xsl:with-param name="role-b" select="role[2]"/>
      </xsl:call-template>
    </xsl:if>
    
    <xsl:if test="associative">
      
      <xsl:if test="not($singleton-1)">
        <xsl:call-template name="navigation-collection-to-associative-body">
          <xsl:with-param name="role-a" select="role[1]"/>
          <xsl:with-param name="role-b" select="role[2]"/>
          <xsl:with-param name="assoc" select="associative"/>
        </xsl:call-template>
      </xsl:if>

      <xsl:if test="not($singleton-2)">
        <xsl:call-template name="navigation-collection-from-associative-body">
          <xsl:with-param name="role-a" select="role[1]"/>
          <xsl:with-param name="role-b" select="role[2]"/>
          <xsl:with-param name="assoc" select="associative"/>
        </xsl:call-template>
      </xsl:if>
        
    </xsl:if>

    <!-- Second direction: from collection -->

    <xsl:if test="not($singleton-1) and not($singleton-2)">
      <xsl:call-template name="navigation-collection-body">
        <xsl:with-param name="role-a" select="role[2]"/>
        <xsl:with-param name="role-b" select="role[1]"/>
      </xsl:call-template>
    </xsl:if>
      
    <xsl:if test="associative">
        
      <xsl:if test="not($singleton-2)">
        <xsl:call-template name="navigation-collection-to-associative-body">
          <xsl:with-param name="role-a" select="role[2]"/>
          <xsl:with-param name="role-b" select="role[1]"/>
          <xsl:with-param name="assoc" select="associative"/>
        </xsl:call-template>
      </xsl:if>
        
      <xsl:if test="not($singleton-1)">
        <xsl:call-template name="navigation-collection-from-associative-body">
          <xsl:with-param name="role-a" select="role[2]"/>
          <xsl:with-param name="role-b" select="role[1]"/>
          <xsl:with-param name="assoc" select="associative"/>
        </xsl:call-template>
      </xsl:if>

    </xsl:if>

  </xsl:template>


  <!-- Called at domain/association to generate a navigation-from-collection
       function body. -->
  <xsl:template name="navigation-collection-body">
    <xsl:param name="role-a"/>   <!-- from this .. -->
    <xsl:param name="role-b"/>   <!-- .. to this -->

    <xsl:choose>
      <xsl:when test="associative">
        <xsl:call-template
          name="navigation-collection-with-associative-body">
          <xsl:with-param name="role-a" select="$role-a"/>
          <xsl:with-param name="role-b" select="$role-b"/>
        </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
        <xsl:call-template
          name="navigation-collection-without-associative-body">
          <xsl:with-param name="role-a" select="$role-a"/>
          <xsl:with-param name="role-b" select="$role-b"/>
        </xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>

  </xsl:template>


  <!-- Called at domain/association to generate a navigation-from-collection
       function body when there is an associative class. -->
  <xsl:template name="navigation-collection-with-associative-body">
    <xsl:param name="role-a"/>   <!-- from this .. -->
    <xsl:param name="role-b"/>   <!-- .. to this -->

    <xsl:variable name="n" select="name"/>
    <xsl:variable name="a">
      <xsl:value-of select="$role-a/classname"/>
    </xsl:variable>
    <xsl:variable name="b">
      <xsl:value-of select="$role-b/classname"/>
    </xsl:variable>
    <xsl:variable name="associative" select="associative"/>
    <xsl:variable name="c">
      <xsl:value-of select="$associative"/>
    </xsl:variable>

    <xsl:call-template name="navigation-collection-specification">
      <xsl:with-param name="role-a" select="$role-a"/>
      <xsl:with-param name="role-b" select="$role-b"/>
    </xsl:call-template>
    <xsl:text> is&#10;</xsl:text>

    <xsl:value-of select="$II"/>
    <xsl:value-of
      select="/domain/class[name=$associative]/abbreviation"/>
    <xsl:text> : constant </xsl:text>
    <xsl:value-of select="$c"/>
    <xsl:text>.Collections.Collection&#10;</xsl:text>
    <xsl:value-of select="$IIC"/>
    <xsl:text>:= </xsl:text>
    <xsl:value-of select="$role-a/name"/>
    <xsl:text> (</xsl:text>
    <xsl:value-of
      select="/domain/class[name=$role-a/classname]/abbreviation"/>
    <xsl:text>);&#10;</xsl:text>
    
    <xsl:value-of select="$I"/>
    <xsl:text>begin&#10;</xsl:text>

    <xsl:value-of select="$II"/>
    <xsl:text>return </xsl:text>
    <xsl:value-of  select="$role-a/name"/>
    <xsl:text> (</xsl:text>
    <xsl:value-of
      select="/domain/class[name=$associative]/abbreviation"/>
    <xsl:text>);&#10;</xsl:text>

    <xsl:value-of select="$I"/>
    <xsl:text>end </xsl:text>
    <xsl:value-of select="$role-a/name"/>
    <xsl:text>;&#10;</xsl:text>
    <xsl:value-of select="$blank-line"/>

  </xsl:template>


  <!-- Called at domain/association to generate a navigation-from-collection
       function body when there is no associative class. -->
  <xsl:template name="navigation-collection-without-associative-body">
    <xsl:param name="role-a"/>   <!-- from this .. -->
    <xsl:param name="role-b"/>   <!-- .. to this -->

    <xsl:variable name="n" select="name"/>
    <xsl:variable name="a">
      <xsl:value-of select="$role-a/classname"/>
    </xsl:variable>
    <xsl:variable name="b">
      <xsl:value-of select="$role-b/classname"/>
    </xsl:variable>

    <xsl:call-template name="navigation-collection-specification">
      <xsl:with-param name="role-a" select="$role-a"/>
      <xsl:with-param name="role-b" select="$role-b"/>
    </xsl:call-template>
    <xsl:text> is&#10;</xsl:text>

    <xsl:choose>

      <xsl:when test="$role-a/@multiple">

        <xsl:call-template name="navigation-collection-many-to-one-body">
          
          <xsl:with-param name="a" select="$a"/>
          <xsl:with-param name="b" select="$b"/>
          <xsl:with-param name="role-a" select="$role-a"/>

        </xsl:call-template>

      </xsl:when>
      
      <xsl:when test="$role-b/@multiple">

        <xsl:call-template name="navigation-collection-one-to-many-body">
          
          <xsl:with-param name="a" select="$a"/>
          <xsl:with-param name="b" select="$b"/>
          <xsl:with-param name="role-a" select="$role-a"/>

        </xsl:call-template>

      </xsl:when>
      
      <xsl:otherwise>    <!-- one-to-one -->
      
        <xsl:call-template name="navigation-collection-one-to-one-body">
          
          <xsl:with-param name="a" select="$a"/>
          <xsl:with-param name="b" select="$b"/>
          <xsl:with-param name="role-a" select="$role-a"/>

        </xsl:call-template>

      </xsl:otherwise>
      
    </xsl:choose>
    
  </xsl:template>


  <!-- Called at domain/association to generate an associative navigation
       function body. -->
  <xsl:template name="navigation-collection-from-associative-body">
    <xsl:param name="role-a"/>
    <xsl:param name="role-b"/>   <!-- .. to this -->
    <xsl:param name="assoc"/>    <!-- from this .. -->

    <xsl:variable name="n" select="name"/>
    <xsl:variable name="a">
      <xsl:value-of select="$role-a/classname"/>
    </xsl:variable>
    <xsl:variable name="b">
      <xsl:value-of select="$role-b/classname"/>
    </xsl:variable>
    <xsl:variable name="c">
      <xsl:value-of select="$assoc"/>
    </xsl:variable>

    <xsl:call-template
      name="navigation-collection-from-associative-specification">
      <xsl:with-param name="role-a" select="$role-a"/>
      <xsl:with-param name="role-b" select="$role-b"/>
      <xsl:with-param name="assoc" select="$assoc"/>
    </xsl:call-template>
    <xsl:text> is&#10;</xsl:text>

    <xsl:choose>

      <xsl:when test="$role-a/@multiple">

        <xsl:call-template name="navigation-collection-many-to-one-body">
          
          <xsl:with-param name="a" select="$c"/>
          <xsl:with-param name="b" select="$b"/>
          <xsl:with-param name="role-a" select="$role-a"/>

        </xsl:call-template>

        <!--
        <xsl:value-of select="$II"/>
        <xsl:text>function Nav is new ColdFrame.Navigate_From_Many_Collection&#10;</xsl:text>
        
        <xsl:value-of select="$IIC"/>
        <xsl:text>(Many_Instance =&gt; </xsl:text>
        <xsl:value-of select="$c"/>
        <xsl:text>.Instance,&#10;</xsl:text>
        
        <xsl:value-of select="$IIC"/>
        <xsl:text> Many_Handle =&gt; </xsl:text>
        <xsl:value-of select="$c"/>
        <xsl:text>.Handle,&#10;</xsl:text>
        
        <xsl:value-of select="$IIC"/>
        <xsl:text> Many =&gt; </xsl:text>
        <xsl:value-of select="$c"/>
        <xsl:text>.Abstract_Containers,&#10;</xsl:text>
        
        <xsl:value-of select="$IIC"/>
        <xsl:text> From =&gt; </xsl:text>
        <xsl:value-of select="$c"/>
        <xsl:text>.Collections.Collection,&#10;</xsl:text>
        
        <xsl:value-of select="$IIC"/>
        <xsl:text> One_Instance =&gt; </xsl:text>
        <xsl:value-of select="$b"/>
        <xsl:text>.Instance,&#10;</xsl:text>
        
        <xsl:value-of select="$IIC"/>
        <xsl:text> One_Handle =&gt; </xsl:text>
        <xsl:value-of select="$b"/>
        <xsl:text>.Handle,&#10;</xsl:text>
        
        <xsl:value-of select="$IIC"/>
        <xsl:text> One =&gt; </xsl:text>
        <xsl:value-of select="$b"/>
        <xsl:text>.Abstract_Containers,&#10;</xsl:text>
        
        <xsl:value-of select="$IIC"/>
        <xsl:text> Set =&gt; </xsl:text>
        <xsl:value-of select="$b"/>
        <xsl:text>.Sets.Set,&#10;</xsl:text>
        
        <xsl:value-of select="$IIC"/>
        <xsl:text> Add_To_Set =&gt; </xsl:text>
        <xsl:value-of select="$b"/>
        <xsl:text>.Sets.Add,&#10;</xsl:text>
        
        <xsl:value-of select="$IIC"/>
        <xsl:text> To =&gt; </xsl:text>
        <xsl:value-of select="$b"/>
        <xsl:text>.Collections.Collection,&#10;</xsl:text>

        <xsl:value-of select="$IIC"/>
        <xsl:text> Navigate_From_Many =&gt; </xsl:text>
        <xsl:value-of select="$role-a/name"/>
        <xsl:text>,&#10;</xsl:text>
        
        <xsl:value-of select="$IIC"/>
        <xsl:text> Clear =&gt; </xsl:text>
        <xsl:value-of select="$b"/>
        <xsl:text>.Collections.Clear,&#10;</xsl:text>
        
        <xsl:value-of select="$IIC"/>
        <xsl:text> Add_To_Result =&gt; </xsl:text>
        <xsl:value-of select="$b"/>
        <xsl:text>.Collections.Append);&#10;</xsl:text>
        
        <xsl:value-of select="$I"/>
        <xsl:text>begin&#10;</xsl:text>

        <xsl:value-of select="$II"/>
        <xsl:text>return Nav (</xsl:text>
        <xsl:value-of select="/domain/class[name=$assoc]/abbreviation"/>
        <xsl:text>);&#10;</xsl:text>

        <xsl:value-of select="$I"/>
        <xsl:text>end </xsl:text>
        <xsl:value-of select="$role-a/name"/>
        <xsl:text>;&#10;</xsl:text>
        <xsl:value-of select="$blank-line"/>
        -->

      </xsl:when>
      
      <xsl:otherwise>

        <xsl:call-template name="navigation-collection-one-to-one-body">
          
          <xsl:with-param name="a" select="$c"/>
          <xsl:with-param name="b" select="$b"/>
          <xsl:with-param name="role-a" select="$role-a"/>

        </xsl:call-template>

      </xsl:otherwise>
      
    </xsl:choose>

  </xsl:template>


  <!-- Called at domain/association to generate an associative navigation
       function body. -->
  <xsl:template name="navigation-collection-to-associative-body">
    <xsl:param name="role-a"/>   <!-- from this .. -->
    <xsl:param name="role-b"/>
    <xsl:param name="assoc"/>    <!-- .. to this -->

    <xsl:variable name="n" select="name"/>
    <xsl:variable name="a">
      <xsl:value-of select="$role-a/classname"/>
    </xsl:variable>
    <xsl:variable name="b">
      <xsl:value-of select="$role-b/classname"/>
    </xsl:variable>
    <xsl:variable name="c">
      <xsl:value-of select="$assoc"/>
    </xsl:variable>

    <xsl:call-template
      name="navigation-collection-to-associative-specification">
      <xsl:with-param name="role-a" select="$role-a"/>
      <xsl:with-param name="role-b" select="$role-b"/>
      <xsl:with-param name="assoc" select="$assoc"/>
    </xsl:call-template>
    <xsl:text> is&#10;</xsl:text>

    <xsl:choose>

      <xsl:when test="$role-b/@multiple">

        <xsl:call-template name="navigation-collection-one-to-many-body">
          
          <xsl:with-param name="a" select="$a"/>
          <xsl:with-param name="b" select="$c"/>
          <xsl:with-param name="role-a" select="$role-a"/>

        </xsl:call-template>

      </xsl:when>

      <xsl:otherwise>

        <xsl:call-template name="navigation-collection-one-to-one-body">
          
          <xsl:with-param name="a" select="$a"/>
          <xsl:with-param name="b" select="$c"/>
          <xsl:with-param name="role-a" select="$role-a"/>

        </xsl:call-template>

      </xsl:otherwise>
      
    </xsl:choose>

  </xsl:template>


  <!-- Utilities. -->


  <!-- Called at domain/association to generate a navigation-from-collection
       function spec (no closing ";" or "is"). -->
  <xsl:template name="navigation-collection-specification">

    <xsl:param name="role-a"/>
    <xsl:param name="role-b"/>

    <xsl:value-of select="$I"/>
    <xsl:text>function </xsl:text>
    <xsl:value-of select="$role-a/name"/>
    <xsl:text>&#10;</xsl:text>
    <xsl:value-of select="$IC"/>
    <xsl:text>(</xsl:text>
    <xsl:value-of select="/domain/class[name=$role-a/classname]/abbreviation"/>
    <xsl:text> : </xsl:text>
    <xsl:value-of select="$role-a/classname"/>
    <xsl:text>.Collections.Collection)&#10;</xsl:text>
    <xsl:value-of select="$IC"/>
    <xsl:text>return </xsl:text>
    <xsl:value-of select="$role-b/classname"/>
    <xsl:text>.Collections.Collection</xsl:text>

  </xsl:template>


  <!-- Called at domain/association to generate a navigation-from-collection
       function spec (no closing ";" or "is"). -->
  <xsl:template name="navigation-collection-from-associative-specification">

    <xsl:param name="role-a"/>
    <xsl:param name="role-b"/>   <!-- .. to this -->
    <xsl:param name="assoc"/>    <!-- from this .. -->

    <xsl:value-of select="$I"/>
    <xsl:text>function </xsl:text>
    <xsl:value-of select="$role-a/name"/>
    <xsl:text>&#10;</xsl:text>
    <xsl:value-of select="$IC"/>
    <xsl:text>(</xsl:text>
    <xsl:value-of select="/domain/class[name=$assoc]/abbreviation"/>
    <xsl:text> : </xsl:text>
    <xsl:value-of select="$assoc"/>
    <xsl:text>.Collections.Collection)&#10;</xsl:text>
    <xsl:value-of select="$IC"/>
    <xsl:text>return </xsl:text>
    <xsl:value-of select="$role-b/classname"/>
    <xsl:text>.Collections.Collection</xsl:text>

  </xsl:template>


  <!-- Called at domain/association to generate an associative
       navigation-from-collection function spec (no closing ";" or "is"). -->
  <xsl:template name="navigation-collection-to-associative-specification">

    <xsl:param name="role-a"/>   <!-- from this .. -->
    <xsl:param name="role-b"/>
    <xsl:param name="assoc"/>    <!-- .. to this -->

    <xsl:value-of select="$I"/>
    <xsl:text>function </xsl:text>
    <xsl:value-of select="$role-a/name"/>
    <xsl:text>&#10;</xsl:text>
    <xsl:value-of select="$IC"/>
    <xsl:text>(</xsl:text>
    <xsl:value-of select="/domain/class[name=$role-a/classname]/abbreviation"/>
    <xsl:text> : </xsl:text>
    <xsl:value-of select="$role-a/classname"/>
    <xsl:text>.Collections.Collection)&#10;</xsl:text>
    <xsl:value-of select="$IC"/>
    <xsl:text>return </xsl:text>
    <xsl:value-of select="$assoc"/>
    <xsl:text>.Collections.Collection</xsl:text>

  </xsl:template>


  <!-- Called at domain/association to generate the body for a one-to-one
       navigation. -->
  <xsl:template name="navigation-collection-one-to-one-body">
    
    <xsl:param name="a"/>       <!-- name of source class -->
    <xsl:param name="b"/>       <!-- name of target class -->
    <xsl:param name="role-a"/>  <!-- node for source role -->

    <!-- from one to one
            In_It : {a}.Abstract_Containers.Iterator'Class
              := {a}.Collections.New_Iterator ({a-abbrev});
            T : {b}.Handle;
            Result : {b}.Collections.Collection;
            use {a}.Abstract_Containers;
            use {b};
            use {b}.Collections;
         begin
            while not Is_Done (In_It) loop
               T := {role-a} (Current_Item (In_It));
               if T /= null then
                  Append (Result, T);
               end if;
               Next (In_It);
            end loop;
            return Result;
         end {role-a};
         -->

    <xsl:value-of select="$II"/>
    <xsl:text>In_It : </xsl:text>
    <xsl:value-of select="$a"/>
    <xsl:text>.Abstract_Containers.Iterator'Class&#10;</xsl:text>

    <xsl:value-of select="$IIC"/>
    <xsl:text>:= </xsl:text>
    <xsl:value-of select="$a"/>
    <xsl:text>.Collections.New_Iterator (</xsl:text>
    <xsl:value-of select="../class[name=$a]/abbreviation"/>
    <xsl:text>);&#10;</xsl:text>

    <xsl:value-of select="$II"/>
    <xsl:text>T : </xsl:text>
    <xsl:value-of select="$b"/>
    <xsl:text>.Handle;&#10;</xsl:text>

    <xsl:value-of select="$II"/>
    <xsl:text>Result : </xsl:text>
    <xsl:value-of select="$b"/>
    <xsl:text>.Collections.Collection;&#10;</xsl:text>

    <xsl:value-of select="$II"/>
    <xsl:text>use </xsl:text>
    <xsl:value-of select="$a"/>
    <xsl:text>.Abstract_Containers;&#10;</xsl:text>

    <xsl:value-of select="$II"/>
    <xsl:text>use </xsl:text>
    <xsl:value-of select="$b"/>
    <xsl:text>;&#10;</xsl:text>

    <xsl:value-of select="$II"/>
    <xsl:text>use </xsl:text>
    <xsl:value-of select="$b"/>
    <xsl:text>.Collections;&#10;</xsl:text>

    <xsl:value-of select="$I"/>
    <xsl:text>begin&#10;</xsl:text>

    <xsl:value-of select="$II"/>
    <xsl:text>while not Is_Done (In_It) loop&#10;</xsl:text>

    <xsl:value-of select="$III"/>
    <xsl:text>T := </xsl:text>
    <xsl:value-of select="$role-a/name"/>
    <xsl:text> (Current_Item (In_It));&#10;</xsl:text>

    <xsl:value-of select="$III"/>
    <xsl:text>if T /= null then&#10;</xsl:text>

    <xsl:value-of select="$IIII"/>
    <xsl:text>Append (Result, T);&#10;</xsl:text>

    <xsl:value-of select="$III"/>
    <xsl:text>end if;&#10;</xsl:text>

    <xsl:value-of select="$III"/>
    <xsl:text>Next (In_It);&#10;</xsl:text>

    <xsl:value-of select="$II"/>
    <xsl:text>end loop;&#10;</xsl:text>

    <xsl:value-of select="$II"/>
    <xsl:text>return Result;&#10;</xsl:text>

    <xsl:value-of select="$I"/>
    <xsl:text>end </xsl:text>
    <xsl:value-of select="$role-a/name"/>
    <xsl:text>;&#10;</xsl:text>
    <xsl:value-of select="$blank-line"/>

  </xsl:template>


  <!-- Called at domain/association to generate the body for a many-to-one
       navigation. -->
  <xsl:template name="navigation-collection-many-to-one-body">
    
    <xsl:param name="a"/>       <!-- name of source class -->
    <xsl:param name="b"/>       <!-- name of target class -->
    <xsl:param name="role-a"/>  <!-- node for source role -->

    <!-- from many to one
            In_It : {a}.Abstract_Containers.Iterator'Class
              := {a}.Collections.New_Iterator ({a-abbrev});
            T : {b}.Handle;
            Tmp : {b}.Sets.Set;
            Tmp_It : {b}.Abstract_Containers.Iterator'Class
              := {b}.Sets.New_Iterator (Tmp);
            Result : {b}.Collections.Collection;
            use {a}.Abstract_Containers;
            use {b};
            use {b}.Abstract_Containers;
            use {b}.Collections;
            use {b}.Sets;
         begin
            while not Is_Done (In_It) loop
               T := {role-a} (Current_Item (In_It));
               if T /= null then
                   Add (Tmp, T);
               end if;
               Next (In_It);
            end loop;
            Reset (Tmp_It);
            while not Is_Done (Tmp_It) loop
               Append (Result, Current_Item (Tmp_It));
               Next (Tmp_It);
            end loop;
            return Result;
         end {role-a};
         -->

    <xsl:value-of select="$II"/>
    <xsl:text>In_It : </xsl:text>
    <xsl:value-of select="$a"/>
    <xsl:text>.Abstract_Containers.Iterator'Class&#10;</xsl:text>

    <xsl:value-of select="$IIC"/>
    <xsl:text>:= </xsl:text>
    <xsl:value-of select="$a"/>
    <xsl:text>.Collections.New_Iterator (</xsl:text>
    <xsl:value-of select="../class[name=$a]/abbreviation"/>
    <xsl:text>);&#10;</xsl:text>

    <xsl:value-of select="$II"/>
    <xsl:text>T : </xsl:text>
    <xsl:value-of select="$b"/>
    <xsl:text>.Handle;&#10;</xsl:text>

    <xsl:value-of select="$II"/>
    <xsl:text>Tmp : </xsl:text>
    <xsl:value-of select="$b"/>
    <xsl:text>.Sets.Set;&#10;</xsl:text>

    <xsl:value-of select="$II"/>
    <xsl:text>Tmp_It : </xsl:text>
    <xsl:value-of select="$b"/>
    <xsl:text>.Abstract_Containers.Iterator'Class&#10;</xsl:text>

    <xsl:value-of select="$IIC"/>
    <xsl:text>:= </xsl:text>
    <xsl:value-of select="$b"/>
    <xsl:text>.Sets.New_Iterator (Tmp);&#10;</xsl:text>

    <xsl:value-of select="$II"/>
    <xsl:text>Result : </xsl:text>
    <xsl:value-of select="$b"/>
    <xsl:text>.Collections.Collection;&#10;</xsl:text>

    <xsl:value-of select="$II"/>
    <xsl:text>use </xsl:text>
    <xsl:value-of select="$a"/>
    <xsl:text>.Abstract_Containers;&#10;</xsl:text>

    <xsl:value-of select="$II"/>
    <xsl:text>use </xsl:text>
    <xsl:value-of select="$b"/>
    <xsl:text>;&#10;</xsl:text>

    <xsl:value-of select="$II"/>
    <xsl:text>use </xsl:text>
    <xsl:value-of select="$b"/>
    <xsl:text>.Abstract_Containers;&#10;</xsl:text>

    <xsl:value-of select="$II"/>
    <xsl:text>use </xsl:text>
    <xsl:value-of select="$b"/>
    <xsl:text>.Collections;&#10;</xsl:text>

    <xsl:value-of select="$II"/>
    <xsl:text>use </xsl:text>
    <xsl:value-of select="$b"/>
    <xsl:text>.Sets;&#10;</xsl:text>

    <xsl:value-of select="$I"/>
    <xsl:text>begin&#10;</xsl:text>

    <xsl:value-of select="$II"/>
    <xsl:text>while not Is_Done (In_It) loop&#10;</xsl:text>

    <xsl:value-of select="$III"/>
    <xsl:text>T := </xsl:text>
    <xsl:value-of select="$role-a/name"/>
    <xsl:text> (Current_Item (In_It));&#10;</xsl:text>

    <xsl:value-of select="$III"/>
    <xsl:text>if T /= null then&#10;</xsl:text>

    <xsl:value-of select="$IIII"/>
    <xsl:text>Add (Tmp, T);&#10;</xsl:text>

    <xsl:value-of select="$III"/>
    <xsl:text>end if;&#10;</xsl:text>

    <xsl:value-of select="$III"/>
    <xsl:text>Next (In_It);&#10;</xsl:text>

    <xsl:value-of select="$II"/>
    <xsl:text>end loop;&#10;</xsl:text>

    <xsl:value-of select="$II"/>
    <xsl:text>Reset (Tmp_It);&#10;</xsl:text>

    <xsl:value-of select="$II"/>
    <xsl:text>while not Is_Done (Tmp_It) loop&#10;</xsl:text>

    <xsl:value-of select="$III"/>
    <xsl:text>Append (Result, Current_Item (Tmp_It));&#10;</xsl:text>

    <xsl:value-of select="$III"/>
    <xsl:text>Next (Tmp_It);&#10;</xsl:text>

    <xsl:value-of select="$II"/>
    <xsl:text>end loop;&#10;</xsl:text>

    <xsl:value-of select="$II"/>
    <xsl:text>return Result;&#10;</xsl:text>

    <xsl:value-of select="$I"/>
    <xsl:text>end </xsl:text>
    <xsl:value-of select="$role-a/name"/>
    <xsl:text>;&#10;</xsl:text>
    <xsl:value-of select="$blank-line"/>

  </xsl:template>


  <!-- Called at domain/association to generate the body for a one-to-many
       navigation. -->
  <xsl:template name="navigation-collection-one-to-many-body">
    
    <xsl:param name="a"/>       <!-- name of source class -->
    <xsl:param name="b"/>       <!-- name of target class -->
    <xsl:param name="role-a"/>  <!-- node for source role -->

    <!-- from one to many
            In_It : {a}.Abstract_Containers.Iterator'Class
              := {a}.Collections.New_Iterator ({a-abbrev});
            T : {b}.Collections.Collection;
            T_It : {b}.Abstract_Containers.Iterator'Class
              := {b}.Collections.New_Iterator (T);
            Result : {b}.Collections.Collection;
            use {a}.Abstract_Containers;
            use {b}.Abstract_Containers;
            use {b}.Collections;
         begin
            while not Is_Done (In_It) loop
               T := {role-a} (Current_Item (In_It));
               Reset (T_It);
               while not Is_Done (T_It) loop
                  Append (Result, Current_Item (T_It));
                  Next (T_It);
               end loop;
               Next (In_It);
            end loop;
            return Result;
         end {role-a};
         -->

    <xsl:value-of select="$II"/>
    <xsl:text>In_It : </xsl:text>
    <xsl:value-of select="$a"/>
    <xsl:text>.Abstract_Containers.Iterator'Class&#10;</xsl:text>

    <xsl:value-of select="$IIC"/>
    <xsl:text>:= </xsl:text>
    <xsl:value-of select="$a"/>
    <xsl:text>.Collections.New_Iterator (</xsl:text>
    <xsl:value-of select="../class[name=$a]/abbreviation"/>
    <xsl:text>);&#10;</xsl:text>

    <xsl:value-of select="$II"/>
    <xsl:text>T : </xsl:text>
    <xsl:value-of select="$b"/>
    <xsl:text>.Collections.Collection;&#10;</xsl:text>

    <xsl:value-of select="$II"/>
    <xsl:text>T_It : </xsl:text>
    <xsl:value-of select="$b"/>
    <xsl:text>.Abstract_Containers.Iterator'Class&#10;</xsl:text>

    <xsl:value-of select="$IIC"/>
    <xsl:text>:= </xsl:text>
    <xsl:value-of select="$b"/>
    <xsl:text>.Collections.New_Iterator (T);&#10;</xsl:text>

    <xsl:value-of select="$II"/>
    <xsl:text>Result : </xsl:text>
    <xsl:value-of select="$b"/>
    <xsl:text>.Collections.Collection;&#10;</xsl:text>

    <xsl:value-of select="$II"/>
    <xsl:text>use </xsl:text>
    <xsl:value-of select="$a"/>
    <xsl:text>.Abstract_Containers;&#10;</xsl:text>

    <xsl:value-of select="$II"/>
    <xsl:text>use </xsl:text>
    <xsl:value-of select="$b"/>
    <xsl:text>.Abstract_Containers;&#10;</xsl:text>

    <xsl:value-of select="$II"/>
    <xsl:text>use </xsl:text>
    <xsl:value-of select="$b"/>
    <xsl:text>.Collections;&#10;</xsl:text>

    <xsl:value-of select="$I"/>
    <xsl:text>begin&#10;</xsl:text>

    <xsl:value-of select="$II"/>
    <xsl:text>while not Is_Done (In_It) loop&#10;</xsl:text>

    <xsl:value-of select="$III"/>
    <xsl:text>T := </xsl:text>
    <xsl:value-of select="$role-a/name"/>
    <xsl:text> (Current_Item (In_It));&#10;</xsl:text>

    <xsl:value-of select="$III"/>
    <xsl:text>Reset (T_It);&#10;</xsl:text>

    <xsl:value-of select="$III"/>
    <xsl:text>while not Is_Done (T_It) loop&#10;</xsl:text>

    <xsl:value-of select="$IIII"/>
    <xsl:text>Append (Result, Current_Item (T_It));&#10;</xsl:text>

    <xsl:value-of select="$IIII"/>
    <xsl:text>Next (T_It);&#10;</xsl:text>

    <xsl:value-of select="$III"/>
    <xsl:text>end loop;&#10;</xsl:text>

    <xsl:value-of select="$III"/>
    <xsl:text>Next (In_It);&#10;</xsl:text>

    <xsl:value-of select="$II"/>
    <xsl:text>end loop;&#10;</xsl:text>

    <xsl:value-of select="$II"/>
    <xsl:text>return Result;&#10;</xsl:text>

    <xsl:value-of select="$I"/>
    <xsl:text>end </xsl:text>
    <xsl:value-of select="$role-a/name"/>
    <xsl:text>;&#10;</xsl:text>
    <xsl:value-of select="$blank-line"/>

  </xsl:template>


</xsl:stylesheet>
