<!-- $Id: ada-association-collection.xsl,v df5a5ba14c37 2002/02/17 11:28:01 simon $ -->
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

     As a special exception, when portions of this file are copied by a
     stylesheet processor into an output file, you may use that output
     file without restriction.
     -->

<!-- Generates support for navigation from collections (navigation from
     single handles is in ada-association.xsl). -->

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="1.0">

  <!-- Generate specs for Association packages. -->
  <xsl:template match="domain/association" mode="association-collection-spec">

    <xsl:variable name="class-1" select="role[1]/classname"/>
    <xsl:variable name="class-2" select="role[2]/classname"/>

    <xsl:if test="not(/domain/class[name=$class-1 or name=$class-2]/@singleton)">
      
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

  </xsl:template>


  <!-- Generate bodies for Association packages. -->
  <xsl:template match="domain/association" mode="association-collection-body">

    <xsl:variable name="class-1" select="role[1]/classname"/>
    <xsl:variable name="class-2" select="role[2]/classname"/>

    <xsl:if test="not(/domain/class[name=$class-1 or name=$class-2]/@singleton)">
      
      <!-- Context clauses. -->
      <xsl:call-template name="association-body-context"/>
      
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
        
        <xsl:value-of select="$II"/>
        <xsl:text>function Nav is new ColdFrame.Navigate_From_Many_Collection&#10;</xsl:text>
        
        <xsl:value-of select="$IIC"/>
        <xsl:text>(Many_Instance =&gt; </xsl:text>
        <xsl:value-of select="$a"/>
        <xsl:text>.Instance,&#10;</xsl:text>
        
        <xsl:value-of select="$IIC"/>
        <xsl:text> Many_Handle =&gt; </xsl:text>
        <xsl:value-of select="$a"/>
        <xsl:text>.Handle,&#10;</xsl:text>
        
        <xsl:value-of select="$IIC"/>
        <xsl:text> Many =&gt; </xsl:text>
        <xsl:value-of select="$a"/>
        <xsl:text>.Abstract_Containers,&#10;</xsl:text>
        
        <xsl:value-of select="$IIC"/>
        <xsl:text> From =&gt; </xsl:text>
        <xsl:value-of select="$a"/>
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
        
      </xsl:when>
      
      <xsl:when test="$role-b/@multiple">
        
        <xsl:value-of select="$II"/>
        <xsl:text>function Nav is new ColdFrame.Navigate_From_One_To_Many_Collection&#10;</xsl:text>
        
        <xsl:value-of select="$IIC"/>
        <xsl:text>(One_Instance =&gt; </xsl:text>
        <xsl:value-of select="$a"/>
        <xsl:text>.Instance,&#10;</xsl:text>
        
        <xsl:value-of select="$IIC"/>
        <xsl:text> One_Handle =&gt; </xsl:text>
        <xsl:value-of select="$a"/>
        <xsl:text>.Handle,&#10;</xsl:text>
        
        <xsl:value-of select="$IIC"/>
        <xsl:text> One =&gt; </xsl:text>
        <xsl:value-of select="$a"/>
        <xsl:text>.Abstract_Containers,&#10;</xsl:text>
        
        <xsl:value-of select="$IIC"/>
        <xsl:text> From =&gt; </xsl:text>
        <xsl:value-of select="$a"/>
        <xsl:text>.Collections.Collection,&#10;</xsl:text>
        
        <xsl:value-of select="$IIC"/>
        <xsl:text> Many_Instance =&gt; </xsl:text>
        <xsl:value-of select="$b"/>
        <xsl:text>.Instance,&#10;</xsl:text>
        
        <xsl:value-of select="$IIC"/>
        <xsl:text> Many_Handle =&gt; </xsl:text>
        <xsl:value-of select="$b"/>
        <xsl:text>.Handle,&#10;</xsl:text>
        
        <xsl:value-of select="$IIC"/>
        <xsl:text> Many =&gt; </xsl:text>
        <xsl:value-of select="$b"/>
        <xsl:text>.Abstract_Containers,&#10;</xsl:text>
        
        <xsl:value-of select="$IIC"/>
        <xsl:text> To =&gt; </xsl:text>
        <xsl:value-of select="$b"/>
        <xsl:text>.Collections.Collection,&#10;</xsl:text>
        
        <xsl:value-of select="$IIC"/>
        <xsl:text> Navigate_From_One =&gt; </xsl:text>
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
        
      </xsl:when>
      
      <xsl:otherwise>    <!-- one-to-one -->
      
        <xsl:value-of select="$II"/>
        <xsl:text>function Nav is new ColdFrame.Navigate_From_One_To_One_Collection&#10;</xsl:text>
      
        <xsl:value-of select="$IIC"/>
        <xsl:text>(First_Instance =&gt; </xsl:text>
        <xsl:value-of select="$a"/>
        <xsl:text>.Instance,&#10;</xsl:text>
        
        <xsl:value-of select="$IIC"/>
        <xsl:text> First_Handle =&gt; </xsl:text>
        <xsl:value-of select="$a"/>
        <xsl:text>.Handle,&#10;</xsl:text>
        
        <xsl:value-of select="$IIC"/>
        <xsl:text> First =&gt; </xsl:text>
        <xsl:value-of select="$a"/>
        <xsl:text>.Abstract_Containers,&#10;</xsl:text>
        
        <xsl:value-of select="$IIC"/>
        <xsl:text> From =&gt; </xsl:text>
        <xsl:value-of select="$a"/>
        <xsl:text>.Collections.Collection,&#10;</xsl:text>
        
        <xsl:value-of select="$IIC"/>
        <xsl:text> Second_Instance =&gt; </xsl:text>
        <xsl:value-of select="$b"/>
        <xsl:text>.Instance,&#10;</xsl:text>
        
        <xsl:value-of select="$IIC"/>
        <xsl:text> Second_Handle =&gt; </xsl:text>
        <xsl:value-of select="$b"/>
        <xsl:text>.Handle,&#10;</xsl:text>
        
        <xsl:value-of select="$IIC"/>
        <xsl:text> Second =&gt; </xsl:text>
        <xsl:value-of select="$b"/>
        <xsl:text>.Abstract_Containers,&#10;</xsl:text>
        
        <xsl:value-of select="$IIC"/>
        <xsl:text> To =&gt; </xsl:text>
        <xsl:value-of select="$b"/>
        <xsl:text>.Collections.Collection,&#10;</xsl:text>
        
        <xsl:value-of select="$IIC"/>
        <xsl:text> Navigate_From_First =&gt; </xsl:text>
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
        
      </xsl:otherwise>
      
    </xsl:choose>
    
    <xsl:value-of select="$I"/>
    <xsl:text>begin&#10;</xsl:text>

    <xsl:value-of select="$II"/>
    <xsl:text>return Nav (</xsl:text>
    <xsl:value-of select="/domain/class[name=$role-a/classname]/abbreviation"/>
    <xsl:text>);&#10;</xsl:text>

    <xsl:value-of select="$I"/>
    <xsl:text>end </xsl:text>
    <xsl:value-of select="$role-a/name"/>
    <xsl:text>;&#10;</xsl:text>
    <xsl:value-of select="$blank-line"/>

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
        
      </xsl:when>
      
      <xsl:otherwise>

        <xsl:value-of select="$II"/>
        <xsl:text>function Nav is new ColdFrame.Navigate_From_One_To_One_Collection&#10;</xsl:text>
      
        <xsl:value-of select="$IIC"/>
        <xsl:text>(First_Instance =&gt; </xsl:text>
        <xsl:value-of select="$c"/>
        <xsl:text>.Instance,&#10;</xsl:text>
        
        <xsl:value-of select="$IIC"/>
        <xsl:text> First_Handle =&gt; </xsl:text>
        <xsl:value-of select="$c"/>
        <xsl:text>.Handle,&#10;</xsl:text>
        
        <xsl:value-of select="$IIC"/>
        <xsl:text> First =&gt; </xsl:text>
        <xsl:value-of select="$c"/>
        <xsl:text>.Abstract_Containers,&#10;</xsl:text>
        
        <xsl:value-of select="$IIC"/>
        <xsl:text> From =&gt; </xsl:text>
        <xsl:value-of select="$c"/>
        <xsl:text>.Collections.Collection,&#10;</xsl:text>
        
        <xsl:value-of select="$IIC"/>
        <xsl:text> Second_Instance =&gt; </xsl:text>
        <xsl:value-of select="$b"/>
        <xsl:text>.Instance,&#10;</xsl:text>
        
        <xsl:value-of select="$IIC"/>
        <xsl:text> Second_Handle =&gt; </xsl:text>
        <xsl:value-of select="$b"/>
        <xsl:text>.Handle,&#10;</xsl:text>
        
        <xsl:value-of select="$IIC"/>
        <xsl:text> Second =&gt; </xsl:text>
        <xsl:value-of select="$b"/>
        <xsl:text>.Abstract_Containers,&#10;</xsl:text>
        
        <xsl:value-of select="$IIC"/>
        <xsl:text> To =&gt; </xsl:text>
        <xsl:value-of select="$b"/>
        <xsl:text>.Collections.Collection,&#10;</xsl:text>
        
        <xsl:value-of select="$IIC"/>
        <xsl:text> Navigate_From_First =&gt; </xsl:text>
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
        
      </xsl:otherwise>
      
    </xsl:choose>

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

        <xsl:value-of select="$II"/>
        <xsl:text>function Nav is new ColdFrame.Navigate_From_One_To_Many_Collection&#10;</xsl:text>
        
        <xsl:value-of select="$IIC"/>
        <xsl:text>(One_Instance =&gt; </xsl:text>
        <xsl:value-of select="$a"/>
        <xsl:text>.Instance,&#10;</xsl:text>
        
        <xsl:value-of select="$IIC"/>
        <xsl:text> One_Handle =&gt; </xsl:text>
        <xsl:value-of select="$a"/>
        <xsl:text>.Handle,&#10;</xsl:text>
        
        <xsl:value-of select="$IIC"/>
        <xsl:text> One =&gt; </xsl:text>
        <xsl:value-of select="$a"/>
        <xsl:text>.Abstract_Containers,&#10;</xsl:text>
        
        <xsl:value-of select="$IIC"/>
        <xsl:text> From =&gt; </xsl:text>
        <xsl:value-of select="$a"/>
        <xsl:text>.Collections.Collection,&#10;</xsl:text>
        
        <xsl:value-of select="$IIC"/>
        <xsl:text> Many_Instance =&gt; </xsl:text>
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
        <xsl:text> To =&gt; </xsl:text>
        <xsl:value-of select="$c"/>
        <xsl:text>.Collections.Collection,&#10;</xsl:text>
        
        <xsl:value-of select="$IIC"/>
        <xsl:text> Navigate_From_One =&gt; </xsl:text>
        <xsl:value-of select="$role-a/name"/>
        <xsl:text>,&#10;</xsl:text>
        
        <xsl:value-of select="$IIC"/>
        <xsl:text> Clear =&gt; </xsl:text>
        <xsl:value-of select="$c"/>
        <xsl:text>.Collections.Clear,&#10;</xsl:text>
        
        <xsl:value-of select="$IIC"/>
        <xsl:text> Add_To_Result =&gt; </xsl:text>
        <xsl:value-of select="$c"/>
        <xsl:text>.Collections.Append);&#10;</xsl:text>

      </xsl:when>

      <xsl:otherwise>

        <xsl:value-of select="$II"/>
        <xsl:text>function Nav is new ColdFrame.Navigate_From_One_To_One_Collection&#10;</xsl:text>
        
        <xsl:value-of select="$IIC"/>
        <xsl:text>(First_Instance =&gt; </xsl:text>
        <xsl:value-of select="$a"/>
        <xsl:text>.Instance,&#10;</xsl:text>
        
        <xsl:value-of select="$IIC"/>
        <xsl:text> First_Handle =&gt; </xsl:text>
        <xsl:value-of select="$a"/>
        <xsl:text>.Handle,&#10;</xsl:text>
        
        <xsl:value-of select="$IIC"/>
        <xsl:text> First =&gt; </xsl:text>
        <xsl:value-of select="$a"/>
        <xsl:text>.Abstract_Containers,&#10;</xsl:text>
        
        <xsl:value-of select="$IIC"/>
        <xsl:text> From =&gt; </xsl:text>
        <xsl:value-of select="$a"/>
        <xsl:text>.Collections.Collection,&#10;</xsl:text>
        
        <xsl:value-of select="$IIC"/>
        <xsl:text> Second_Instance =&gt; </xsl:text>
        <xsl:value-of select="$c"/>
        <xsl:text>.Instance,&#10;</xsl:text>
        
        <xsl:value-of select="$IIC"/>
        <xsl:text> Second_Handle =&gt; </xsl:text>
        <xsl:value-of select="$c"/>
        <xsl:text>.Handle,&#10;</xsl:text>
        
        <xsl:value-of select="$IIC"/>
        <xsl:text> Second =&gt; </xsl:text>
        <xsl:value-of select="$c"/>
        <xsl:text>.Abstract_Containers,&#10;</xsl:text>
        
        <xsl:value-of select="$IIC"/>
        <xsl:text> To =&gt; </xsl:text>
        <xsl:value-of select="$c"/>
        <xsl:text>.Collections.Collection,&#10;</xsl:text>
        
        <xsl:value-of select="$IIC"/>
        <xsl:text> Navigate_From_First =&gt; </xsl:text>
        <xsl:value-of select="$role-a/name"/>
        <xsl:text>,&#10;</xsl:text>
        
        <xsl:value-of select="$IIC"/>
        <xsl:text> Clear =&gt; </xsl:text>
        <xsl:value-of select="$c"/>
        <xsl:text>.Collections.Clear,&#10;</xsl:text>
        
        <xsl:value-of select="$IIC"/>
        <xsl:text> Add_To_Result =&gt; </xsl:text>
        <xsl:value-of select="$c"/>
        <xsl:text>.Collections.Append);&#10;</xsl:text>
        
      </xsl:otherwise>
      
    </xsl:choose>

    <xsl:value-of select="$I"/>
    <xsl:text>begin&#10;</xsl:text>

    <xsl:value-of select="$II"/>
    <xsl:text>return Nav (</xsl:text>
    <xsl:value-of select="/domain/class[name=$role-a/classname]/abbreviation"/>
    <xsl:text>);&#10;</xsl:text>

    <xsl:value-of select="$I"/>
    <xsl:text>end </xsl:text>
    <xsl:value-of select="$role-a/name"/>
    <xsl:text>;&#10;</xsl:text>
    <xsl:value-of select="$blank-line"/>

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


</xsl:stylesheet>
