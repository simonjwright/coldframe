<!-- $Id: ada-teardown.xsl,v dd318091d39b 2003/09/03 20:16:49 simon $ -->
<!-- XSL stylesheet to generate Ada code for tearing down the whole
     domain (for testing). -->
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
                version="1.0">

  <!-- Generate tear-down of the whole Domain, intended to be used
       with AUnit. Called at domain. -->
  <xsl:template name="domain-teardown">

    <xsl:call-template name="do-not-edit"/>

    <xsl:text>procedure </xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>.Tear_Down;&#10;</xsl:text>

    <xsl:call-template name="do-not-edit"/>

    <xsl:text>with </xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>.Events.Tear_Down;&#10;</xsl:text>

    <xsl:for-each select="class">
      <xsl:sort select="name"/>

      <xsl:text>with </xsl:text>
      <xsl:value-of select="../name"/>
      <xsl:text>.</xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text>.Tear_Down;&#10;</xsl:text>

    </xsl:for-each>

    <xsl:for-each select="type[@callback]">
      <xsl:sort select="name"/>

      <xsl:text>with </xsl:text>
      <xsl:value-of select="../name"/>
      <xsl:text>.</xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text>_Callback;&#10;</xsl:text>
    </xsl:for-each>

    <xsl:text>procedure </xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>.Tear_Down is&#10;</xsl:text>
    <xsl:text>begin&#10;</xsl:text>
    
    <xsl:value-of select="$I"/>
    <xsl:text>Events.Tear_Down;&#10;</xsl:text>

    <xsl:for-each select="class">
      <xsl:sort select="name"/>
      
      <xsl:value-of select="$I"/>
      <xsl:value-of select="name"/>
      <xsl:text>.Tear_Down;&#10;</xsl:text>
      
    </xsl:for-each>
    
    <xsl:for-each select="type[@callback]">
      <xsl:sort select="name"/>
      
      <xsl:value-of select="$I"/>
      <xsl:value-of select="name"/>
      <xsl:text>_Callback.Clear;&#10;</xsl:text>
    </xsl:for-each>
    
    <xsl:value-of select="$I"/>
    <xsl:text>Domain_Initialized := False;&#10;</xsl:text>

    <xsl:text>end </xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>.Tear_Down;&#10;</xsl:text>

    <xsl:apply-templates mode="class-teardown-spec"/>
    <xsl:apply-templates mode="class-teardown-body"/>

  </xsl:template>


  <xsl:template mode="class-teardown-spec" match="domain/class">
    <xsl:call-template name="do-not-edit"/>
    <xsl:text>procedure </xsl:text>
    <xsl:value-of select="../name"/>
    <xsl:text>.</xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>.Tear_Down;&#10;</xsl:text>
  </xsl:template>

  <xsl:template mode="class-teardown-spec" match="*"/>


  <xsl:template mode="class-teardown-body" match="domain/class">

    <!-- Calculate the maximum number of instances. -->
    <xsl:variable name="max">
      <xsl:call-template name="number-of-instances"/>
    </xsl:variable>

    <xsl:choose>
      
      <xsl:when test="$max=1">
        
        <!-- 
             with Ada.Unchecked_Deallocation;
             procedure {Domain}.{Class}.Tear_Down is
                procedure Free is new Ada.Unchecked_Deallocation (Instance, Handle);
             begin
                if This /= null then
                   {teardown} {(This)};                - if any
                   if not This.The_T'Terminated then   - if active
                      abort This.The_T.all;
                   end if;
                   Free (This.The_T);
                   Free (This);
                end if;
             end {Domain}.{Class}.Tear_Down;
             -->

        <xsl:call-template name="do-not-edit"/>
        
        <xsl:text>with Ada.Unchecked_Deallocation;&#10;</xsl:text>
        
        <xsl:text>procedure </xsl:text>
        <xsl:value-of select="../name"/>
        <xsl:text>.</xsl:text>
        <xsl:value-of select="name"/>
        <xsl:text>.Tear_Down is&#10;</xsl:text>
        
        <xsl:value-of select="$I"/>
        <xsl:text>procedure Free is new Ada.Unchecked_Deallocation (Instance, Handle);&#10;</xsl:text>
        
        <xsl:text>begin&#10;</xsl:text>
        
        <xsl:value-of select="$I"/>
        <xsl:text>if This /= null then&#10;</xsl:text>
        
        <xsl:for-each select="operation[@teardown]">
          <xsl:sort select="name"/>
          <xsl:call-template name="instance-teardown-call">
            <xsl:with-param name="indent" select="$II"/>
            <xsl:with-param name="param-name" select="'This'"/>
          </xsl:call-template>
        </xsl:for-each>
        
        <xsl:if test="@active">
          <xsl:value-of select="$II"/>
          <xsl:text>if not This.The_T'Terminated then&#10;</xsl:text>
          <xsl:value-of select="$III"/>
          <xsl:text>abort This.The_T.all;&#10;</xsl:text>
          <xsl:value-of select="$II"/>
          <xsl:text>end if;&#10;</xsl:text>
          <xsl:value-of select="$II"/>
          <xsl:text>Free (This.The_T);&#10;</xsl:text>
        </xsl:if>
        
        <xsl:value-of select="$II"/>
        <xsl:text>Free (This);&#10;</xsl:text>

        
        <xsl:value-of select="$I"/>
        <xsl:text>end if;&#10;</xsl:text>
        
        <xsl:text>end </xsl:text>
        <xsl:value-of select="../name"/>
        <xsl:text>.</xsl:text>
        <xsl:value-of select="name"/>
        <xsl:text>.Tear_Down;&#10;</xsl:text>
        
      </xsl:when>
      
      <xsl:otherwise>
        
        <!--
             with Ada.Unchecked_Deallocation;
             procedure {Domain}.{Class}.Tear_Down is
                use ColdFrame.Instances.Abstract_Containers;
                It : Iterator'Class := Maps.New_Iterator (The_Container);
                H : Handle;
                procedure Free is new Ada.Unchecked_Deallocation (Instance, Handle);
             begin
                while not Is_Done (It) loop
                   H := Handle (Current_Item (It));
                   {teardown} (H);                     - if any
                   if not H.The_T'Terminated then      - if active
                      abort H.The_T.all;
                   end if;
                   Free (H.The_T);
                   Free (H);
                   Next (It);
                end loop;
                Maps.Clear (The_Container);
                Next_Identifier := 0;                  -  for Autonumbering
             end {Domain}.{Class}.Tear_Down;
             -->

        <xsl:call-template name="do-not-edit"/>

        <xsl:text>with Ada.Unchecked_Deallocation;&#10;</xsl:text>

        <xsl:text>procedure </xsl:text>
        <xsl:value-of select="../name"/>
        <xsl:text>.</xsl:text>
        <xsl:value-of select="name"/>
        <xsl:text>.Tear_Down is&#10;</xsl:text>

        <xsl:value-of select="$I"/>
        <xsl:text>use ColdFrame.Instances.Abstract_Containers;&#10;</xsl:text>
        <xsl:value-of select="$I"/>
        <xsl:text>It : Iterator'Class := Maps.New_Iterator (The_Container);&#10;</xsl:text>
        <xsl:value-of select="$I"/>
        <xsl:text>H : Handle;&#10;</xsl:text>
        <xsl:value-of select="$I"/>
        <xsl:text>procedure Free is new Ada.Unchecked_Deallocation (Instance, Handle);&#10;</xsl:text>

        <xsl:text>begin&#10;</xsl:text>

        <xsl:value-of select="$I"/>
        <xsl:text>while not Is_Done (It) loop&#10;</xsl:text>
        <xsl:value-of select="$II"/>
        <xsl:text>H := Handle (Current_Item (It));&#10;</xsl:text>

        <xsl:for-each select="operation[@teardown]">
          <xsl:sort select="name"/>
          <xsl:call-template name="instance-teardown-call">
            <xsl:with-param name="indent" select="$II"/>
            <xsl:with-param name="param-name" select="'H'"/>
          </xsl:call-template>
        </xsl:for-each>

        <xsl:if test="@active">
          <xsl:value-of select="$II"/>
          <xsl:text>if not H.The_T'Terminated then&#10;</xsl:text>
          <xsl:value-of select="$III"/>
          <xsl:text>abort H.The_T.all;&#10;</xsl:text>
          <xsl:value-of select="$II"/>
          <xsl:text>end if;&#10;</xsl:text>
          <xsl:value-of select="$II"/>
          <xsl:text>Free (H.The_T);&#10;</xsl:text>
        </xsl:if>

        <xsl:value-of select="$II"/>
        <xsl:text>Free (H);&#10;</xsl:text>
        <xsl:value-of select="$II"/>
        <xsl:text>Next (It);&#10;</xsl:text>
        <xsl:value-of select="$I"/>
        <xsl:text>end loop;&#10;</xsl:text>

        <xsl:value-of select="$I"/>
        <xsl:text>Maps.Clear (The_Container);&#10;</xsl:text>        

        <!-- .. Autonumber support .. -->
        <xsl:if test="count(attribute[@identifier])=1
                      and attribute[@identifier]/type='Autonumber'">
          <xsl:value-of select="$I"/>
          <xsl:text>Next_Identifier := 0;&#10;</xsl:text>
        </xsl:if>

        <xsl:text>end </xsl:text>
        <xsl:value-of select="../name"/>
        <xsl:text>.</xsl:text>
        <xsl:value-of select="name"/>
        <xsl:text>.Tear_Down;&#10;</xsl:text>

      </xsl:otherwise>
    
    </xsl:choose>

  </xsl:template>

  <xsl:template mode="class-teardown-body" match="*"/>


  <!-- Called at domain to generate the spec of the Events teardown
       procedure. -->
  <xsl:template name="event-teardown-spec">
    
    <xsl:call-template name="do-not-edit"/>
    <xsl:text>procedure </xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>.Events.Tear_Down;&#10;</xsl:text>

  </xsl:template>


  <!-- Called at domain to generate the body of the Events teardown
       procedure. -->
  <xsl:template name="event-teardown-body">
    
    <xsl:call-template name="do-not-edit"/>
    <xsl:text>procedure </xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>.Events.Tear_Down is&#10;</xsl:text>
    <xsl:text>begin&#10;</xsl:text>
    <xsl:value-of select="$I"/>
    <xsl:text>ColdFrame.Project.Events.Tear_Down (Dispatcher);&#10;</xsl:text>
    <xsl:text>end </xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>.Events.Tear_Down;&#10;</xsl:text>

  </xsl:template>


  <!-- Called at class/operation to generate an instance teardown
       call. -->
  <xsl:template name="instance-teardown-call">

    <!-- The indentation to apply. -->
    <xsl:param name="indent"/>
    <!-- The name of the instance handle. -->
    <xsl:param name="param-name"/>

    <xsl:value-of select="$indent"/>
    <xsl:value-of select="name"/>
    <xsl:if test="not(../@singleton)">
      <xsl:text> (</xsl:text>
      <xsl:value-of select="$param-name"/>
      <xsl:text>)</xsl:text>
    </xsl:if>
    <xsl:text>;&#10;</xsl:text>

  </xsl:template>


</xsl:stylesheet>
