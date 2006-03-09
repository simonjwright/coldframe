<!-- $Id: ada-unittest.xsl,v a92983c044a0 2006/03/09 07:05:39 simonjwright $ -->
<!-- XSL stylesheet to generate Ada code for attribute peek/poke
     (for test only, please!). -->
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
  xmlns:un="http://pushface.org/coldframe/unittest"
  xmlns:ty="http://pushface.org/coldframe/type"
  xmlns:ut="http://pushface.org/coldframe/utilities"
  version="1.0">

  <xsl:template
    match="class[attribute[not(@refers)] or statemachine]"
    mode="un:unit-spec">

    <!--
         package {domain}.{class}.Unit_Test is
            type Timer_P is access constant ColdFrame.Project.Events.Timer;
            type State is
              (..);
            function Get_{attr-name}
              (This : Handle) return {attr-type};
            procedure Set_{attr-name)
              (This : Handle; To : {attr-type});
            function Access_{timer-attr-name}
              (This : Handle) return Timer_P;
         end {domain}.{class}.Unit_Test;
         -->

    <xsl:variable
      name="instance-needs-this"
      select="not(@public or @singleton or @utility)"/>

    <xsl:call-template name="ut:do-not-edit"/>
    <xsl:text>pragma Style_Checks (Off);&#10;</xsl:text>
    <xsl:call-template name="ut:identification-info"/>

    <xsl:text>package </xsl:text>
    <xsl:value-of select="../name"/>.<xsl:value-of select="name"/>
    <xsl:text>.Unit_Test is&#10;</xsl:text>

    <xsl:value-of select="$blank-line"/>

    <xsl:if test="attribute/type='Timer'">
      <xsl:value-of select="$I"/>
      <xsl:text>type Timer_P is access constant ColdFrame.Project.Events.Timer;&#10;</xsl:text>
      <xsl:value-of select="$blank-line"/>
    </xsl:if>

    <xsl:if test="statemachine">
      
      <!-- See ada-state.xsl. -->

      <xsl:value-of select="$I"/>
      <xsl:text>type State is&#10;</xsl:text>
      <xsl:value-of select="$IC"/>
      <xsl:text>(</xsl:text>

      <xsl:for-each select="statemachine/state/name">

        <!-- initial state first -->
        <!-- XXX final state last? -->
        <xsl:sort select="concat(not (../@initial),.)"/>

        <xsl:value-of select="."/>
        <xsl:if test="position() &lt; last()">
          <xsl:text>,&#10; </xsl:text>
          <xsl:value-of select="$IC"/>
        </xsl:if>

      </xsl:for-each>
      
      <xsl:text>);&#10;</xsl:text>
      <xsl:value-of select="$blank-line"/>
      
    </xsl:if>

    <xsl:for-each select="attribute[not(@refers) and not(type='Timer')]">
      <xsl:sort select="name"/>
      <xsl:value-of select="$I"/>
      <xsl:text>function Get_</xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text>&#10;</xsl:text>
      <xsl:value-of select="$IC"/>
      <xsl:choose>
        <xsl:when test="$instance-needs-this and not(@class)">
          <xsl:text>(This : Handle) return </xsl:text>
        </xsl:when>
        <xsl:otherwise>
          <xsl:text>return </xsl:text>
        </xsl:otherwise>
      </xsl:choose>
      <xsl:call-template name="ut:type-name">
        <xsl:with-param name="type" select="type"/>
        <xsl:with-param name="class" select=".."/>        
      </xsl:call-template>
      <xsl:text>;&#10;</xsl:text>
      <xsl:value-of select="$blank-line"/>
    </xsl:for-each>

    <xsl:for-each select="attribute
                          [not(@refers)
                          and not(@identifier)
                          and not(type='Timer')]">
      <xsl:sort select="name"/>
      <xsl:value-of select="$I"/>
      <xsl:text>procedure Set_</xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text>&#10;</xsl:text>
      <xsl:value-of select="$IC"/>
      <xsl:choose>
        <xsl:when test="$instance-needs-this and not(@class)">
          <xsl:text>(This : Handle; To : </xsl:text>
        </xsl:when>
        <xsl:otherwise>
          <xsl:text>(To : </xsl:text>
        </xsl:otherwise>
      </xsl:choose>
      <xsl:call-template name="ut:type-name">
        <xsl:with-param name="type" select="type"/>
        <xsl:with-param name="class" select=".."/>        
      </xsl:call-template>
      <xsl:text>);&#10;</xsl:text>
      <xsl:value-of select="$blank-line"/>
    </xsl:for-each>

    <xsl:if test="statemachine">

      <xsl:choose>
        <xsl:when test="$instance-needs-this">
          <xsl:value-of select="$I"/>
          <xsl:text>function Get_State_Machine_State (This : Handle) return State;&#10;</xsl:text>
          <xsl:value-of select="$blank-line"/>
          
          <xsl:value-of select="$I"/>
          <xsl:text>procedure Set_State_Machine_State (This : Handle; To : State);&#10;</xsl:text>
          <xsl:value-of select="$blank-line"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="$I"/>
          <xsl:text>function Get_State_Machine_State return State;&#10;</xsl:text>
          <xsl:value-of select="$blank-line"/>
          
          <xsl:value-of select="$I"/>
          <xsl:text>procedure Set_State_Machine_State (To : State);&#10;</xsl:text>
          <xsl:value-of select="$blank-line"/>
        </xsl:otherwise>
      </xsl:choose>

    <xsl:for-each select="attribute[type='Timer']">
      <xsl:sort select="name"/>
      <xsl:value-of select="$I"/>
      <xsl:text>function Access_</xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text>&#10;</xsl:text>
      <xsl:value-of select="$IC"/>
      <xsl:choose>
        <xsl:when test="$instance-needs-this and not(@class)">
          <xsl:text>(This : Handle) return Timer_P;&#10;</xsl:text>
        </xsl:when>
        <xsl:otherwise>
          <xsl:text>return Timer_P;&#10;</xsl:text>
        </xsl:otherwise>
      </xsl:choose>
      <xsl:value-of select="$blank-line"/>
    </xsl:for-each>

    </xsl:if>

    <xsl:text>end </xsl:text>
    <xsl:value-of select="../name"/>.<xsl:value-of select="name"/>
    <xsl:text>.Unit_Test;&#10;</xsl:text>

  </xsl:template>

  <xsl:template match="*" mode="un:unit-spec"/>


  <xsl:template
    match="class[attribute[not(@refers)] or statemachine]"
    mode="un:unit-body">

    <!--
         package body {domain}.{class}.Unit_Test is
            function Get_{attr-name}
              (This : Handle) return {attr-type} is
            begin
               return This.{attr-name}
            end Get_{attr-name};
            procedure Set_{attr-name)
              (This : Handle; To : {attr-type}) is
            begin
               This.{attr-name} := To;
            end Set_{attr-name);
            function Access_{timer-attr-name}
              (This : Handle) return Timer_P is
            begin
               return This.{timer-attr-name}'Unrestricted_Access;
            end Access_{timer-attr-name};
         end {domain}.{class}.Unit_Test;
         -->

    <xsl:call-template name="ut:do-not-edit"/>
    <xsl:text>pragma Style_Checks (Off);&#10;</xsl:text>
    <xsl:call-template name="ut:identification-info"/>

    <xsl:text>package body </xsl:text>
    <xsl:value-of select="../name"/>.<xsl:value-of select="name"/>
    <xsl:text>.Unit_Test is&#10;</xsl:text>

    <xsl:value-of select="$blank-line"/>

    <xsl:for-each select="attribute[not(@refers) and not(type='Timer')]">
      <xsl:sort select="name"/>
      <xsl:value-of select="$I"/>
      <xsl:text>function Get_</xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text>&#10;</xsl:text>
      <xsl:value-of select="$IC"/>
      <xsl:text>(This : Handle) return </xsl:text>
      <xsl:call-template name="ut:type-name">
        <xsl:with-param name="type" select="type"/>
        <xsl:with-param name="class" select=".."/>        
      </xsl:call-template>
      <xsl:text> is&#10;</xsl:text>
      <xsl:value-of select="$I"/>
      <xsl:text>begin&#10;</xsl:text>
      <xsl:value-of select="$II"/>
      <xsl:text>return This.</xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text>;&#10;</xsl:text>
      <xsl:value-of select="$I"/>
      <xsl:text>end Get_</xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text>;&#10;</xsl:text>
      <xsl:value-of select="$blank-line"/>
    </xsl:for-each>

    <xsl:for-each select="attribute
                          [not(@refers)
                          and not(@identifier)
                          and not(type='Timer')]">
      <xsl:sort select="name"/>
      <xsl:value-of select="$I"/>
      <xsl:text>procedure Set_</xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text>&#10;</xsl:text>
      <xsl:value-of select="$IC"/>
      <xsl:text>(This : Handle; To : </xsl:text>
      <xsl:call-template name="ut:type-name">
        <xsl:with-param name="type" select="type"/>
        <xsl:with-param name="class" select=".."/>        
      </xsl:call-template>
      <xsl:text>) is&#10;</xsl:text>
      <xsl:value-of select="$I"/>
      <xsl:text>begin&#10;</xsl:text>
      <xsl:value-of select="$II"/>
      <xsl:text>This.</xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text> := To;&#10;</xsl:text>
      <xsl:value-of select="$I"/>
      <xsl:text>end Set_</xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text>;&#10;</xsl:text>
      <xsl:value-of select="$blank-line"/>
    </xsl:for-each>

    <xsl:if test="statemachine">

      <xsl:value-of select="$I"/>
      <xsl:text>function Get_State_Machine_State (This : Handle) return State is&#10;</xsl:text>

      <xsl:value-of select="$II"/>
      <xsl:text>C : constant array (State_Machine_State_T) of State :=&#10;</xsl:text>
      <xsl:value-of select="$IIC"/>
      <xsl:text>(</xsl:text>

      <xsl:for-each select="statemachine/state/name">

        <!-- initial state first -->
        <!-- XXX final state last? -->
        <xsl:sort select="concat(not (../@initial),.)"/>

        <xsl:value-of select="."/>
        <xsl:text> =&gt; </xsl:text>
        <xsl:value-of select="."/>
        <xsl:if test="position() &lt; last()">
          <xsl:text>,&#10; </xsl:text>
          <xsl:value-of select="$IIC"/>
        </xsl:if>

      </xsl:for-each>

      <xsl:text>);&#10;</xsl:text>

      <xsl:value-of select="$I"/>
      <xsl:text>begin&#10;</xsl:text>
      <xsl:value-of select="$II"/>
      <xsl:text>return C (This.State_Machine_State);&#10;</xsl:text>
      <xsl:value-of select="$I"/>
      <xsl:text>end Get_State_Machine_State;&#10;</xsl:text>
      <xsl:value-of select="$blank-line"/>
      
      <xsl:value-of select="$I"/>
      <xsl:text>procedure Set_State_Machine_State (This : Handle; To : State) is&#10;</xsl:text>

      <xsl:value-of select="$II"/>
      <xsl:text>C : constant array (State) of State_Machine_State_T :=&#10;</xsl:text>
      <xsl:value-of select="$IIC"/>
      <xsl:text>(</xsl:text>

      <xsl:for-each select="statemachine/state/name">

        <!-- initial state first -->
        <!-- XXX final state last? -->
        <xsl:sort select="concat(not (../@initial),.)"/>

        <xsl:value-of select="."/>
        <xsl:text> =&gt; </xsl:text>
        <xsl:value-of select="."/>
        <xsl:if test="position() &lt; last()">
          <xsl:text>,&#10; </xsl:text>
          <xsl:value-of select="$IIC"/>
        </xsl:if>

      </xsl:for-each>

      <xsl:text>);&#10;</xsl:text>

      <xsl:value-of select="$I"/>
      <xsl:text>begin&#10;</xsl:text>
      <xsl:value-of select="$II"/>
      <xsl:text>This.State_Machine_State := C (To);&#10;</xsl:text>
      <xsl:value-of select="$I"/>
      <xsl:text>end Set_State_Machine_State;&#10;</xsl:text>
      <xsl:value-of select="$blank-line"/>

    </xsl:if>

    <xsl:for-each select="attribute[type='Timer']">
      <xsl:sort select="name"/>
      <xsl:value-of select="$I"/>
      <xsl:text>function Access_</xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text>&#10;</xsl:text>
      <xsl:value-of select="$IC"/>
      <xsl:text>(This : Handle) return Timer_P is&#10;</xsl:text>
      <xsl:value-of select="$I"/>
      <xsl:text>begin&#10;</xsl:text>
      <xsl:value-of select="$II"/>
      <xsl:text>return This.</xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text>'Unrestricted_Access;&#10;</xsl:text>
      <xsl:value-of select="$I"/>
      <xsl:text>end Access_</xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text>;&#10;</xsl:text>
      <xsl:value-of select="$blank-line"/>
    </xsl:for-each>

    <xsl:text>end </xsl:text>
    <xsl:value-of select="../name"/>.<xsl:value-of select="name"/>
    <xsl:text>.Unit_Test;&#10;</xsl:text>

  </xsl:template>

  <xsl:template match="*" mode="un:unit-body"/>


</xsl:stylesheet>
