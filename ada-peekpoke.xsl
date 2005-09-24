<!-- $Id: ada-peekpoke.xsl,v b0e347485d11 2005/09/24 16:27:11 simonjwright $ -->
<!-- XSL stylesheet to generate Ada code for Callbacksattribute peek/poke
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
  xmlns:pp="http://pushface.org/coldframe/peekpoke"
  xmlns:ty="http://pushface.org/coldframe/type"
  xmlns:ut="http://pushface.org/coldframe/utilities"
  version="1.0">

  <xsl:template
    match="class[attribute[not(@refers) and not(type='Timer')]
           or statemachine]"
    mode="pp:peekpoke-spec">

    <!--
         package {domain}.{class}.Peek_Poke is
            function Peek_{attr-name}
              (This : Handle) return {attr-type};
            procedure Poke_{attr-name)
              (This : Handle; To : {attr-type});
         end {domain}.{class}.Peek_Poke;
         -->

    <xsl:call-template name="ut:do-not-edit"/>
    <xsl:text>pragma Style_Checks (Off);&#10;</xsl:text>
    <xsl:call-template name="ut:identification-info"/>

    <xsl:text>package </xsl:text>
    <xsl:value-of select="../name"/>.<xsl:value-of select="name"/>
    <xsl:text>_Peek_Poke is&#10;</xsl:text>

    <xsl:value-of select="$blank-line"/>

    <xsl:for-each select="attribute[not(@refers) and not(type='Timer')]">
      <xsl:sort select="name"/>
      <xsl:value-of select="$I"/>
      <xsl:text>function Peek_</xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text>&#10;</xsl:text>
      <xsl:value-of select="$IC"/>
      <xsl:text>(This : Handle) return </xsl:text>
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
      <xsl:text>procedure Poke_</xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text>&#10;</xsl:text>
      <xsl:value-of select="$IC"/>
      <xsl:text>(This : Handle; To : </xsl:text>
      <xsl:call-template name="ut:type-name">
        <xsl:with-param name="type" select="type"/>
        <xsl:with-param name="class" select=".."/>        
      </xsl:call-template>
      <xsl:text>);&#10;</xsl:text>
      <xsl:value-of select="$blank-line"/>
    </xsl:for-each>

    <xsl:text>end </xsl:text>
    <xsl:value-of select="../name"/>.<xsl:value-of select="name"/>
    <xsl:text>_Peek_Poke;&#10;</xsl:text>

  </xsl:template>

  <xsl:template match="*" mode="pp:peekpoke-spec"/>


  <xsl:template
    match="class[attribute[not(@refers)] or statemachine]"
    mode="pp:peekpoke-body">

    <!--
         package body {domain}.{class}.Peek_Poke is
            function Peek_{attr-name}
              (This : Handle) return {attr-type} is
            begin
               return This.{attr-name}
            end Peek_{attr-name};
            procedure Poke_{attr-name)
              (This : Handle; To : {attr-type}) is
            begin
               This.{attr-name} := To;
            end Poke_{attr-name);
         end {domain}.{class}.Peek_Poke;
         -->

    <xsl:call-template name="ut:do-not-edit"/>
    <xsl:text>pragma Style_Checks (Off);&#10;</xsl:text>
    <xsl:call-template name="ut:identification-info"/>

    <xsl:text>package body </xsl:text>
    <xsl:value-of select="../name"/>.<xsl:value-of select="name"/>
    <xsl:text>_Peek_Poke is&#10;</xsl:text>

    <xsl:value-of select="$blank-line"/>

    <xsl:for-each select="attribute[not(@refers) and not(type='Timer')]">
      <xsl:sort select="name"/>
      <xsl:value-of select="$I"/>
      <xsl:text>function Peek_</xsl:text>
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
      <xsl:text>end Peek_</xsl:text>
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
      <xsl:text>procedure Poke_</xsl:text>
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
      <xsl:text>end Poke_</xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text>;&#10;</xsl:text>
      <xsl:value-of select="$blank-line"/>
    </xsl:for-each>

    <xsl:text>end </xsl:text>
    <xsl:value-of select="../name"/>.<xsl:value-of select="name"/>
    <xsl:text>_Peek_Poke;&#10;</xsl:text>

  </xsl:template>

  <xsl:template match="*" mode="pp:peekpoke-body"/>


</xsl:stylesheet>
