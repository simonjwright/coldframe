<!-- $Id: ada-serialization.xsl,v b48d7a3452c0 2003/01/26 19:08:19 simon $ -->
<!-- XSL stylesheet to generate Ada code for "serializable" types. -->
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
  version="1.0">


  <!-- Called at domain to output a child package spec if there
       are any serializable types. -->
  <xsl:template name="serializable-type-spec">

    <xsl:if test="type/@serializable">

      <xsl:call-template name="do-not-edit"/>
      <xsl:text>with ColdFrame.Project.Serialization;&#10;</xsl:text>
      <xsl:text>package </xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text>.Serializable is&#10;</xsl:text>

      <xsl:for-each select="type[@serializable]">
        <xsl:sort select="name"/>

            <!--
                 type {name} is new ColdFrame.Project.Serialization.Base with record
                   Payload : {domain}.{name};
                 end record;
                 function Image (S : {name}) return String;
                 -->

            <xsl:value-of select="$I"/>
            <xsl:text>type </xsl:text>
            <xsl:value-of select="name"/>
            <xsl:text> is new ColdFrame.Project.Serialization.Base with record&#10;</xsl:text>
            <xsl:value-of select="$II"/>
            <xsl:text>Payload : </xsl:text>
            <xsl:value-of select="../name"/>
            <xsl:text>.</xsl:text>
            <xsl:value-of select="name"/>
            <xsl:text>;&#10;</xsl:text>
            <xsl:value-of select="$I"/>
            <xsl:text>end record;&#10;</xsl:text>

            <xsl:value-of select="$I"/>
            <xsl:text>function Image (S : </xsl:text>
            <xsl:value-of select="name"/>
            <xsl:text>) return String;&#10;</xsl:text>
            
      </xsl:for-each>

      <xsl:text>end </xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text>.Serializable;&#10;</xsl:text>
      
    </xsl:if>
    
  </xsl:template>


  <!-- called at domain to output a child package body if there
       are any serializable types. -->
  <xsl:template name="serializable-type-body">
    
    <xsl:if test="type/@serializable">
      
      <xsl:call-template name="do-not-edit"/>
      <xsl:text>package body </xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text>.Serializable is&#10;</xsl:text>
      
      <xsl:apply-templates
        select="type[@serializable]"
        mode="serializable-type-image-body">
        <xsl:sort select="name"/>
      </xsl:apply-templates>
      
      <xsl:text>end </xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text>.Serializable;&#10;</xsl:text>
      
    </xsl:if>
    
  </xsl:template>
  

  <!-- Called to generate the implementation of Image for a serializable
       type. -->
  <xsl:template
    match="domain/type[@serializable]"
    mode="serializable-type-image-body">

    <!--
         function Image (S : Info) return String is
         begin
            return "<record name=""{domain}.{type}"">" & ASCII.LF
              & "<field name=""{attr-name}"">"
              & S.{attr-name}'Img
              & "</field>" & ASCII.LF
              & "<field name=""{time-attr-name}"">"
              & ColdFrame.Project.Calendar.Image (S.{time-attr-name})
              & "</field>" & ASCII.LF
              & "</record>" & ASCII.LF;
         end Image;
         -->
    
    <xsl:value-of select="$I"/>
    <xsl:text>function Image (S : </xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>) return String is&#10;</xsl:text>
    <xsl:value-of select="$I"/>
    <xsl:text>begin&#10;</xsl:text>

    <xsl:value-of select="$II"/>
    <xsl:text>return "&lt;record name=""</xsl:text>
    <xsl:value-of select="../name"/>
    <xsl:text>.</xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>""&gt;" &amp; ASCII.LF&#10;</xsl:text>

    <xsl:for-each select="attribute">

      <xsl:choose>

        <xsl:when test="type='Date' or type='Time'">
          <!-- Date and time have to be split over several lines. -->
          <xsl:value-of select="$IIC"/>
          <xsl:text>&amp; "&lt;field name=""</xsl:text>
          <xsl:value-of select="name"/>
          <xsl:text>""&gt;"&#10;</xsl:text>
          <xsl:value-of select="$IIC"/>
          <xsl:text>&amp; </xsl:text>
          <xsl:text>ColdFrame.Project.Calendar.Image (S.Payload.</xsl:text>
          <xsl:value-of select="name"/>
          <xsl:text>)&#10;</xsl:text>
          <xsl:value-of select="$IIC"/>
          <xsl:text>&amp; "&lt;/field&gt;" &amp; ASCII.LF&#10;</xsl:text>
        </xsl:when>

        <xsl:otherwise>
          <xsl:value-of select="$IIC"/>
          <xsl:text>&amp; "&lt;field name=""</xsl:text>
          <xsl:value-of select="name"/>
          <xsl:text>""&gt;" &amp; </xsl:text>
          <xsl:text>S.Payload.</xsl:text>
          <xsl:value-of select="name"/>
          <xsl:text>'Img &amp; "&lt;/field&gt;" &amp; ASCII.LF&#10;</xsl:text>
        </xsl:otherwise>

      </xsl:choose>

    </xsl:for-each>

    <xsl:value-of select="$IIC"/>
    <xsl:text>&amp; "&lt;/record&gt;" &amp; ASCII.LF;&#10;</xsl:text>

    <xsl:value-of select="$I"/>
    <xsl:text>end Image;&#10;</xsl:text>
    <xsl:value-of select="$blank-line"/>

  </xsl:template>

  <xsl:template match="*" mode="serializable-type-image-body"/>


</xsl:stylesheet>
