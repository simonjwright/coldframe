<!-- $Id: ada-serialization.xsl,v c8daa47c154b 2004/02/08 19:34:03 simon $ -->
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


  <!-- Called at domain to output the Serializable child package spec. -->
  <xsl:template name="serializable-type-spec">

    <xsl:call-template name="do-not-edit"/>
    <xsl:call-template name="identification-info"/>

    <xsl:if test="type/@serializable">
      <xsl:text>with ColdFrame.Project.Serialization;&#10;</xsl:text>
    </xsl:if>

    <xsl:text>package </xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>.Serializable is&#10;</xsl:text>

    <xsl:value-of select="$blank-line"/>

    <xsl:for-each select="type[@serializable]">
      <xsl:sort select="name"/>

      <!--
           type {name}
           is new ColdFrame.Project.Serialization.Base with record
           Payload : {domain}.{name};
           end record;
           function Image (S : {name}) return String;
           -->

      <xsl:value-of select="$I"/>
      <xsl:text>type </xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text>&#10;</xsl:text>
      <xsl:value-of select="$I"/>
      <xsl:text>is new ColdFrame.Project.Serialization.Base with record&#10;</xsl:text>
      <xsl:value-of select="$II"/>
      <xsl:text>Payload : </xsl:text>
      <xsl:value-of select="../name"/>
      <xsl:text>.</xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text>;&#10;</xsl:text>
      <xsl:value-of select="$I"/>
      <xsl:text>end record;&#10;</xsl:text>

      <xsl:value-of select="$blank-line"/>

      <xsl:value-of select="$I"/>
      <xsl:text>function Image (S : </xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text>) return String;&#10;</xsl:text>

      <xsl:value-of select="$blank-line"/>

    </xsl:for-each>

    <xsl:text>end </xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>.Serializable;&#10;</xsl:text>

  </xsl:template>


  <!-- called at domain to output a child package body if there
       are any serializable types. -->
  <xsl:template name="serializable-type-body">

    <xsl:if test="type/@serializable">

      <xsl:call-template name="do-not-edit"/>
      <xsl:call-template name="identification-info"/>
      <xsl:text>package body </xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text>.Serializable is&#10;</xsl:text>

      <xsl:value-of select="$blank-line"/>

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
           Name : constant String
             := "{domain}.{type}";
         begin
            return "<record name=""" & Name & """>" & ASCII.LF
              & Base_Attribute_Image (S)
              & "<field name=""{attr-name}"">"
              & S.Payload.{attr-name}'Img
              & "</field>" & ASCII.LF
              & "<field name=""{time-attr-name}"">"
              & ColdFrame.Project.Calendar.Image (S.Payload.{time-attr-name})
              & "</field>" & ASCII.LF
              & "<field name=""{ustring-attr-name}"">"
              & Ada.Strings.Unbounded.To_String (S.Payload.{ustring-attr-name})
              & "</field>" & ASCII.LF
              & "<field name=""{bstring-attr-name}"">"
              & {type}_Package.To_String (S.Payload.{bstring-attr-name})
              & "</field>" & ASCII.LF
              & {field-image} (S.Payload.{field-imaged-attr-name}, "field-imaged-attr-name") & ASCII.LF
              & "</record>";
         end Image;
         -->

    <xsl:value-of select="$I"/>
    <xsl:text>function Image (S : </xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>) return String is&#10;</xsl:text>
    <xsl:value-of select="$II"/>
    <xsl:text>Name : constant String :=&#10;</xsl:text>
    <xsl:value-of select="$IIC"/>
    <xsl:text>"</xsl:text>
    <xsl:value-of select="../name"/>
    <xsl:text>.</xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>";&#10;</xsl:text>
    <xsl:value-of select="$I"/>
    <xsl:text>begin&#10;</xsl:text>

    <xsl:value-of select="$II"/>
    <xsl:text>return "&lt;record name=""" &amp; Name &amp; """&gt;" &amp; ASCII.LF&#10;</xsl:text>
    <xsl:value-of select="$IIC"/>
    <xsl:text>&amp; Base_Attribute_Image (S)&#10;</xsl:text>

    <xsl:choose>

      <xsl:when test="attribute">

        <xsl:call-template name="image-of-type">
          <xsl:with-param name="type" select="."/>
        </xsl:call-template>

      </xsl:when>

      <xsl:otherwise>

        <xsl:call-template name="image-of-type">
          <xsl:with-param name="type" select="."/>
          <xsl:with-param name="name" select="name"/>
        </xsl:call-template>

      </xsl:otherwise>

    </xsl:choose>

    <xsl:value-of select="$IIC"/>
    <xsl:text>&amp; "&lt;/record&gt;";&#10;</xsl:text>

    <xsl:value-of select="$I"/>
    <xsl:text>end Image;&#10;</xsl:text>
    <xsl:value-of select="$blank-line"/>

  </xsl:template>

  <xsl:template match="*" mode="serializable-type-image-body"/>


  <!-- Called (at domain/type) to generate code to print a value of the
       type. -->
  <xsl:template name="image-of-type">

    <!-- The type to be processed. -->
    <xsl:param name="type"/>

    <!-- The Ada name to be used (eg, "Payload.X.Y". -->
    <xsl:param name="field" select="'Payload'"/>

    <!-- The text name to be used (eg, "X.Y" for the above). -->
    <xsl:param name="name" select="''"/>

    <xsl:choose>

      <xsl:when test="$type/array">
        <!-- This is an array type; not supported.. -->

        <xsl:call-template name="log-error"/>
        <xsl:message>
          <xsl:text>Error: can't create image for array </xsl:text>
          <xsl:value-of select="$type/name"/>
        </xsl:message>

      </xsl:when>

      <xsl:when test="$type/attribute">
        <!-- This is a composite type. -->

        <xsl:for-each select="$type/attribute">

          <xsl:variable name="print-name">
            <xsl:choose>
              <xsl:when test="$name">
                <xsl:value-of select="concat($name,'.',current()/name)"/>
              </xsl:when>
              <xsl:otherwise>
                <xsl:value-of select="name"/>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:variable>

          <xsl:call-template name="image-of-type">
            <xsl:with-param
              name="type"
              select="/domain/type[name=current()/type]"/>
            <xsl:with-param
              name="field"
              select="concat($field,'.',current()/name)"/>
            <xsl:with-param
              name="name"
              select="$print-name"/>
          </xsl:call-template>

        </xsl:for-each>

      </xsl:when>

      <xsl:otherwise>
        <!-- This is a simple type. -->

        <xsl:choose>

          <xsl:when test="$type/@null">
            <!-- Null record. -->
            <xsl:value-of select="$IIC"/>
            <xsl:text>&amp; "&lt;field name=""</xsl:text>
            <xsl:value-of select="$name"/>
            <xsl:text>""&gt;null&lt;/field&gt;" &amp; ASCII.LF&#10;</xsl:text>
          </xsl:when>

          <xsl:when test="$type/name='Date' or $type/name='Time'">
            <!-- Date/Time. -->
            <xsl:value-of select="$IIC"/>
            <xsl:text>&amp; "&lt;field name=""</xsl:text>
            <xsl:value-of select="$name"/>
            <xsl:text>""&gt;"&#10;</xsl:text>
            <xsl:value-of select="$IIC"/>
            <xsl:text>&amp; </xsl:text>
            <xsl:text>ColdFrame.Project.Calendar.Image (S.</xsl:text>
            <xsl:value-of select="$field"/>
            <xsl:text>)&#10;</xsl:text>
            <xsl:value-of select="$IIC"/>
            <xsl:text>&amp; "&lt;/field&gt;" &amp; ASCII.LF&#10;</xsl:text>
          </xsl:when>

          <xsl:when test="$type/name='Text' or $type/name='Unbounded_String'">
            <!-- Unbounded string. -->
            <xsl:value-of select="$IIC"/>
            <xsl:text>&amp; "&lt;field name=""</xsl:text>
            <xsl:value-of select="$name"/>
            <xsl:text>""&gt;"&#10;</xsl:text>
            <xsl:value-of select="$IIC"/>
            <xsl:text>&amp; </xsl:text>
            <xsl:text>Ada.Strings.Unbounded.To_String (S.</xsl:text>
            <xsl:value-of select="$field"/>
            <xsl:text>)&#10;</xsl:text>
            <xsl:value-of select="$IIC"/>
            <xsl:text>&amp; "&lt;/field&gt;" &amp; ASCII.LF&#10;</xsl:text>
          </xsl:when>

          <xsl:when test="$type/string">
            <!-- Bounded string. -->
            <xsl:value-of select="$IIC"/>
            <xsl:text>&amp; "&lt;field name=""</xsl:text>
            <xsl:value-of select="$name"/>
            <xsl:text>""&gt;"&#10;</xsl:text>
            <xsl:value-of select="$IIC"/>
            <xsl:text>&amp; </xsl:text>
            <xsl:value-of select="$type/name"/>
            <xsl:text>_Package.To_String (S.</xsl:text>
            <xsl:value-of select="$field"/>
            <xsl:text>)&#10;</xsl:text>
            <xsl:value-of select="$IIC"/>
            <xsl:text>&amp; "&lt;/field&gt;" &amp; ASCII.LF&#10;</xsl:text>
          </xsl:when>

          <xsl:when test="$type/@type-image">
            <!-- The type has a user-defined type image operation. -->
            <xsl:value-of select="$IIC"/>
            <xsl:text>&amp; "&lt;field name=""</xsl:text>
            <xsl:value-of select="$name"/>
            <xsl:text>""&gt;"&#10;</xsl:text>
            <xsl:value-of select="$IIC"/>
            <xsl:text>&amp; </xsl:text>
            <xsl:value-of select="$type/@type-image"/>
            <xsl:text> (S.</xsl:text>
            <xsl:value-of select="$field"/>
            <xsl:text>)&#10;</xsl:text>
            <xsl:value-of select="$IIC"/>
            <xsl:text>&amp; "&lt;/field&gt;" &amp; ASCII.LF&#10;</xsl:text>
          </xsl:when>

          <xsl:when test="$type/@field-image">
            <!-- The type has a user-defined field image operation. -->
            <xsl:value-of select="$IIC"/>
            <xsl:text>&amp; </xsl:text>
            <xsl:value-of select="$type/@field-image"/>
            <xsl:text> (S.</xsl:text>
            <xsl:value-of select="$field"/>
            <xsl:text>, "</xsl:text>
            <xsl:value-of select="$name"/>
            <xsl:text>") &amp; ASCII.LF&#10;</xsl:text>
          </xsl:when>

          <xsl:otherwise>
            <!-- Assume it's a scalar. -->
            <xsl:value-of select="$IIC"/>
            <xsl:text>&amp; "&lt;field name=""</xsl:text>
            <xsl:value-of select="$name"/>
            <xsl:text>""&gt;"&#10;</xsl:text>
            <xsl:value-of select="$IIC"/>
            <xsl:text>&amp; </xsl:text>
            <xsl:text>S.</xsl:text>
            <xsl:value-of select="$field"/>
            <xsl:text>'Img&#10;</xsl:text>
            <xsl:value-of select="$IIC"/>
            <xsl:text>&amp; "&lt;/field&gt;" &amp; ASCII.LF&#10;</xsl:text>
          </xsl:otherwise>

        </xsl:choose>

      </xsl:otherwise>

    </xsl:choose>

  </xsl:template>


</xsl:stylesheet>
