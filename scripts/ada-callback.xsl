<!-- $Id: ada-callback.xsl,v f3a9cc2c7d9c 2014/01/11 14:11:13 simonjwright $ -->
<!-- XSL stylesheet to generate Ada code for Callbacks. -->
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
  xmlns:cb="http://pushface.org/coldframe/callback"
  xmlns:ty="http://pushface.org/coldframe/type"
  xmlns:ut="http://pushface.org/coldframe/utilities"
  version="1.1">

  <xsl:template match="type[@callback]" mode="cb:callback-spec">
    <!--
         with ColdFrame.Callbacks;
         package {domain}.{type}_Callback
         is new ColdFrame.Callbacks
            (T => {type});
         -->

    <xsl:call-template name="ut:do-not-edit"/>
    <xsl:text>pragma Style_Checks (Off);&#10;</xsl:text>
    <xsl:call-template name="ut:identification-info"/>

    <xsl:text>with ColdFrame.Callbacks;&#10;</xsl:text>
    <xsl:text>package </xsl:text>
    <xsl:value-of select="../name"/>.<xsl:value-of select="name"/>
    <xsl:text>_Callback&#10;</xsl:text>
    <xsl:text>is new ColdFrame.Callbacks&#10;</xsl:text>
    <xsl:value-of select="$C"/>
    <xsl:text>(T =&gt; </xsl:text>
    <xsl:value-of select="../name"/>.<xsl:value-of select="name"/>
    <xsl:text>);&#10;</xsl:text>

  </xsl:template>

  <xsl:template match="*" mode="cb:callback-spec"/>


  <!-- Called at class to create the callback manager. -->
  <xsl:template
    mode="cb:create-callback-manager"
    match="operation[@callback]">
    <xsl:variable name="package-and-type">
      <xsl:call-template name="cb:package-and-type">
        <xsl:with-param
          name="type"
          select="../../type[name=current()/parameter/type]"/>
      </xsl:call-template>
    </xsl:variable>
    <xsl:variable name="package">
      <xsl:value-of select="../../name"/>
      <xsl:text>.</xsl:text>
      <xsl:value-of select="../name"/>
      <xsl:text>.</xsl:text>
      <xsl:value-of select="translate($package-and-type/package,'.', '_')"/>
      <xsl:text>_</xsl:text>
      <xsl:value-of select="$package-and-type/type"/>
   </xsl:variable>

    <xsl:call-template name="ut:do-not-edit"/>
    <xsl:text>pragma Style_Checks (Off);&#10;</xsl:text>
    <xsl:call-template name="ut:identification-info"/>

    <xsl:text>with ColdFrame.Project.Events.Callback_Manager;&#10;</xsl:text>
    <xsl:text>with </xsl:text>
    <xsl:value-of select="$package-and-type/package"/>
    <xsl:text>.</xsl:text>
    <xsl:value-of select="$package-and-type/type"/>
    <xsl:text>_Callback;&#10;</xsl:text>

    <xsl:text>package </xsl:text>
    <xsl:value-of select="../../name"/>
    <xsl:text>.</xsl:text>
    <xsl:value-of select="../name"/>
    <xsl:text>.</xsl:text>
    <xsl:value-of select="translate($package-and-type/package,'.', '_')"/>
    <xsl:text>_</xsl:text>
    <xsl:value-of select="$package-and-type/type"/>
    <xsl:text> is&#10;new ColdFrame.Project.Events.Callback_Manager.Callback_Manager_G&#10;</xsl:text>
    <xsl:value-of select="$C"/>
    <xsl:text>(T =&gt; </xsl:text>
    <xsl:value-of select="$package-and-type/package"/>
    <xsl:text>.</xsl:text>
    <xsl:value-of select="$package-and-type/type"/>
    <xsl:text>,&#10;</xsl:text>
    <xsl:value-of select="$C"/>
    <xsl:text> Callback => </xsl:text>
    <xsl:value-of select="$package-and-type/package"/>
    <xsl:text>.</xsl:text>
    <xsl:value-of select="$package-and-type/type"/>
    <xsl:text>_Callback);&#10;</xsl:text>

  </xsl:template>

  <xsl:template mode="cb:create-callback-manager" match="*"/>


  <!-- Called at class to output the context clause for users of the
       callback manager.  -->
  <xsl:template mode="cb:manager-context" match="operation[@callback]">
    <xsl:variable name="parameter-type" select="parameter/type"/>
    <xsl:variable name="package-and-type">
      <xsl:call-template name="cb:package-and-type">
        <xsl:with-param
          name="type"
          select="../../type[name=$parameter-type]"/>
      </xsl:call-template>
    </xsl:variable>

    <xsl:text>with </xsl:text>
    <xsl:value-of select="../../name"/>
    <xsl:text>.</xsl:text>
    <xsl:value-of select="../name"/>
    <xsl:text>.</xsl:text>
    <xsl:value-of select="translate($package-and-type/package,'.', '_')"/>
    <xsl:text>_</xsl:text>
    <xsl:value-of select="$package-and-type/type"/>
    <xsl:text>;&#10;</xsl:text>
  </xsl:template>

  <xsl:template mode="cb:manager-context" match="*"/>


  <!-- Called at class to initialize the callback manager. -->
  <xsl:template
    mode="cb:initialize-callback-manager"
    match="operation[@callback]">
    <xsl:variable name="package-and-type">
      <xsl:call-template name="cb:package-and-type">
        <xsl:with-param
          name="type"
          select="../../type[name=current()/parameter/type]"/>
      </xsl:call-template>
    </xsl:variable>
    <xsl:variable name="package">
      <xsl:value-of select="translate($package-and-type/package, '.', '_')"/>
      <xsl:text>_</xsl:text>
      <xsl:value-of select="$package-and-type/type"/>
    </xsl:variable>

    <xsl:value-of select="$I"/>
    <xsl:value-of select="$package"/>
    <xsl:text>.Register (Events.Dispatcher);&#10;</xsl:text>
    <xsl:value-of select="$I"/>
    <xsl:value-of select="$package"/>
    <xsl:text>.Register (</xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>'Access);&#10;</xsl:text>
  </xsl:template>

  <xsl:template mode="cb:initialize-callback-manager" match="*"/>


  <!-- Called at class to clear the callback manager.  -->
  <xsl:template mode="cb:manager-teardown" match="operation[@callback]">
    <xsl:variable name="package-and-type">
      <xsl:call-template name="cb:package-and-type">
        <xsl:with-param
          name="type"
          select="../../type[name=current()/parameter/type]"/>
      </xsl:call-template>
    </xsl:variable>

    <xsl:value-of select="$I"/>
    <xsl:value-of select="translate($package-and-type/package, '.', '_')"/>
    <xsl:text>_</xsl:text>
    <xsl:value-of select="$package-and-type/type"/>
    <xsl:text>.Clear;&#10;</xsl:text>
  </xsl:template>

  <xsl:template mode="cb:manager-teardown" match="*"/>


  <!-- Called with a //domain/type (used as the parameter of an
       operation, implying that callback management is required).
       Returns an element with elements package (the name of the
       source package, may be empty) and type (the name of the type in that
       package). -->
  <xsl:template name="cb:package-and-type">
    <xsl:param name="type" select="''"/>
    <xsl:choose>
      <xsl:when test="$type/imported">
        <xsl:element name="package">
          <xsl:value-of select="$type/imported"/>
        </xsl:element>
        <xsl:element name="type">
          <xsl:value-of select="$type/name"/>
        </xsl:element>
      </xsl:when>
      <xsl:when test="$type/renames">
        <xsl:variable name="renamed-from" select="$type/renames"/>
        <xsl:variable name="renamed-from-package">
          <xsl:call-template name="ty:find-source-package">
            <xsl:with-param
              name="input"
              select="$renamed-from"/>
          </xsl:call-template>
        </xsl:variable>
        <xsl:element name="package">
          <xsl:value-of select="$renamed-from-package"/>
        </xsl:element>
        <xsl:element name="type">
          <xsl:value-of
            select="substring-after($renamed-from,
                    concat($renamed-from-package, '.'))"/>
        </xsl:element>
      </xsl:when>
      <xsl:otherwise>
        <!-- Unusual usage. The package is the domain name. -->
        <xsl:element name="package">
          <xsl:value-of select="$type/../name"/>
        </xsl:element>
        <xsl:element name="type">
          <xsl:value-of select="$type/name"/>
        </xsl:element>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

</xsl:stylesheet>
