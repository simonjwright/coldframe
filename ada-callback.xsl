<!-- $Id: ada-callback.xsl,v c6ee965debf4 2004/10/19 16:12:49 simon $ -->
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
  xmlns:ut="http://pushface.org/coldframe/utilities"
  version="1.0">

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


</xsl:stylesheet>
