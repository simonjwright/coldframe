<!-- $Id: ada-callback.xsl,v 2d076122e3d3 2004/06/03 05:23:06 simon $ -->
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


<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="1.0">

  <xsl:template match="type[@callback]" mode="callback-spec">
    <!--
         with ColdFrame.Callbacks;
         package {domain}.{type}_Callback
         is new ColdFrame.Callbacks
            (T => {type});
         -->

    <xsl:call-template name="do-not-edit"/>
    <xsl:call-template name="identification-info"/>
    <xsl:text>pragma Style_Checks (Off);&#10;</xsl:text>

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

  <xsl:template match="*" mode="callback-spec"/>


</xsl:stylesheet>
