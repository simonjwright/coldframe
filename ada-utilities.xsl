<!-- $Id: ada-utilities.xsl,v 081bd0744b4b 2003/01/11 17:32:10 simon $ -->
<!-- XSL stylesheet, utilities to help generate Ada code. -->
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

  <!-- Called at domain/class to compute number of instances.
       If not, set parameter "c" to the class for which the computation
       is required.
       Outputs a number; +Inf signals "unknown". -->
  <xsl:template name="number-of-instances">
    <xsl:param name="c" select="."/>

    <xsl:variable name="name" select="$c/name"/>
    <xsl:variable
      name="assoc"
      select="/domain/association[associative=$name]"/>
        
    <xsl:choose>

      <xsl:when test="$assoc">

        <xsl:variable name="role-1" select="$assoc/role[1]"/>
        <xsl:variable name="role-2" select="$assoc/role[2]"/>

        <xsl:choose>
          
          <xsl:when test="$role-1/@multiple and $role-2/@multiple">
            <xsl:variable name="n-1">
              <xsl:call-template name="number-of-instances">
                <xsl:with-param
                  name="c"
                  select="/domain/class[name=$role-1/classname]"/>
              </xsl:call-template>
            </xsl:variable>
            <xsl:variable name="n-2">
              <xsl:call-template name="number-of-instances">
                <xsl:with-param
                  name="c"
                  select="/domain/class[name=$role-2/classname]"/>
              </xsl:call-template>
            </xsl:variable>
            <xsl:variable name="result" select="$n-1 * $n-2"/>
            <xsl:choose>
              <xsl:when test="$result != $result">
                <!-- ie, result is NaN; happens with Inf * Inf. -->
                <xsl:value-of select="1 div 0"/>
              </xsl:when>
              <xsl:otherwise>
                <xsl:value-of select="$result"/>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:when>

          <xsl:when test="$role-1/@multiple">
            <xsl:call-template name="number-of-instances">
              <xsl:with-param
                name="c"
                select="/domain/class[name=$role-1/classname]"/>
            </xsl:call-template>
          </xsl:when>

          <xsl:when test="$role-2/@multiple">
            <xsl:call-template name="number-of-instances">
              <xsl:with-param
                name="c"
                select="/domain/class[name=$role-2/classname]"/>
            </xsl:call-template>
          </xsl:when>

          <xsl:otherwise>
            <xsl:variable name="n-1">
              <xsl:call-template name="number-of-instances">
                <xsl:with-param
                  name="c"
                  select="/domain/class[name=$role-1/classname]"/>
              </xsl:call-template>
            </xsl:variable>
            <xsl:variable name="n-2">
              <xsl:call-template name="number-of-instances">
                <xsl:with-param
                  name="c"
                  select="/domain/class[name=$role-2/classname]"/>
              </xsl:call-template>
            </xsl:variable>
            <xsl:choose>
              <xsl:when test="$n-1 &lt; $n-2">
                <xsl:value-of select="$n-1"/>
              </xsl:when>
              <xsl:otherwise>
                <xsl:value-of select="$n-2"/>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:otherwise>

        </xsl:choose>

      </xsl:when>

      <xsl:when test="count($c/attribute/@identifier)=1">

        <xsl:variable name="type-name" select="$c/attribute[@identifier]/type"/>
        <xsl:variable name="type" select="/domain/type[name=$type-name]"/>
        
        <xsl:choose>

          <xsl:when test="$type/enumeration">
            <xsl:value-of select="count($type/enumeration/literal)"/>
          </xsl:when>

          <xsl:when test="$type/integer[lower and upper]">

            <xsl:value-of select="$type/integer/upper
                                  - $type/integer/lower
                                  + 1"/>
            
          </xsl:when>

          <xsl:when test="$c/@max">
            <xsl:value-of select="$c/@max"/>
          </xsl:when>

          <xsl:otherwise>
            <xsl:value-of select="1 div 0"/>
          </xsl:otherwise>

        </xsl:choose>

      </xsl:when>

      <xsl:when test="$c/@max">
        <xsl:value-of select="$c/@max"/>
      </xsl:when>

      <xsl:otherwise>
        <xsl:value-of select="1 div 0"/>
      </xsl:otherwise>

    </xsl:choose>
    
  </xsl:template>


  <!-- Progress messages. -->
  <xsl:template name="progress-message">
    <xsl:param name="m"/>

    <xsl:if test="not($verbose='no')">
      <xsl:message>
        <xsl:value-of select="$m"/>
      </xsl:message>
    </xsl:if>

  </xsl:template>


  <!-- "Don't edit" warnings. -->
  <xsl:template name="do-not-edit">
    <xsl:text>--------------------------------------------&#10;</xsl:text>
    <xsl:text>--  Automatically generated: do not edit  --&#10;</xsl:text>
    <xsl:text>--------------------------------------------&#10;</xsl:text>
  </xsl:template>

  <xsl:template name="should-not-edit">
    <xsl:text>--------------------------------------------------------&#10;</xsl:text>
    <xsl:text>--  Automatically generated: should not need editing  --&#10;</xsl:text>
    <xsl:text>--------------------------------------------------------&#10;</xsl:text>
  </xsl:template>

  <xsl:template name="should-edit">
    <xsl:text>-------------------------------------------&#10;</xsl:text>
    <xsl:text>--  Automatically generated: edit this!  --&#10;</xsl:text>
    <xsl:text>-------------------------------------------&#10;</xsl:text>
  </xsl:template>


  <!-- Handle special type name conversions. -->
  <xsl:template name="type-name">

    <!-- The name of the type to be generated. -->
    <xsl:param name="type"/>

    <!-- The current object (class or type). -->
    <xsl:param name="class" select="/.."/>

    <!-- Does the current object map to Handle? -->
    <xsl:param name="is-class" select="'yes'"/>

    <xsl:choose>

      <!-- Autonumber maps to Long Integer. -->
      <xsl:when test="$type='Autonumber'">
        <xsl:text>Long_Long_Integer</xsl:text>
      </xsl:when>

      <!-- If we're coding a class and this is The current class, it  maps to
           just Handle. -->
      <xsl:when test="$type=$class/name and $is-class='yes'">
        <xsl:text>Handle</xsl:text>
      </xsl:when>

      <!-- A Class (not the current class) maps to
           ColdFrame.Instances.Handle. -->
      <xsl:when test="/domain/class/name=$type">
        <xsl:text>ColdFrame.Instances.Handle</xsl:text>
      </xsl:when>

      <!-- Counterpart maps to ColdFrame.Instances.Handle. -->
      <xsl:when test="$type='Counterpart'">
        <xsl:text>ColdFrame.Instances.Handle</xsl:text>
      </xsl:when>

      <!-- Date maps to ColdFrame.Project.Calendar.Time. -->
      <xsl:when test="$type='Date'">
        <xsl:text>ColdFrame.Project.Calendar.Time</xsl:text>
      </xsl:when>

      <!-- Real maps to Float. -->
      <xsl:when test="$type='Real'">
        <xsl:text>Float</xsl:text>
      </xsl:when>

      <!-- Text maps to Unbounded_String. -->
      <xsl:when test="$type='Text'">
        <xsl:text>Unbounded_String</xsl:text>
      </xsl:when>

      <!-- Time maps to ColdFrame.Project.Calendar.Time. -->
      <xsl:when test="$type='Time'">
        <xsl:text>ColdFrame.Project.Calendar.Time</xsl:text>
      </xsl:when>

      <!-- Timer maps to ColdFrame.Project.Events.Timer. -->
      <xsl:when test="$type='Timer'">
        <xsl:text>ColdFrame.Project.Events.Timer</xsl:text>
      </xsl:when>

      <xsl:otherwise>
        <xsl:value-of select="$type"/>
      </xsl:otherwise>

    </xsl:choose>
  </xsl:template>


  <!-- Generate commentary. -->
  <xsl:template name="commentary">
    <!-- The current indentation. -->
    <xsl:param name="indent" select="''"/>
    <!-- Either a newline or an empty string. -->
    <xsl:param name="separate-pars" select="''"/>

    <xsl:for-each select="documentation/par">
      
      <xsl:call-template name="comment-line">
        <xsl:with-param name="indent" select="$indent"/>
        <xsl:with-param name="line" select="normalize-space(.)"/>
      </xsl:call-template>
      
      <xsl:value-of select="$separate-pars"/>
    
    </xsl:for-each>

  </xsl:template>


  <!-- Output a paragraph of comment. -->
  <xsl:template name="comment-line">
    <!-- The current indentation. -->
    <xsl:param name="indent"/>
    <!-- The rest of the line to be output. -->
    <xsl:param name="line"/>
    <!-- The length of text output so far. -->
    <xsl:param name="length" select="0"/>

    <xsl:variable name="word">
      <xsl:choose>
        <xsl:when test="contains($line, ' ')">
          <xsl:value-of select="substring-before($line, ' ')"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="$line"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:variable name="rest" select="substring-after($line, ' ')"/>

    <xsl:choose>

      <xsl:when test="$length=0 and string-length($line)&gt;0">

        <xsl:variable name="start">
          <xsl:value-of select="$indent"/>
          <xsl:text>--  </xsl:text>
          <xsl:value-of select="$word"/>
        </xsl:variable>
        
        <xsl:value-of select="$start"/>

        <xsl:call-template name="comment-line">
          <xsl:with-param name="indent" select="$indent"/>
          <xsl:with-param name="line" select="$rest"/>
          <xsl:with-param name="length" select="string-length($start)"/>
        </xsl:call-template>

      </xsl:when>

      <xsl:when test="string-length($line)=0">
        <xsl:text>&#10;</xsl:text>
      </xsl:when>

      <xsl:when test="$length
                      + 1
                      + string-length($word)&gt;$fill-column">

        <xsl:text>&#10;</xsl:text>
        
        <xsl:call-template name="comment-line">
          <xsl:with-param name="indent" select="$indent"/>
          <xsl:with-param name="line" select="$line"/>
        </xsl:call-template>

      </xsl:when>

      <xsl:otherwise>
        
        <xsl:text> </xsl:text>
        <xsl:value-of select="$word"/>

        <xsl:call-template name="comment-line">
          <xsl:with-param name="indent" select="$indent"/>
          <xsl:with-param name="line" select="$rest"/>
          <xsl:with-param name="length"
            select="$length + 1 + string-length($word)"/>
        </xsl:call-template>

      </xsl:otherwise>
      
    </xsl:choose>

  </xsl:template>


</xsl:stylesheet>
