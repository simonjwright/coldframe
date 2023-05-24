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

<xsl:stylesheet
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:ut="http://pushface.org/coldframe/utilities"
  version="1.0"
>

  <!-- Generate commentary. -->
  <xsl:template name="ut:commentary">
    <!-- The current indentation. -->
    <xsl:param name="indent" select="''"/>
    <!-- Either a newline or an empty string. -->
    <xsl:param name="separate-pars" select="''"/>

    <xsl:for-each select="documentation/par">

      <xsl:call-template name="ut:comment-line">
        <xsl:with-param name="indent" select="$indent"/>
        <xsl:with-param name="line" select="normalize-space(.)"/>
      </xsl:call-template>

      <xsl:value-of select="$separate-pars"/>

    </xsl:for-each>

  </xsl:template>


  <!-- Output a paragraph of comment. -->
  <xsl:template name="ut:comment-line">
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

        <xsl:call-template name="ut:comment-line">
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

        <xsl:call-template name="ut:comment-line">
          <xsl:with-param name="indent" select="$indent"/>
          <xsl:with-param name="line" select="$line"/>
        </xsl:call-template>

      </xsl:when>

      <xsl:otherwise>

        <xsl:text> </xsl:text>
        <xsl:value-of select="$word"/>

        <xsl:call-template name="ut:comment-line">
          <xsl:with-param name="indent" select="$indent"/>
          <xsl:with-param name="line" select="$rest"/>
          <xsl:with-param name="length"
            select="$length + 1 + string-length($word)"/>
        </xsl:call-template>

      </xsl:otherwise>

    </xsl:choose>

  </xsl:template>


  <!-- User-overridable banner for each file. -->
  <xsl:template name="user-banner"/>


  <!-- "Could/Don't/Shouldn't/Should edit" banners. -->
  <xsl:template name="ut:could-edit">
    <xsl:call-template name="user-banner"/>
    <xsl:text>-------------------------------------------------&#10;</xsl:text>
    <xsl:text>--  Automatically generated: may need editing  --&#10;</xsl:text>
    <xsl:text>-------------------------------------------------&#10;</xsl:text>
  </xsl:template>

  <xsl:template name="ut:do-not-edit">
    <xsl:call-template name="user-banner"/>
    <xsl:text>--------------------------------------------&#10;</xsl:text>
    <xsl:text>--  Automatically generated: do not edit  --&#10;</xsl:text>
    <xsl:text>--------------------------------------------&#10;</xsl:text>
  </xsl:template>

  <xsl:template name="ut:should-not-edit">
    <xsl:call-template name="user-banner"/>
    <xsl:text>--------------------------------------------------------&#10;</xsl:text>
    <xsl:text>--  Automatically generated: should not need editing  --&#10;</xsl:text>
    <xsl:text>--------------------------------------------------------&#10;</xsl:text>
  </xsl:template>

  <xsl:template name="ut:should-edit">
    <xsl:call-template name="user-banner"/>
    <xsl:text>--------------------------------------------&#10;</xsl:text>
    <xsl:text>--  Automatically generated:  edit this!  --&#10;</xsl:text>
    <xsl:text>--  PLEASE DELETE THIS BANNER AFTERWARDS  --&#10;</xsl:text>
    <xsl:text>--------------------------------------------&#10;</xsl:text>
  </xsl:template>


  <!-- Called to generate identification information. -->
  <xsl:template name="ut:identification-info">
    <xsl:text>--  Domain revision: </xsl:text>
    <xsl:choose>
      <xsl:when test="/domain/revision">
        <xsl:value-of select="/domain/revision"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>unspecified</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:text>&#10;</xsl:text>
    <xsl:text>--  Extraction date: </xsl:text>
    <xsl:value-of select="/domain/date/day"/>
    <xsl:text> </xsl:text>
    <xsl:value-of select="/domain/date/month"/>
    <xsl:text> </xsl:text>
    <xsl:value-of select="/domain/date/year"/>
    <xsl:text>, </xsl:text>
    <xsl:value-of select="/domain/date/time"/>
    <xsl:text>&#10;</xsl:text>
    <xsl:text>--  Extractor: </xsl:text>
    <xsl:value-of select="/domain/extractor"/>
    <xsl:text>&#10;</xsl:text>
    <xsl:text>--  Normalizer: </xsl:text>
    <xsl:value-of select="/domain/normalizer"/>
    <xsl:text>&#10;</xsl:text>
    <xsl:text>--  Generator: cf-DATE&#10;</xsl:text>
  </xsl:template>

  <!-- Generates a distinctive text pattern in the generated code,
       to be detected during code generation to indicate failure.
       We used to use saxon:assign; no longer supported. -->
  <xsl:template name="ut:log-error">
    <xsl:text>&#10;[[[Model error detected hereabouts]]]&#10;</xsl:text>
  </xsl:template>

  <!-- Called at domain/class to compute number of instances.
       (If not at domain/class, set parameter "c" to the class
       for which the computation is required.)
       Outputs a number; 1000000000000 signals "unknown" (used to use
       +Inf but comparisons failed). -->
  <xsl:template name="ut:number-of-instances">
    <xsl:param name="c" select="."/>

    <xsl:variable name="name" select="$c/name"/>

    <xsl:choose>

      <xsl:when test="$c/@singleton">
        <xsl:value-of select="1"/>
      </xsl:when>

      <xsl:when test="$c/@public and count($c/attribute[not(@class)])=0">
        <xsl:value-of select="0"/>
      </xsl:when>

      <xsl:when test="$c/@public">
        <xsl:value-of select="1"/>
      </xsl:when>

      <xsl:when test="$c/@utility">
        <xsl:value-of select="0"/>
      </xsl:when>

      <xsl:when test="$c/@max">
        <xsl:value-of select="$c/@max"/>
      </xsl:when>

      <xsl:when test="/domain/inheritance/child=$name">

        <!-- Make a tree fragment roots/max where the max elements
             contain the max number of instances of each parent. This
             is a recursive call. -->
        <xsl:variable name="roots">
          <xsl:for-each select="/domain/inheritance[child=$name]/parent">
            <xsl:element name="max">
              <xsl:call-template name="ut:number-of-instances">
                <xsl:with-param
                  name="c"
                  select="/domain/class[name=current()]"/>
              </xsl:call-template>
            </xsl:element>
          </xsl:for-each>
        </xsl:variable>

        <!-- Sort into descending order and select the first element.
             See https://stackoverflow.com/a/4692734/40851. -->
        <xsl:for-each select="$roots/max">
          <xsl:sort
            select="."
            data-type="number"
            order="descending"/>
          <xsl:if test="position()=1">
            <xsl:value-of select="."/>
          </xsl:if>
        </xsl:for-each>
      </xsl:when>

      <xsl:when test="$c/associative">

        <xsl:variable name="role-1" select="$c/associative/role[1]"/>
        <xsl:variable name="role-2" select="$c/associative/role[2]"/>

        <xsl:choose>

          <xsl:when test="$role-1/@multiple and $role-2/@multiple">
            <xsl:variable name="n-1">
              <xsl:call-template name="ut:number-of-instances">
                <xsl:with-param
                  name="c"
                  select="/domain/class[name=$role-1/classname]"/>
              </xsl:call-template>
            </xsl:variable>
            <xsl:variable name="n-2">
              <xsl:call-template name="ut:number-of-instances">
                <xsl:with-param
                  name="c"
                  select="/domain/class[name=$role-2/classname]"/>
              </xsl:call-template>
            </xsl:variable>
            <xsl:variable name="result" select="$n-1 * $n-2"/>
            <xsl:choose>
              <xsl:when test="$result != $result">
                <!-- ie, result is NaN; happens with Inf * Inf. -->
                <xsl:value-of select="1000000000000"/>
              </xsl:when>
              <xsl:otherwise>
                <xsl:value-of select="$result"/>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:when>

          <xsl:when test="$role-1/@multiple">
            <xsl:call-template name="ut:number-of-instances">
              <xsl:with-param
                name="c"
                select="/domain/class[name=$role-1/classname]"/>
            </xsl:call-template>
          </xsl:when>

          <xsl:when test="$role-2/@multiple">
            <xsl:call-template name="ut:number-of-instances">
              <xsl:with-param
                name="c"
                select="/domain/class[name=$role-2/classname]"/>
            </xsl:call-template>
          </xsl:when>

          <xsl:otherwise>
            <xsl:variable name="n-1">
              <xsl:call-template name="ut:number-of-instances">
                <xsl:with-param
                  name="c"
                  select="/domain/class[name=$role-1/classname]"/>
              </xsl:call-template>
            </xsl:variable>
            <xsl:variable name="n-2">
              <xsl:call-template name="ut:number-of-instances">
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

          <xsl:when test="$type/name='Boolean'">2</xsl:when>

          <xsl:when test="$type/enumeration">
            <xsl:value-of select="count($type/enumeration/literal)"/>
          </xsl:when>

          <xsl:when test="$type/integer[lower and upper]
                          and number($type/integer/lower)
                          and number($type/integer/upper)">

            <!-- XXX should we get normalize to compute the length? -->

            <xsl:value-of select="$type/integer/upper
                                  - $type/integer/lower
                                  + 1"/>

          </xsl:when>

          <xsl:when test="$type/unsigned and number($type/unsigned/mod)">

            <xsl:value-of select="$type/unsigned/mod"/>

          </xsl:when>

          <xsl:otherwise>
            <xsl:value-of select="1000000000000"/>
          </xsl:otherwise>

        </xsl:choose>

      </xsl:when>

      <xsl:otherwise>
        <xsl:value-of select="1000000000000"/>
      </xsl:otherwise>

    </xsl:choose>

  </xsl:template>


  <!-- Called at domain/class to determine whether the container for
       the class's extent can be implemented as an array.
       (If not at domain/class, set parameter "c" to the class
       for which the computation is required.)
       Returns 'yes' or 'no'.
       -->
  <xsl:template name="ut:can-use-array">
    <xsl:param name="c" select="."/>

    <xsl:choose>

      <!-- There must only be one identifying attribute. -->
      <xsl:when test="count($c/attribute/@identifier)=1">

        <xsl:variable
          name="type"
          select="/domain/type[name=$c/attribute[@identifier]/type]"/>

        <!-- and it must be of the right kind. -->
        <xsl:choose>

          <!-- Assume all enumeration types are OK. -->
          <xsl:when test="$type/enumeration">yes</xsl:when>

          <!-- Imported types are OK if {hash=enumeration} -->
          <xsl:when test="$type/@hash='enumeration'">yes</xsl:when>

          <!-- Bounded integer types, if small enough. -->
          <xsl:when
            test="number($type/integer/lower) and number($type/integer/upper)">
            <xsl:choose>
              <xsl:when
                test="($type/integer/upper
                      - $type/integer/lower)
                      &lt; $max-hash-buckets">yes</xsl:when>
              <xsl:otherwise>no</xsl:otherwise>
            </xsl:choose>
          </xsl:when>

          <!-- Unsigned types, if small enough. -->
          <xsl:when test="number($type/unsigned/mod)">
            <xsl:choose>
              <xsl:when
                test="$type/unsigned/mod &lt; $max-hash-buckets">yes</xsl:when>
              <xsl:otherwise>no</xsl:otherwise>
            </xsl:choose>
          </xsl:when>

          <xsl:otherwise>no</xsl:otherwise>

        </xsl:choose>

      </xsl:when>

      <xsl:otherwise>no</xsl:otherwise>

    </xsl:choose>

  </xsl:template>


  <!-- Should container package parents be "Ada" or "ColdFrame"? -->
  <xsl:template name="ut:container-parent">
    <xsl:param name="c" select="."/>
    <xsl:variable name="max">
      <xsl:call-template name="ut:number-of-instances"/>
    </xsl:variable>

    <xsl:choose>
      <xsl:when
        test="$max &lt;= $max-bounded-container and $containers !=
              'standard'">ColdFrame</xsl:when>
      <xsl:otherwise>Ada</xsl:otherwise>
    </xsl:choose>

  </xsl:template>
  <!-- Progress messages. -->
  <xsl:template name="ut:progress-message">
    <xsl:param name="m"/>

    <xsl:if test="not($verbose='no')">
      <xsl:message>
        <xsl:value-of select="$m"/>
      </xsl:message>
    </xsl:if>

  </xsl:template>


  <!-- Handle special type name conversions. -->
  <xsl:template name="ut:type-name">

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

      <!-- If we're coding a class and this is the current class, it  maps to
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


  <!-- Called from operation to generate a dummy return value.  If not
       possible, returns nil (and the operation is expected to create
       a dummy of the type and return that). -->
  <xsl:template name="ut:dummy-return-value">

    <!-- The name of the type. -->
    <xsl:param name="type"/>

    <xsl:variable name="the-type" select="/domain/type[name=$type]"/>

    <xsl:choose>

      <xsl:when test="$the-type/@standard">

        <xsl:choose>

          <!-- Date or Time -->
          <xsl:when test="$type='Date' or $type='Time'">
            <xsl:text>ColdFrame.Project.Calendar.Clock</xsl:text>
          </xsl:when>

          <!-- Ordinary string -->
          <xsl:when test="$type='String'">
            <xsl:text>""</xsl:text>
          </xsl:when>

          <!-- Unbounded string -->
          <xsl:when test="$type='Unbounded_String' or $type='Text'">
            <xsl:text>Null_Unbounded_String</xsl:text>
          </xsl:when>

          <!-- Counterpart -->
          <xsl:when test="$type='Counterpart'">
            <xsl:text>null</xsl:text>
          </xsl:when>

          <!-- Handle (won't work for types, though) -->
          <xsl:when test="$type='Handle'">
            <xsl:text>null</xsl:text>
          </xsl:when>

          <!-- Other standard types are scalars (but may be renamed) -->
          <xsl:otherwise>
            <xsl:call-template name="ut:type-name">
              <xsl:with-param name="type" select="$type"/>
              <xsl:with-param name="is-class" select="false()"/>
            </xsl:call-template>
            <xsl:text>'First</xsl:text>
          </xsl:otherwise>

        </xsl:choose>

      </xsl:when>

      <xsl:otherwise>

        <xsl:choose>

          <!-- Access -->
          <xsl:when test="/domain/type/@access=$type">
            <xsl:text>null</xsl:text>
          </xsl:when>

          <!-- Bounded string -->
          <xsl:when test="$the-type/string/max">
            <xsl:value-of select="$type"/>
            <xsl:text>_Package.Null_Bounded_String</xsl:text>
          </xsl:when>

          <!-- Class -->
          <xsl:when test="/domain/class/name=$type">
            <xsl:text>null</xsl:text>
          </xsl:when>

          <!-- {counterpart} -->
          <xsl:when test="$the-type/counterpart">
            <xsl:text>null</xsl:text>
          </xsl:when>

          <!-- Fixed string -->
          <xsl:when test="$the-type/string/fixed">
            <xsl:text>(others =&gt; ' ')</xsl:text>
          </xsl:when>

          <!-- Scalars -->
          <xsl:when test="$the-type/enumeration
                          or $the-type/integer
                          or $the-type/real
                          or $the-type/unsigned">
            <xsl:call-template name="ut:type-name">
              <xsl:with-param name="type" select="$type"/>
            </xsl:call-template>
            <xsl:text>'First</xsl:text>
          </xsl:when>

          <!-- Otherwise, we don't know, so return nil. -->

        </xsl:choose>
      </xsl:otherwise>

    </xsl:choose>
  </xsl:template>


  <!-- with $package;
       pragma Warnings (Off, $package);
       -->
  <xsl:template name="ut:context-clause-suppress-warnings">
    <xsl:param name="package"/>

    <xsl:text>with </xsl:text>
    <xsl:value-of select="$package"/>
    <xsl:text>;&#10;</xsl:text>
    <xsl:text>pragma Warnings (Off, </xsl:text>
    <xsl:value-of select="$package"/>
    <xsl:text>);&#10;</xsl:text>

  </xsl:template>


</xsl:stylesheet>
