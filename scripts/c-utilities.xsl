<!-- XSL stylesheet, utilities to help generate C code. -->

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="1.0">

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
    <xsl:text>/*----------------------------------------*/&#10;</xsl:text>
    <xsl:text>/*  Automatically generated: do not edit  */&#10;</xsl:text>
    <xsl:text>/*----------------------------------------*/&#10;</xsl:text>
  </xsl:template>

  <xsl:template name="should-not-edit">
    <xsl:text>/*----------------------------------------------------*/&#10;</xsl:text>
    <xsl:text>/*  Automatically generated: should not need editing  */&#10;</xsl:text>
    <xsl:text>/*----------------------------------------------------*/&#10;</xsl:text>
  </xsl:template>

  <xsl:template name="could-edit">
    <xsl:text>/*---------------------------------------------*/&#10;</xsl:text>
    <xsl:text>/*  Automatically generated: may need editing  */&#10;</xsl:text>
    <xsl:text>/*---------------------------------------------*/&#10;</xsl:text>
  </xsl:template>

  <xsl:template name="should-edit">
    <xsl:text>/*---------------------------------------*/&#10;</xsl:text>
    <xsl:text>/*  Automatically generated: edit this!  */&#10;</xsl:text>
    <xsl:text>/*---------------------------------------*/&#10;</xsl:text>
  </xsl:template>


  <!-- Generate commentary. -->
  <xsl:template name="commentary">
    <!-- The current indentation. -->
    <xsl:param name="indent" select="''"/>
    <!-- Either a newline or an empty string. -->
    <xsl:param name="separate-pars" select="''"/>

    <!--
    <xsl:for-each select="documentation/par">

      <xsl:call-template name="comment-line">
        <xsl:with-param name="indent" select="$indent"/>
        <xsl:with-param name="line" select="normalize-space(.)"/>
      </xsl:call-template>

      <xsl:value-of select="$separate-pars"/>

    </xsl:for-each>
    -->

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
