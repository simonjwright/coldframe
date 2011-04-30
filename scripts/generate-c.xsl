<!-- XSL stylesheet to generate C code. -->
<!-- $Id$ -->

<xsl:stylesheet
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  version="1.0">

  <xsl:include href="c-utilities.xsl"/>

  <xsl:strip-space elements="*"/>

  <xsl:output method="text"/>


  <!-- +++++ Command line parameters. +++++ -->

  <!-- For identification info. -->
  <xsl:param name="coldframe-version" select="cf-DATE"/>

  <!-- Control indentation. -->
  <xsl:param name="standard-indent" select="'  '"/>
  <xsl:param name="continuation-indent" select="'   '"/>

  <!-- Control added blank lines: no or yes.. -->
  <xsl:param name="add-blank-lines" select="'yes'"/>

  <!-- Control verbosity: no or yes. -->
  <xsl:param name="verbose" select="'no'"/>

  <!-- Control comment paragraph fill width. -->
  <xsl:param name="fill-column" select="70"/>

  <!-- Global shorthands for indentation. -->
  <xsl:param name="I" select="$standard-indent"/>
  <xsl:param name="II" select="concat($I, $I)"/>
  <xsl:param name="III" select="concat($II, $I)"/>
  <xsl:param name="IIII" select="concat($III, $I)"/>
  <xsl:param name="IIIII" select="concat($IIII, $I)"/>
  <xsl:param name="C" select="$continuation-indent"/>
  <xsl:param name="IC" select="concat($I, $C)"/>
  <xsl:param name="IIC" select="concat($II, $C)"/>
  <xsl:param name="IIIC" select="concat($III, $C)"/>
  <xsl:param name="IIIIC" select="concat($IIII, $C)"/>
  <xsl:param name="IIIIIC" select="concat($IIIII, $C)"/>

  <!-- Added blank lines -->
  <xsl:param name="blank-line">
    <xsl:choose>
      <xsl:when test="$add-blank-lines='yes'">
        <xsl:text>&#10;</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:param>

  <!-- Remember the main document. -->
  <xsl:variable name="main-document" select="/"/>

  <!-- Generate the type declarations -->
  <xsl:template match="domain">

    <xsl:call-template name="do-not-edit"/>

    <!-- Identification info -->
    <xsl:call-template name="progress-message">
      <xsl:with-param name="m" select="'Generating identification info ..'"/>
    </xsl:call-template>
    <xsl:call-template name="identification-info"/>

    <!-- Commentary. -->
    <xsl:value-of select="$blank-line"/>
    <xsl:call-template name="commentary">
      <xsl:with-param name="separate-pars" select="$blank-line"/>
    </xsl:call-template>

    <!-- Include SNTP info if needed. -->
    <xsl:if test="type/attribute/type='SNTP_Timestamp'">
      <xsl:value-of select="$I"/>
      <xsl:text>struct sntp_timestamp {&#10;</xsl:text>
      <xsl:value-of select="$II"/>
      <xsl:text>unsigned long sec;&#10;</xsl:text>
      <xsl:value-of select="$II"/>
      <xsl:text>unsigned long frac;&#10;</xsl:text>
      <xsl:value-of select="$I"/>
      <xsl:text>};&#10;</xsl:text>
      <xsl:value-of select="$blank-line"/>
    </xsl:if>

    <xsl:for-each select="type[attribute]">
      <xsl:sort select="name"/>

      <xsl:value-of select="$I"/>
      <xsl:text>struct </xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text> {&#10;</xsl:text>

      <xsl:for-each select="attribute">

        <xsl:value-of select="$II"/>

        <xsl:choose>
          <xsl:when test="type='Integer_8'">
            <xsl:text>signed char</xsl:text>
          </xsl:when>
          <xsl:when test="type='Integer_16'">
            <xsl:text>short</xsl:text>
          </xsl:when>
          <xsl:when test="type='Integer_32'">
            <xsl:text>int</xsl:text>
          </xsl:when>
          <xsl:when test="type='Unsigned_8'">
            <xsl:text>unsigned char</xsl:text>
          </xsl:when>
          <xsl:when test="type='Unsigned_16'">
            <xsl:text>unsigned short</xsl:text>
          </xsl:when>
          <xsl:when test="type='Unsigned_32'">
            <xsl:text>unsigned</xsl:text>
          </xsl:when>
          <xsl:when test="type='Float_32'">
            <xsl:text>float</xsl:text>
          </xsl:when>
          <xsl:when test="type='Float_64'">
            <xsl:text>double</xsl:text>
          </xsl:when>
          <xsl:when test="type='SNTP_Timestamp'">
            <xsl:text>struct sntp_timestamp</xsl:text>
          </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="type"/>
          </xsl:otherwise>
        </xsl:choose>

        <xsl:text> </xsl:text>
        <xsl:value-of select="name"/>
        <xsl:text>;&#10;</xsl:text>
        
      </xsl:for-each>

      <xsl:value-of select="$I"/>
      <xsl:text>};&#10;</xsl:text>
      <xsl:value-of select="$blank-line"/>

    </xsl:for-each>
    
  </xsl:template>


  <!-- Called at domain to generate identification information. -->
  <xsl:template name="identification-info">
    <xsl:text>/*  Domain revision: </xsl:text>
    <xsl:choose>
      <xsl:when test="revision">
        <xsl:value-of select="revision"/>        
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>unspecified</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:text>  */&#10;</xsl:text>
    <xsl:text>/*  Extraction date: </xsl:text>
    <xsl:value-of select="date/day"/>
    <xsl:text> </xsl:text>
    <xsl:value-of select="date/month"/>
    <xsl:text> </xsl:text>
    <xsl:value-of select="date/year"/>
    <xsl:text>, </xsl:text>
    <xsl:value-of select="date/time"/>
    <xsl:text>  */&#10;</xsl:text>
    <xsl:text>/*  Extractor: </xsl:text>
    <xsl:value-of select="extractor"/>
    <xsl:text>  */&#10;</xsl:text>
    <xsl:text>/*  Normalizer: </xsl:text>
    <xsl:value-of select="normalizer"/>
    <xsl:text>  */&#10;</xsl:text>
    <xsl:text>/*  Generator: </xsl:text>
    <xsl:value-of select="$coldframe-version"/>
    <xsl:text>  */&#10;</xsl:text>
  </xsl:template>


  <!-- Catch unspecified default matches -->
  <xsl:template match="*"/>


</xsl:stylesheet>
