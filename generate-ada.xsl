<!-- SId$ -->
<!-- Generate Ada code. -->
<!-- Copyright (C) Simon Wright <simon@pushface.org> -->

<xsl:stylesheet
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:adainit="http://www.pushface.org/java/ada_string_manipulation"
  xmlns:str="http://www.pushface.org/java/string_manipulation"
  version="1.0">

  <xsl:strip-space elements="*"/>

  <xsl:output method="text"/>

  <!-- Since the support code is created in Ada, we need to initialize
       the Ada runtime. -->
  <xsl:variable
    name="initialize-the-ada-runtime"
    select="adainit:adainit()"/>

  <!-- Generate the top-level package for the domain, then all the
       others. -->
  <xsl:template match="domain">
    <xsl:variable name="name" select="str:identifier(name)"/>
    <xsl:text>package </xsl:text>
    <xsl:value-of select="$name"/>
    <xsl:text> is&#10;</xsl:text>
    <xsl:apply-templates select="objects/type" mode="generate-domain-types"/>
    <xsl:text>end </xsl:text>
    <xsl:value-of select="$name"/>
    <xsl:text>;&#10;</xsl:text>
    <xsl:apply-templates select="objects/object"/>
  </xsl:template>

  <!-- generate Types entries -->
  <xsl:template mode="generate-domain-types" match="domain/objects/type">
    <xsl:variable name="name" select="str:identifier(name)"/>
    <xsl:text>  type </xsl:text>
    <xsl:value-of select="$name"/>
    <xsl:text> is </xsl:text>
    <xsl:apply-templates mode="generate-domain-type"/>
    <xsl:text>;&#10;</xsl:text>
  </xsl:template>

  <!-- Generate enumeration type -->
  <xsl:template mode="generate-domain-type" match="enumeration">
    <xsl:text>&#10;    (</xsl:text>
    <xsl:for-each select="literal">
      <xsl:variable name="literal" select="str:identifier(.)"/>
      <xsl:value-of select="str:identifier(.)"/>
      <xsl:if test="position() &lt; last()">
        <xsl:text>,&#10;     </xsl:text>
      </xsl:if>
    </xsl:for-each>
    <xsl:text>)</xsl:text>
  </xsl:template>

  <!-- Generate real type -->
  <xsl:template mode="generate-domain-type" match="real">
    <xsl:text>digits </xsl:text>
    <xsl:value-of select="str:number(digits)"/>
    <xsl:text> range </xsl:text>
    <xsl:value-of select="str:number(lower)"/>
    <xsl:text> .. </xsl:text>
    <xsl:value-of select="str:number(upper)"/>
  </xsl:template>

  <!-- Generate the class packages. -->
  <xsl:template match="objects/object">
    <xsl:variable name="domain" select="str:identifier(../../name)"/>
    <xsl:variable name="name" select="str:identifier(name)"/>
    <xsl:text>package </xsl:text>
    <xsl:value-of select="$domain"/>.<xsl:value-of select="$name"/>
    <xsl:text> is&#10;</xsl:text>
    <xsl:text>  type T is private;&#10;</xsl:text>
    <xsl:text>private&#10;</xsl:text>
    <xsl:call-template name="instance-record"/>
    <xsl:text>end </xsl:text>
    <xsl:value-of select="$domain"/>.<xsl:value-of select="$name"/>
    <xsl:text>;&#10;</xsl:text>
  </xsl:template>

  <!-- Called from domain/objects/object to generate the actual instance
       record for the class. -->
  <xsl:template name="instance-record">
    <xsl:variable name="n" select="count(attributes/attribute)"/>
    <xsl:choose>
      <xsl:when test="$n &gt; 0">
        <xsl:text>  type T is record&#10;</xsl:text>
        <xsl:apply-templates
          mode="instance-record-component"
          select="attributes/attribute"/>
        <xsl:text>  end record;&#10;</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>  type T is null record;&#10;</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <!-- Generate the individual components of the class instance record. -->
  <xsl:template
    match="attributes/attribute"
    mode="instance-record-component">
    <xsl:variable name="name" select="str:identifier(name)"/>
    <xsl:variable name="type" select="str:identifier(type)"/>
    <xsl:text>    </xsl:text>
    <xsl:value-of select="$name"/>
    <xsl:text> : </xsl:text>
    <xsl:value-of select="$type"/>
    <xsl:text>;&#10;</xsl:text>
  </xsl:template>

  <!-- Catch unspecified default matches -->
  <xsl:template match="*"/>

  <!-- Catch unspecified mode="xxx" matches -->
  <xsl:template mode="generate-domain-types" match="*"/>
  <xsl:template mode="generate-domain-type" match="*"/>
  <xsl:template mode="instance-record-component" match="*"/>

</xsl:stylesheet>
