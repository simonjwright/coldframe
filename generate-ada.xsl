<!-- SId$ -->
<!-- Generate Ada code. -->
<!-- Copyright (C) Simon Wright <simon@pushface.org> -->

<xsl:stylesheet
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  version="1.0">
  <!--
  xmlns:adainit="http://www.pushface.org/java/ada_string_manipulation"
  xmlns:str="http://www.pushface.org/java/string_manipulation"
       -->

  <xsl:strip-space elements="*"/>

  <xsl:output method="text"/>

  <!-- Since the support code is created in Ada, we need to initialize
       the Ada runtime.
  <xsl:variable
    name="initialize-the-ada-runtime"
    select="adainit:adainit()"/>
  -->

  <!-- Generate the top-level package for the domain, then all the
       others -->
  <xsl:template match="domain">
    <xsl:apply-templates mode="domain-context"/>
    <xsl:text>package </xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text> is&#10;</xsl:text>
    <xsl:apply-templates select="type" mode="domain-type"/>
    <xsl:text>end </xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>;&#10;</xsl:text>
    <xsl:apply-templates select="object" mode="object-spec"/>
    <xsl:apply-templates select="object" mode="object-body"/>
    <xsl:apply-templates select="object/operation" mode="operation-separate"/>
  </xsl:template>

  <!-- Generate domain context clauses -->
  <xsl:template mode="domain-context" match="domain/type">
    <xsl:choose>
      <xsl:when test="string/max">
        <xsl:text>with Ada.Strings.Bounded;</xsl:text>
        <xsl:text> use Ada.Strings.Bounded;&#10;</xsl:text>
      </xsl:when>
    </xsl:choose>
  </xsl:template>

  <!-- generate domain Types entries (not for standard types) -->
  <xsl:template mode="domain-type" match="domain/type">
    <xsl:if test="not(standard)">
      <xsl:choose>
        <xsl:when test="enumeration">
          <xsl:text>  type </xsl:text>
          <xsl:value-of select="name"/>
          <xsl:text> is </xsl:text>
          <xsl:text>&#10;    (</xsl:text>
          <xsl:for-each select="enumeration/literal">
            <xsl:value-of select="."/>
            <xsl:if test="position() &lt; last()">
              <xsl:text>,&#10;     </xsl:text>
            </xsl:if>
          </xsl:for-each>
          <xsl:text>);&#10;</xsl:text>
        </xsl:when>
        <xsl:when test="integer">
          <xsl:text>  type </xsl:text>
          <xsl:value-of select="name"/>
          <xsl:text> is range </xsl:text>
          <xsl:value-of select="integer/lower"/>
          <xsl:text> .. </xsl:text>
          <xsl:value-of select="integer/upper"/>
          <xsl:text>;&#10;</xsl:text>
        </xsl:when>
        <xsl:when test="real">
          <xsl:text>  type </xsl:text>
          <xsl:value-of select="name"/>
          <xsl:text> is digits </xsl:text>
          <xsl:value-of select="real/digits"/>
          <xsl:text> range </xsl:text>
          <xsl:value-of select="real/lower"/>
          <xsl:text> .. </xsl:text>
          <xsl:value-of select="real/upper"/>
          <xsl:text>;&#10;</xsl:text>
        </xsl:when>
        <xsl:when test="string">
          <xsl:text>  package </xsl:text>
          <xsl:value-of select="name"/>
          <xsl:text>_Package is&#10;</xsl:text>
          <xsl:text>     new Generic_Bounded_Length (Max => </xsl:text>
          <xsl:value-of select="string/max"/>
          <xsl:text>);&#10;</xsl:text>
          <xsl:text>  subtype </xsl:text>
          <xsl:value-of select="name"/>
          <xsl:text> is </xsl:text>
          <xsl:value-of select="name"/>
          <xsl:text>_Package.Bounded_String;&#10;</xsl:text>
        </xsl:when>
        <xsl:otherwise>
          <xsl:text>  -- Unrecognised type category for </xsl:text>
          <xsl:value-of select="name"/>
          <xsl:text>&#10;</xsl:text>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:if>
  </xsl:template>

  <!-- Generate the class packages (specs). -->
  <xsl:template match="domain/object" mode="object-spec">
    <xsl:call-template name="object-context"/>
    <xsl:text>package </xsl:text>
    <xsl:value-of select="../name"/>.<xsl:value-of select="name"/>
    <xsl:text> is&#10;</xsl:text>
    <xsl:text>  Key : constant String := &quot;</xsl:text>
    <xsl:value-of select="key"/>
    <xsl:text>&quot;;&#10;</xsl:text>
    <xsl:text>  type T is private;&#10;</xsl:text>
    <xsl:apply-templates mode="operation-spec"/>
    <xsl:text>private&#10;</xsl:text>
    <xsl:call-template name="instance-record"/>
    <xsl:text>end </xsl:text>
    <xsl:value-of select="../name"/>.<xsl:value-of select="name"/>
    <xsl:text>;&#10;</xsl:text>
  </xsl:template>

  <!-- Called from domain/object to generate context clauses -->
  <xsl:template name="object-context">
    <xsl:if test="attribute/type='Unbounded_String'
                  or operation/parameter/type='Unbounded_String'">
      <xsl:text>with Ada.Strings.Unbounded;</xsl:text>
      <xsl:text> use Ada.Strings.Unbounded;&#10;</xsl:text>
    </xsl:if>
  </xsl:template>

  <!-- Called from domain/object to generate subprogram specs -->
  <xsl:template match="object/operation" mode="operation-spec">
    <xsl:call-template name="subprogram-specification">
       <xsl:with-param name="indent" select="'  '"/>
    </xsl:call-template>
    <xsl:text>;&#10;</xsl:text>
  </xsl:template>

  <!-- Generate the class packages (bodies). -->
  <xsl:template match="domain/object" mode="object-body">
    <xsl:if test="operation">
      <xsl:text>package body </xsl:text>
      <xsl:value-of select="../name"/>.<xsl:value-of select="name"/>
      <xsl:text> is&#10;</xsl:text>
      <xsl:apply-templates mode="operation-body"/>
      <xsl:text>end </xsl:text>
      <xsl:value-of select="../name"/>.<xsl:value-of select="name"/>
      <xsl:text>;&#10;</xsl:text>
    </xsl:if>
  </xsl:template>

  <!-- Called from domain/object to generate separate subprogram bodies -->
  <xsl:template match="object/operation" mode="operation-body">
    <xsl:call-template name="subprogram-specification">
       <xsl:with-param name="indent" select="'  '"/>
    </xsl:call-template>
    <xsl:text> is separate;&#10;</xsl:text>
   </xsl:template>

  <!-- Generate the separate bodies of operations. -->
  <xsl:template match="domain/object/operation" mode="operation-separate">
    <xsl:text>separate (</xsl:text>
    <xsl:value-of select="../../name"/>.<xsl:value-of select="../name"/>
    <xsl:text>)&#10;</xsl:text>
    <xsl:call-template name="subprogram-specification">
       <xsl:with-param name="indent" select="''"/>
    </xsl:call-template>
    <xsl:text> is&#10;</xsl:text>
    <xsl:text>begin&#10;</xsl:text>
    <xsl:choose>
      <xsl:when test="@return">
        <xsl:text>  raise Program_Error;&#10;</xsl:text>
        <xsl:text>  return </xsl:text>
        <xsl:call-template name="default-return-value">
          <xsl:with-param name="type" select="@return"/>
        </xsl:call-template>
        <xsl:text>;&#10;</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>  raise Program_Error;&#10;</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:text>end </xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>;&#10;</xsl:text>
  </xsl:template>

  <!-- Called from object/operation to generate a subprogram specification -->
  <xsl:template name="subprogram-specification">
    <xsl:param name="indent" select="'  '"/>
    <xsl:choose>
      <xsl:when test="@return">
        <xsl:value-of select="$indent"/>
        <xsl:text>function </xsl:text>
        <xsl:value-of select="name"/>
        <xsl:call-template name="parameter-list">
          <xsl:with-param name="indent" select="$indent"/>
        </xsl:call-template>
        <xsl:text> return </xsl:text>
        <xsl:call-template name="type-name">
          <xsl:with-param name="type" select="@return"/>
        </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$indent"/>
        <xsl:text>procedure </xsl:text>
        <xsl:value-of select="name"/>
        <xsl:call-template name="parameter-list">
          <xsl:with-param name="indent" select="$indent"/>
        </xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <!-- Called from object/operation to generate a subprogram parameter list -->
  <xsl:template name="parameter-list">
    <xsl:param name="indent" select="'  '"/>
    <xsl:text>&#10;</xsl:text>
    <xsl:value-of select="$indent"/>
    <xsl:text>  (This : T</xsl:text>
    <xsl:apply-templates mode="parameter">
      <xsl:with-param name="indent" select="$indent"/>
    </xsl:apply-templates>
    <xsl:text>)</xsl:text>
  </xsl:template>

  <!-- Called from object/operation to generate a subprogram parameter -->
  <xsl:template match="operation/parameter" mode="parameter">
    <xsl:param name="indent" select="'  '"/>
    <xsl:text>;&#10;</xsl:text>
    <xsl:value-of select="$indent"/>
    <xsl:text>   </xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text> : </xsl:text>
    <xsl:choose>
      <xsl:when test="@mode='inout'">
        <xsl:text>in out </xsl:text>
      </xsl:when>
      <xsl:when test="@mode='out'">
        <xsl:text>out </xsl:text>
      </xsl:when>
    </xsl:choose>
    <xsl:call-template name="type-name">
      <xsl:with-param name="type" select="type"/>
    </xsl:call-template>
    <xsl:if test="initial">
      <xsl:text> := </xsl:text>
      <xsl:value-of select="initial"/>
    </xsl:if>
  </xsl:template>

  <!-- Called from object/operation to generate a default return value for
       a separate function body -->
  <xsl:template name="default-return-value">
    <xsl:param name="type"/>
    <xsl:choose>
      <xsl:when test="$type='String'">
        <xsl:text>""</xsl:text>
      </xsl:when>
      <xsl:when test="$type='Unbounded_String'">
        <xsl:text>Null_Unbounded_String</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:variable name="the-type" select="../../type[name=$type]"/>
        <xsl:choose>
          <xsl:when test="$the-type/string/max">
            <xsl:value-of select="$type"/>
            <xsl:text>_Package.Null_Bounded_String</xsl:text>
          </xsl:when>
          <xsl:otherwise>
            <xsl:call-template name="type-name">
              <xsl:with-param name="type" select="$type"/>
            </xsl:call-template>
            <xsl:text>'First</xsl:text>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <!-- Called from domain/object to generate the actual instance
       record for the class. -->
  <xsl:template name="instance-record">
    <xsl:choose>
      <xsl:when test="count(attribute) &gt; 0">
        <xsl:text>  type T is record&#10;</xsl:text>
        <xsl:apply-templates
          mode="instance-record-component"
          select="attribute"/>
        <xsl:text>  end record;&#10;</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>  type T is null record;&#10;</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <!-- Generate the individual components of the class instance record. -->
  <xsl:template
    match="attribute"
    mode="instance-record-component">
    <xsl:text>    </xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text> : </xsl:text>
    <xsl:call-template name="type-name">
      <xsl:with-param name="type" select="type"/>
    </xsl:call-template>
    <xsl:text>;</xsl:text>
    <xsl:if test="@identifier or @referential">
      <xsl:text>  --</xsl:text>
      <xsl:if test="@identifier">
        <xsl:text> identifier</xsl:text>
        <xsl:if test="@referential">
          <xsl:text>,</xsl:text>
        </xsl:if>
      </xsl:if>
      <xsl:if test="@referential">
        <xsl:text> referential</xsl:text>
      </xsl:if>
    </xsl:if>
    <xsl:text>&#10;</xsl:text>
  </xsl:template>

  <!-- Handle special type name conversions -->
  <!--- none at present -->
  <xsl:template name="type-name">
    <xsl:param name="type"/>
    <xsl:value-of select="$type"/>
  </xsl:template>

  <!-- Catch unspecified default matches -->
  <xsl:template match="*"/>

  <!-- Catch unspecified mode="xxx" matches -->
  <xsl:template mode="domain-context" match="*"/>
  <xsl:template mode="domain-type" match="*"/>
  <xsl:template mode="instance-record-component" match="*"/>
  <xsl:template mode="object-spec" match="*"/>
  <xsl:template mode="object-body" match="*"/>
  <xsl:template mode="operation-spec" match="*"/>
  <xsl:template mode="operation-body" match="*"/>
  <xsl:template mode="operation-separate" match="*"/>
  <xsl:template mode="parameter" match="*"/>

</xsl:stylesheet>
