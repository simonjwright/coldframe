<!-- $Id: ada-association.xsl,v 8faf8a6104f2 2005/06/13 21:29:53 simonjwright $ -->
<!-- XSL stylesheet to generate Ada code for Associations. -->
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
  xmlns:as="http://pushface.org/coldframe/association"
  xmlns:at="http://pushface.org/coldframe/attribute"
  xmlns:ut="http://pushface.org/coldframe/utilities"
  version="1.0">

  <!-- Generate specs for Association packages. -->
  <xsl:template match="domain/association" mode="as:association-spec">

    <xsl:call-template name="as:check-association-validity"/>

    <xsl:call-template name="ut:do-not-edit"/>
    <xsl:text>pragma Style_Checks (Off);&#10;</xsl:text>
    <xsl:call-template name="ut:identification-info"/>

    <!-- Commentary. -->
    <xsl:value-of select="$blank-line"/>
    <xsl:call-template name="ut:commentary">
      <xsl:with-param name="separate-pars" select="$blank-line"/>
    </xsl:call-template>

    <!-- Context clauses. -->
    <xsl:call-template name="as:association-spec-context"/>

    <xsl:text>private package </xsl:text>
    <xsl:value-of select="../name"/>.<xsl:value-of select="name"/>
    <xsl:text> is&#10;</xsl:text>
    <xsl:value-of select="$blank-line"/>

    <!-- Linking subprogram .. -->
    <xsl:call-template name="as:link-spec"/>

    <!-- Finding function for associative classes .. -->
    <xsl:call-template name="as:find-spec"/>

    <!-- .. unlinking procedure .. -->
    <xsl:call-template name="as:unlink-spec"/>

    <!-- .. navigations .. -->
    <xsl:call-template name="as:navigation-specs"/>

    <!-- .. and close. -->
    <xsl:text>end </xsl:text>
    <xsl:value-of select="../name"/>.<xsl:value-of select="name"/>
    <xsl:text>;&#10;</xsl:text>

  </xsl:template>

  <xsl:template match="*" mode="as:association-spec"/>


  <!-- Called at domain/association to check for invalid associations. -->
  <xsl:template name="as:check-association-validity">

    <xsl:variable
      name="class-a"
      select="../class[name=current()/role[1]/classname]"/>
    <xsl:variable
      name="class-b"
      select="../class[name=current()/role[2]/classname]"/>

    <xsl:if test="$class-a/@public">
      <xsl:call-template name="ut:log-error"/>
      <xsl:message>
        <xsl:text>Error: illegal association </xsl:text>
        <xsl:value-of select="name"/>
        <xsl:text> with class </xsl:text>
        <xsl:value-of select="$class-a/name"/>
      </xsl:message>
    </xsl:if>

    <xsl:if test="$class-b/@public">
      <xsl:call-template name="ut:log-error"/>
      <xsl:message>
        <xsl:text>Error: illegal association </xsl:text>
        <xsl:value-of select="name"/>
        <xsl:text> with class </xsl:text>
        <xsl:value-of select="$class-b/name"/>
      </xsl:message>
    </xsl:if>

  </xsl:template>


  <!-- Called at domain/association to generate context clauses for the
       spec. -->
  <xsl:template name="as:association-spec-context">

    <!-- XXX will duplicate withs for reflexive associations -->

    <xsl:variable name="role-a" select="role[1]"/>
    <xsl:variable name="role-b" select="role[2]"/>
    <xsl:variable name="a" select="$role-a/classname"/>
    <xsl:variable name="b" select="$role-b/classname"/>

    <xsl:text>with </xsl:text>
    <xsl:value-of select="/domain/name"/>
    <xsl:text>.</xsl:text>
    <xsl:value-of select="$a"/>
    <xsl:if test="$role-a/@multiple">  <!-- stupid to have a singleton! -->
      <xsl:text>.Collections</xsl:text>
    </xsl:if>
    <xsl:text>;&#10;</xsl:text>

    <xsl:text>with </xsl:text>
    <xsl:value-of select="/domain/name"/>
    <xsl:text>.</xsl:text>
    <xsl:value-of select="$b"/>
    <xsl:if test="$role-b/@multiple">
      <xsl:text>.Collections</xsl:text>
    </xsl:if>
    <xsl:text>;&#10;</xsl:text>

    <xsl:if test="associative">
      <xsl:text>with </xsl:text>
      <xsl:value-of select="/domain/name"/>
      <xsl:text>.</xsl:text>
      <xsl:value-of select="associative"/>
      <xsl:if test="role/@multiple">
        <xsl:text>.Collections</xsl:text>
      </xsl:if>
      <xsl:text>;&#10;</xsl:text>
    </xsl:if>

  </xsl:template>


  <!-- Called at domain/association to generate the linking subprogram spec -->
  <xsl:template name="as:link-spec">

    <xsl:choose>

      <xsl:when test="associative">
        <xsl:call-template name="as:link-function-specification"/>
      </xsl:when>

      <xsl:otherwise>
        <xsl:call-template name="as:link-procedure-specification"/>
      </xsl:otherwise>

    </xsl:choose>

    <xsl:text>;&#10;</xsl:text>
    <xsl:value-of select="$blank-line"/>

  </xsl:template>


  <!-- Called at domain/association to generate the find function spec
       if any. -->
  <xsl:template name="as:find-spec">
    <xsl:if test="associative">
      <xsl:call-template name="as:association-find-specification"/>
      <xsl:text>;&#10;</xsl:text>
      <xsl:value-of select="$blank-line"/>
    </xsl:if>
  </xsl:template>


  <!-- Called at domain/association to generate the unlinking procedure
       spec -->
  <xsl:template name="as:unlink-spec">

    <xsl:choose>

      <xsl:when test="associative">
        <xsl:call-template name="as:unlink-associative-specification"/>
      </xsl:when>

      <xsl:otherwise>
        <xsl:call-template name="as:unlink-specification"/>
      </xsl:otherwise>

    </xsl:choose>

    <xsl:text>;&#10;</xsl:text>
    <xsl:value-of select="$blank-line"/>

  </xsl:template>


  <!-- Generate bodies for Association packages. -->
  <xsl:template match="domain/association" mode="as:association-body">

    <xsl:call-template name="ut:do-not-edit"/>
    <xsl:text>pragma Style_Checks (Off);&#10;</xsl:text>
    <xsl:call-template name="ut:identification-info"/>

    <!-- Context clauses. -->
    <xsl:call-template name="as:association-body-context"/>

    <xsl:text>package body </xsl:text>
    <xsl:value-of select="../name"/>.<xsl:value-of select="name"/>
    <xsl:text> is&#10;</xsl:text>
    <xsl:value-of select="$blank-line"/>

    <!-- Suppress warnings. -->
    <!-- XXX does this need to be more selective? -->
    <xsl:value-of select="$I"/>
    <xsl:text>pragma Warnings (Off);&#10;</xsl:text>
    <xsl:value-of select="$blank-line"/>

    <!-- Linking subprogram .. -->
    <xsl:call-template name="as:link-body"/>

    <!-- Finding function for associative classes .. -->
    <xsl:call-template name="as:find-body"/>

    <!-- .. unlinking procedure .. -->
    <xsl:call-template name="as:unlink-body"/>

    <!-- .. navigations .. -->
    <xsl:call-template name="as:navigation-bodies"/>

    <!-- .. and close. -->
    <xsl:text>end </xsl:text>
    <xsl:value-of select="../name"/>.<xsl:value-of select="name"/>
    <xsl:text>;&#10;</xsl:text>

  </xsl:template>

  <xsl:template match="*" mode="as:association-body"/>


  <!-- Called at domain/association to generate context clauses for the
       body. -->
  <xsl:template name="as:association-body-context">

    <xsl:text>with ColdFrame.Instances;&#10;</xsl:text>

    <!-- XXX doesn't suppress duplicates for reflexive associations -->

    <xsl:variable name="role-a" select="role[1]"/>
    <xsl:variable name="role-b" select="role[2]"/>
    <xsl:variable name="a" select="$role-a/classname"/>
    <xsl:variable name="b" select="$role-b/classname"/>

    <xsl:if test="not(associative)">

      <xsl:if test="not(/domain/class[name=$a]/@singleton)">

        <xsl:if test="$role-b/@source and not($role-a/@multiple)">
          <xsl:text>with </xsl:text>
          <xsl:value-of select="/domain/name"/>.<xsl:value-of select="$a"/>
          <xsl:text>.Collections;&#10;</xsl:text>
        </xsl:if>

        <xsl:if test="$role-b/@source">
          <xsl:text>with </xsl:text>
          <xsl:value-of select="/domain/name"/>.<xsl:value-of select="$a"/>
          <xsl:text>.Selection_Function;&#10;</xsl:text>
        </xsl:if>

      </xsl:if>

      <xsl:if test="not(/domain/class[name=$b]/@singleton)">

        <xsl:if test="$role-a/@source and not($role-b/@multiple)">
          <xsl:text>with </xsl:text>
          <xsl:value-of select="/domain/name"/>.<xsl:value-of select="$b"/>
          <xsl:text>.Collections;&#10;</xsl:text>
        </xsl:if>

        <xsl:if test="$role-a/@source">
          <xsl:text>with </xsl:text>
          <xsl:value-of select="/domain/name"/>.<xsl:value-of select="$b"/>
          <xsl:text>.Selection_Function;&#10;</xsl:text>
        </xsl:if>

      </xsl:if>

    </xsl:if>

    <xsl:if test="associative">

      <!-- XXX The generated code isn't optimised for associations
           with singletons; not very important, I think. -->

      <xsl:if test="role/@multiple">
        <xsl:text>with </xsl:text>
        <xsl:value-of select="/domain/name"/>
        <xsl:text>.</xsl:text>
        <xsl:value-of select="associative"/>
        <xsl:text>.Abstract_Containers;&#10;</xsl:text>
      </xsl:if>

      <xsl:text>with </xsl:text>
      <xsl:value-of select="/domain/name"/>
      <xsl:text>.</xsl:text>
      <xsl:value-of select="associative"/>
      <xsl:text>.Collections;&#10;</xsl:text>
      <xsl:text>with </xsl:text>
      <xsl:value-of select="/domain/name"/>
      <xsl:text>.</xsl:text>
      <xsl:value-of select="associative"/>
      <xsl:text>.Selection_Function;&#10;</xsl:text>
    </xsl:if>

  </xsl:template>


  <!-- Called at domain/association to generate the linking subprogram body -->
  <xsl:template name="as:link-body">

    <xsl:variable name="n" select="name"/>

    <xsl:choose>

      <xsl:when test="associative">

        <!--
             function Link
               ({role-a} : {a}.Handle;
                {role-b} : {b}.Handle) return {role-c}.Handle is
                Result : {c}.Handle;
                use ColdFrame.Instances;
                pragma Assert (Handle ({role-a}) /= null);
                pragma Assert (Handle ({role-b}) /= null);
             begin
                Result := {c}.Create
                  ((
             -->

        <xsl:call-template name="as:link-function-specification"/>
        <xsl:text> is&#10;</xsl:text>

        <xsl:value-of select="$II"/>
        <xsl:text>Result : </xsl:text>
        <xsl:value-of select="associative"/>
        <xsl:text>.Handle;&#10;</xsl:text>
        <xsl:value-of select="$II"/>
        <xsl:text>use ColdFrame.Instances;&#10;</xsl:text>

        <xsl:value-of select="$II"/>
        <xsl:text>pragma Assert (Handle (</xsl:text>
        <xsl:value-of select="role[1]/name"/>
        <xsl:text>) /= null);&#10;</xsl:text>
        <xsl:value-of select="$II"/>
        <xsl:text>pragma Assert (Handle (</xsl:text>
        <xsl:value-of select="role[2]/name"/>
        <xsl:text>) /= null);&#10;</xsl:text>

        <xsl:value-of select="$I"/>
        <xsl:text>begin&#10;</xsl:text>

        <xsl:value-of select="$II"/>
        <xsl:text>Result := </xsl:text>
        <xsl:value-of select="associative"/>
        <xsl:text>.Create&#10;</xsl:text>
        <xsl:value-of select="$IIC"/>
        <xsl:text>((</xsl:text>

        <xsl:choose>

          <xsl:when test="role[1]/@multiple and role[2]/@multiple">
            <!-- Both ends multiple; the associative class' identifier
                 references both ends. -->

            <!--
                 {a-role-attr} => Handle ({a-role}),
                 {b-role-attr} => Handle ({b-role})));
                 -->

            <xsl:variable name="r1" select="role[1]"/>
            <xsl:variable name="r2" select="role[2]"/>

            <xsl:call-template name="at:attribute-name">
              <xsl:with-param
                name="a"
                select="/domain/class/attribute
                        [@relation=$n
                         and @refers=$r1/classname
                         and @role=$r1/name]"/>
            </xsl:call-template>
            <xsl:text> => Handle (</xsl:text>
            <xsl:value-of select="$r1/name"/>
            <xsl:text>),&#10;</xsl:text>

            <xsl:value-of select="$IIC"/>
            <xsl:text>  </xsl:text>
            <xsl:call-template name="at:attribute-name">
              <xsl:with-param
                name="a"
                select="/domain/class/attribute
                        [@relation=$n
                         and @refers=$r2/classname
                         and @role=$r2/name]"/>
            </xsl:call-template>
            <xsl:text> => Handle (</xsl:text>
            <xsl:value-of select="$r2/name"/>

            <xsl:text>)));&#10;</xsl:text>

          </xsl:when>

          <xsl:when test="role[@multiple]">
            <!-- One end multiple; the associative class' identifier
                 references the multiple end, the other end is
                 referenced by a plain attribute. -->

            <!--
                     {multiple-role-attr} => Handle ({multiple-role})));
                 {c}.Set_{rel}_{single-role}
                   (Result, Handle ({single-role}));
                 -->

            <xsl:variable name="multiple-role" select="role[@multiple]"/>
            <xsl:variable name="single-role" select="role[not(@multiple)]"/>

            <xsl:call-template name="at:attribute-name">
              <xsl:with-param
                name="a"
                select="/domain/class/attribute
                        [@relation=$n
                        and @refers=$multiple-role/classname
                        and @identifier]"/>
              <!-- NB, had to be careful here because of possible reflexive
                   associations. -->
            </xsl:call-template>
            <xsl:text> => Handle (</xsl:text>
            <xsl:value-of select="$multiple-role/name"/>
            <xsl:text>)));&#10;</xsl:text>

            <xsl:value-of select="$II"/>
            <xsl:text></xsl:text>
            <xsl:value-of select="associative"/>
            <xsl:text>.Set_</xsl:text>
            <xsl:call-template name="at:attribute-name">
              <xsl:with-param
                name="a"
                select="/domain/class/attribute
                        [@relation=$n and @refers=$single-role/classname]"/>
            </xsl:call-template>
            <xsl:text>&#10;</xsl:text>
            <xsl:value-of select="$IIC"/>
            <xsl:text>(Result, Handle (</xsl:text>
            <xsl:value-of select="$single-role/name"/>
            <xsl:text>));&#10;</xsl:text>

          </xsl:when>

          <xsl:otherwise>
            <!-- Neither end multiple -->

            <!--
                     {source-role-attr} => Handle ({source-role})));
                 {c}.Set_{rel}_{non-source-role}
                   (Result, Handle ({non-source-role}));
                 -->

            <xsl:variable name="source-role" select="role[@source]"/>
            <xsl:variable name="non-source-role" select="role[not(@source)]"/>

            <xsl:call-template name="at:attribute-name">
              <xsl:with-param
                name="a"
                select="/domain/class/attribute
                        [@relation=$n
                        and @identifier]"/>
            </xsl:call-template>
            <xsl:text> => Handle (</xsl:text>
            <xsl:value-of select="$source-role/name"/>
            <xsl:text>)));&#10;</xsl:text>

            <xsl:value-of select="$II"/>
            <xsl:value-of select="associative"/>
            <xsl:text>.Set_</xsl:text>
            <xsl:call-template name="at:attribute-name">
              <xsl:with-param
                name="a"
                select="/domain/class/attribute
                        [@relation=$n and @refers=$non-source-role/classname]"/>
            </xsl:call-template>
            <xsl:text>&#10;</xsl:text>
            <xsl:value-of select="$IIC"/>
            <xsl:text>(Result, Handle (</xsl:text>
            <xsl:value-of select="$non-source-role/name"/>
            <xsl:text>));&#10;</xsl:text>

          </xsl:otherwise>

        </xsl:choose>

        <!--
                return Result;
             end Link;
             -->

        <xsl:value-of select="$II"/>
        <xsl:text>return Result;&#10;</xsl:text>
        <xsl:value-of select="$I"/>
        <xsl:text>end Link;&#10;</xsl:text>
        <xsl:value-of select="$blank-line"/>

      </xsl:when>

      <xsl:when test="not(associative)">

        <!--
             procedure Link
               ({role-a} : {a}.Handle;
                {role-b} : {b}.Handle) is
                use ColdFrame.Instances;
                pragma Assert (Handle ({role-a}) /= null);
                pragma Assert (Handle ({role-b}) /= null);
             begin
             -->

        <xsl:call-template name="as:link-procedure-specification"/>
        <xsl:text> is&#10;</xsl:text>
        <xsl:value-of select="$II"/>
        <xsl:text>use ColdFrame.Instances;&#10;</xsl:text>

        <xsl:value-of select="$II"/>
        <xsl:text>pragma Assert (Handle (</xsl:text>
        <xsl:value-of select="role[1]/name"/>
        <xsl:text>) /= null);&#10;</xsl:text>
        <xsl:value-of select="$II"/>
        <xsl:text>pragma Assert (Handle (</xsl:text>
        <xsl:value-of select="role[2]/name"/>
        <xsl:text>) /= null);&#10;</xsl:text>

        <xsl:choose>

          <!-- If the referential attribute is already part of the identifier,
               we can't change it (there isn't a Set operation). But the
               act of creating the instance in which the formalization is
               required has already performed the Link. -->
          <xsl:when test="/domain/class/attribute[@relation=$n]/@identifier">

            <!--
                 begin
                    null;
                 -->

            <xsl:value-of select="$I"/>
            <xsl:text>begin&#10;</xsl:text>

            <xsl:value-of select="$II"/>
            <xsl:text>null;&#10;</xsl:text>
          </xsl:when>

          <xsl:otherwise>

            <!--
                     pragma Assert
                      ({dst}.Get_{ref-attr} ({dst}) = null,
                       "already linked");
                begin
                    {dst}.Set_{ref-attr}}
                      ({dst}, {src});
                 -->

            <xsl:variable name="src" select="role[@source]"/>
            <xsl:variable name="dst" select="role[not(@source)]"/>

            <xsl:value-of select="$II"/>
            <xsl:text>pragma Assert&#10;</xsl:text>
            <xsl:value-of select="$IIC"/>
            <xsl:text>(</xsl:text>
            <xsl:value-of select="$dst/classname"/>
            <xsl:text>.Get_</xsl:text>
            <xsl:call-template name="at:attribute-name">
              <xsl:with-param
                name="a"
                select="/domain/class/attribute
                        [@relation=$n and @refers=$src/classname]"/>
            </xsl:call-template>
            <xsl:text> (</xsl:text>
            <xsl:value-of select="$dst/name"/>
            <xsl:text>) = null,&#10;</xsl:text>
            <xsl:value-of select="$IIC"/>
            <xsl:text> "already linked");&#10;</xsl:text>

            <xsl:value-of select="$I"/>
            <xsl:text>begin&#10;</xsl:text>

            <xsl:value-of select="$II"/>
            <xsl:value-of select="$dst/classname"/>
            <xsl:text>.Set_</xsl:text>
            <xsl:call-template name="at:attribute-name">
              <xsl:with-param
                name="a"
                select="/domain/class/attribute
                        [@relation=$n and @refers=$src/classname]"/>
            </xsl:call-template>
            <xsl:text>&#10;</xsl:text>
            <xsl:value-of select="$IIC"/>
            <xsl:text>(</xsl:text>
            <xsl:value-of select="$dst/name"/>
            <xsl:text>, Handle (</xsl:text>
            <xsl:value-of select="$src/name"/>
            <xsl:text>));&#10;</xsl:text>
          </xsl:otherwise>

        </xsl:choose>

        <!--
             end Link;
             -->

        <xsl:value-of select="$I"/>
        <xsl:text>end Link;&#10;</xsl:text>
        <xsl:value-of select="$blank-line"/>

      </xsl:when>

    </xsl:choose>

  </xsl:template>


  <!-- Called at domain/association to generate the find function body,
       if any. -->
  <xsl:template name="as:find-body">
    <xsl:if test="associative">

      <xsl:variable name="n" select="name"/>

      <!--
           function Find
             ({role-a} : {a}.Handle;
              {role-b} : {b}.Handle) return {role-c}.Handle is
              use ColdFrame.Instances;
           begin
              return {c}.Find
                ((
           -->

      <xsl:call-template name="as:association-find-specification"/>
      <xsl:text> is&#10;</xsl:text>

      <xsl:value-of select="$II"/>
      <xsl:text>use ColdFrame.Instances;&#10;</xsl:text>

      <xsl:value-of select="$I"/>
      <xsl:text>begin&#10;</xsl:text>

      <xsl:value-of select="$II"/>
      <xsl:text>return </xsl:text>
      <xsl:value-of select="associative"/>
      <xsl:text>.Find&#10;</xsl:text>
      <xsl:value-of select="$IIC"/>
      <xsl:text>((</xsl:text>

      <xsl:choose>

        <xsl:when test="role[1]/@multiple and role[2]/@multiple">
          <!-- Both ends multiple; the associative class' identifier
               references both ends. -->

          <!--
               {a-role-attr} => Handle ({a-role}),
               {b-role-attr} => Handle ({b-role})));
               -->

          <xsl:variable name="r1" select="role[1]"/>
          <xsl:variable name="r2" select="role[2]"/>

          <xsl:call-template name="at:attribute-name">
            <xsl:with-param
              name="a"
              select="/domain/class/attribute
                      [@relation=$n
                       and @refers=$r1/classname
                       and @role=$r1/name]"/>
          </xsl:call-template>
          <xsl:text> => Handle (</xsl:text>
          <xsl:value-of select="$r1/name"/>
          <xsl:text>),&#10;</xsl:text>

          <xsl:value-of select="$IIC"/>
          <xsl:text>  </xsl:text>
          <xsl:call-template name="at:attribute-name">
            <xsl:with-param
              name="a"
              select="/domain/class/attribute
                      [@relation=$n
                       and @refers=$r2/classname
                       and @role=$r2/name]"/>
          </xsl:call-template>
          <xsl:text> => Handle (</xsl:text>
          <xsl:value-of select="$r2/name"/>

          <xsl:text>)));&#10;</xsl:text>

        </xsl:when>

        <xsl:when test="role[@multiple]">
          <!-- One end multiple; the associative class' identifier
               references the multiple end, the other end is
               referenced by a plain attribute. -->

          <!--
               {multiple-role-attr} => Handle ({multiple-role})));
               -->

          <xsl:variable name="multiple-role" select="role[@multiple]"/>
          <xsl:variable name="single-role" select="role[not(@multiple)]"/>

          <xsl:call-template name="at:attribute-name">
            <xsl:with-param
              name="a"
              select="/domain/class/attribute
                      [@relation=$n
                      and @refers=$multiple-role/classname
                      and @identifier]"/>
            <!-- NB, had to be careful here because of possible reflexive
                 associations. -->
          </xsl:call-template>
          <xsl:text> => Handle (</xsl:text>
          <xsl:value-of select="$multiple-role/name"/>
          <xsl:text>)));&#10;</xsl:text>

        </xsl:when>

        <xsl:otherwise>
          <!-- Neither end multiple -->

          <!--
               {source-role-attr} => Handle ({source-role})));
               -->

          <xsl:variable name="source-role" select="role[@source]"/>
          <xsl:variable name="non-source-role" select="role[not(@source)]"/>

          <xsl:call-template name="at:attribute-name">
            <xsl:with-param
              name="a"
              select="/domain/class/attribute
                      [@relation=$n
                      and @identifier]"/>
          </xsl:call-template>
          <xsl:text> => Handle (</xsl:text>
          <xsl:value-of select="$source-role/name"/>
          <xsl:text>)));&#10;</xsl:text>

        </xsl:otherwise>

      </xsl:choose>

      <!--
           end Find;
           -->

      <xsl:value-of select="$I"/>
      <xsl:text>end Find;&#10;</xsl:text>
      <xsl:value-of select="$blank-line"/>

    </xsl:if>
  </xsl:template>


  <!-- Called at domain/association to generate the unlinking procedure
       body -->
  <xsl:template name="as:unlink-body">

    <xsl:variable name="n" select="name"/>

    <xsl:choose>

      <xsl:when test="associative">

        <!-- There's no point in trying to null out the referential
             attributes, since (presumably) the user's about to delete
             the associative class instance anyway. -->
        <xsl:call-template name="as:unlink-associative-specification"/>
        <xsl:text> is&#10;</xsl:text>
        <xsl:value-of select="$II"/>
        <xsl:text>use ColdFrame.Instances;&#10;</xsl:text>

        <xsl:value-of select="$II"/>
        <xsl:text>pragma Assert (Handle (</xsl:text>
        <xsl:value-of select="associative"/>
        <xsl:text>_Handle) /= null);&#10;</xsl:text>

        <xsl:value-of select="$I"/>
        <xsl:text>begin&#10;</xsl:text>
        <xsl:value-of select="$II"/>
        <xsl:text>null;&#10;</xsl:text>
        <xsl:value-of select="$I"/>
        <xsl:text>end Unlink;&#10;</xsl:text>
        <xsl:value-of select="$blank-line"/>

      </xsl:when>

      <xsl:when test="not(associative)">
        <!--
             procedure Unlink
               ({role-a} : {a}.Handle;
                {role-b} : {b}.Handle) is
                use ColdFrame.Instances;
                pragma Assert (Handle ({role-a}) /= null);
                pragma Assert (Handle ({role-b}) /= null);
             -->

        <xsl:call-template name="as:unlink-specification"/>
        <xsl:text> is&#10;</xsl:text>
        <xsl:value-of select="$II"/>
        <xsl:text>use ColdFrame.Instances;&#10;</xsl:text>

        <xsl:value-of select="$II"/>
        <xsl:text>pragma Assert (Handle (</xsl:text>
        <xsl:value-of select="role[1]/name"/>
        <xsl:text>) /= null);&#10;</xsl:text>
        <xsl:value-of select="$II"/>
        <xsl:text>pragma Assert (Handle (</xsl:text>
        <xsl:value-of select="role[2]/name"/>
        <xsl:text>) /= null);&#10;</xsl:text>

        <xsl:choose>

          <xsl:when test="/domain/class/attribute[@relation=$n]/@identifier">
            <!-- If the referential attribute is part of the identifier,
                 we can't change it (there isn't a Set operation). So
                 we rather hope the user's about to delete the instance. -->
            <!--
                 begin
                    null;
                 -->

            <xsl:value-of select="$I"/>
            <xsl:text>begin&#10;</xsl:text>

            <xsl:value-of select="$II"/>
            <xsl:text>null;&#10;</xsl:text>
          </xsl:when>

          <xsl:otherwise>
            <!-- Null out the referential attribute. -->
            <!--
                    pragma Assert
                      ({dst}.Get_{ref-attr} ({src}) /= null,
                       "already unlinked");
                 begin
                    {dst}.Set_{ref-attr}
                       ({dst}, null);
                 -->

            <xsl:variable name="src" select="role[@source]"/>
            <xsl:variable name="dst" select="role[not(@source)]"/>

            <xsl:value-of select="$II"/>
            <xsl:text>pragma Assert&#10;</xsl:text>
            <xsl:value-of select="$IIC"/>
            <xsl:text>(</xsl:text>
            <xsl:value-of select="$dst/classname"/>
            <xsl:text>.Get_</xsl:text>
            <xsl:call-template name="at:attribute-name">
              <xsl:with-param
                name="a"
                select="/domain/class/attribute
                        [@relation=$n and @refers=$src/classname]"/>
            </xsl:call-template>
            <xsl:text> (</xsl:text>
            <xsl:value-of select="$dst/name"/>
            <xsl:text>) /= null,&#10;</xsl:text>
            <xsl:value-of select="$IIC"/>
            <xsl:text> "already unlinked");&#10;</xsl:text>

            <xsl:value-of select="$I"/>
            <xsl:text>begin&#10;</xsl:text>

            <xsl:value-of select="$II"/>
            <xsl:value-of select="$dst/classname"/>
            <xsl:text>.Set_</xsl:text>
            <xsl:call-template name="at:attribute-name">
              <xsl:with-param
                name="a"
                select="/domain/class/attribute
                        [@relation=$n and @refers=$src/classname]"/>
            </xsl:call-template>
            <xsl:text>&#10;</xsl:text>
            <xsl:value-of select="$IIC"/>
            <xsl:text>(</xsl:text>
            <xsl:value-of select="$dst/name"/>
            <xsl:text>, null);&#10;</xsl:text>
          </xsl:otherwise>

        </xsl:choose>

        <xsl:value-of select="$I"/>
        <xsl:text>end Unlink;&#10;</xsl:text>
        <xsl:value-of select="$blank-line"/>

      </xsl:when>

    </xsl:choose>

  </xsl:template>


  <!-- Called at domain/association to generate the navigation function
       specs. -->
  <xsl:template name="as:navigation-specs">

    <xsl:variable name="role-1" select="role[1]"/>
    <xsl:variable
      name="singleton-1"
      select="/domain/class[name=$role-1/classname]/@singleton"/>
    <xsl:variable name="role-2" select="role[2]"/>
    <xsl:variable
      name="singleton-2"
      select="/domain/class[name=$role-2/classname]/@singleton"/>

    <!-- First direction: from one -->

    <xsl:call-template name="as:navigation-specification">
      <xsl:with-param name="role-a" select="role[1]"/>
      <xsl:with-param name="role-b" select="role[2]"/>
    </xsl:call-template>
    <xsl:text>;&#10;</xsl:text>
    <xsl:value-of select="$blank-line"/>

    <xsl:if test="associative">

      <xsl:call-template name="as:navigation-to-associative-specification">
        <xsl:with-param name="role-a" select="role[1]"/>
        <xsl:with-param name="role-b" select="role[2]"/>
        <xsl:with-param name="assoc" select="associative"/>
      </xsl:call-template>
      <xsl:text>;&#10;</xsl:text>
      <xsl:value-of select="$blank-line"/>

      <xsl:call-template name="as:navigation-from-associative-specification">
        <xsl:with-param name="role-a" select="role[1]"/>
        <xsl:with-param name="role-b" select="role[2]"/>
        <xsl:with-param name="assoc" select="associative"/>
      </xsl:call-template>
      <xsl:text>;&#10;</xsl:text>
      <xsl:value-of select="$blank-line"/>

    </xsl:if>

    <!-- Second direction: from one -->

    <xsl:call-template name="as:navigation-specification">
      <xsl:with-param name="role-a" select="role[2]"/>
      <xsl:with-param name="role-b" select="role[1]"/>
    </xsl:call-template>
    <xsl:text>;&#10;</xsl:text>
    <xsl:value-of select="$blank-line"/>

    <xsl:if test="associative">

      <xsl:call-template name="as:navigation-to-associative-specification">
        <xsl:with-param name="role-a" select="role[2]"/>
        <xsl:with-param name="role-b" select="role[1]"/>
        <xsl:with-param name="assoc" select="associative"/>
      </xsl:call-template>
      <xsl:text>;&#10;</xsl:text>
      <xsl:value-of select="$blank-line"/>

      <xsl:call-template name="as:navigation-from-associative-specification">
        <xsl:with-param name="role-a" select="role[2]"/>
        <xsl:with-param name="role-b" select="role[1]"/>
        <xsl:with-param name="assoc" select="associative"/>
      </xsl:call-template>
      <xsl:text>;&#10;</xsl:text>
      <xsl:value-of select="$blank-line"/>

    </xsl:if>

  </xsl:template>


  <!-- Called at domain/association to generate the navigation function
       bodies. -->
  <xsl:template name="as:navigation-bodies">

    <xsl:variable name="role-1" select="role[1]"/>
    <xsl:variable
      name="singleton-1"
      select="/domain/class[name=$role-1/classname]/@singleton"/>
    <xsl:variable name="role-2" select="role[2]"/>
    <xsl:variable
      name="singleton-2"
      select="/domain/class[name=$role-2/classname]/@singleton"/>

    <!-- First direction: from one -->

    <xsl:call-template name="as:navigation-body">
      <xsl:with-param name="role-a" select="role[1]"/>
      <xsl:with-param name="role-b" select="role[2]"/>
    </xsl:call-template>

    <xsl:if test="associative">

      <xsl:call-template name="as:navigation-to-associative-body">
        <xsl:with-param name="role-a" select="role[1]"/>
        <xsl:with-param name="role-b" select="role[2]"/>
        <xsl:with-param name="assoc" select="associative"/>
      </xsl:call-template>

      <xsl:call-template name="as:navigation-from-associative-body">
        <xsl:with-param name="role-a" select="role[1]"/>
        <xsl:with-param name="role-b" select="role[2]"/>
        <xsl:with-param name="assoc" select="associative"/>
      </xsl:call-template>

    </xsl:if>

    <!-- Second direction: from one -->

    <xsl:call-template name="as:navigation-body">
      <xsl:with-param name="role-a" select="role[2]"/>
      <xsl:with-param name="role-b" select="role[1]"/>
    </xsl:call-template>

    <xsl:if test="associative">

      <xsl:call-template name="as:navigation-to-associative-body">
        <xsl:with-param name="role-a" select="role[2]"/>
        <xsl:with-param name="role-b" select="role[1]"/>
        <xsl:with-param name="assoc" select="associative"/>
      </xsl:call-template>

      <xsl:call-template name="as:navigation-from-associative-body">
        <xsl:with-param name="role-a" select="role[2]"/>
        <xsl:with-param name="role-b" select="role[1]"/>
        <xsl:with-param name="assoc" select="associative"/>
      </xsl:call-template>

    </xsl:if>

  </xsl:template>


  <!-- Called at domain/association to generate a navigation function
       body. -->
  <xsl:template name="as:navigation-body">
    <xsl:param name="role-a"/>   <!-- from this .. -->
    <xsl:param name="role-b"/>   <!-- .. to this -->

    <xsl:choose>
      <xsl:when test="associative">
        <xsl:call-template name="as:navigation-with-associative-body">
          <xsl:with-param name="role-a" select="$role-a"/>
          <xsl:with-param name="role-b" select="$role-b"/>
        </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
        <xsl:call-template name="as:navigation-without-associative-body">
          <xsl:with-param name="role-a" select="$role-a"/>
          <xsl:with-param name="role-b" select="$role-b"/>
        </xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>

  </xsl:template>


  <!-- Called at domain/association to generate a navigation function
       body for an associative association. -->
  <xsl:template name="as:navigation-with-associative-body">
    <xsl:param name="role-a"/>   <!-- from this .. -->
    <xsl:param name="role-b"/>   <!-- .. to this -->

    <xsl:variable name="n" select="name"/>
    <xsl:variable name="a">
      <xsl:value-of select="$role-a/classname"/>
    </xsl:variable>
    <xsl:variable name="b">
      <xsl:value-of select="$role-b/classname"/>
    </xsl:variable>
    <xsl:variable name="associative" select="associative"/>

    <xsl:call-template name="as:navigation-specification">
      <xsl:with-param name="role-a" select="$role-a"/>
      <xsl:with-param name="role-b" select="$role-b"/>
    </xsl:call-template>
    <xsl:text> is&#10;</xsl:text>

    <xsl:variable name="assoc">
      <xsl:value-of select="$associative"/>
    </xsl:variable>

    <xsl:choose>

      <xsl:when test="$role-b/@multiple">

        <!--
             {assoc-abbrev} : constant {assoc}.Collections.Collection
               := {role-a} ({a-abbrev});
             It : {assoc}.Abstract_Containers.Iterator'Class
               := {assoc}.Collections.New_Iterator ({assoc-abbrev});
             Result : {b}.Collections.Collection;
             -->

        <xsl:value-of select="$II"/>
        <xsl:value-of
          select="/domain/class[name=$associative]/abbreviation"/>
        <xsl:text> : constant </xsl:text>
        <xsl:value-of select="$assoc"/>
        <xsl:text>.Collections.Collection&#10;</xsl:text>
        <xsl:value-of select="$IIC"/>
        <xsl:text>:= </xsl:text>
        <xsl:value-of select="$role-a/name"/>
        <xsl:text> (</xsl:text>
        <xsl:value-of
          select="/domain/class[name=$role-a/classname]/abbreviation"/>
        <xsl:text>);&#10;</xsl:text>
        <xsl:value-of select="$II"/>
        <xsl:text>It : </xsl:text>
        <xsl:value-of select="$assoc"/>
        <xsl:text>.Abstract_Containers.Iterator'Class&#10;</xsl:text>
        <xsl:value-of select="$IIC"/>
        <xsl:text> := </xsl:text>
        <xsl:value-of select="$assoc"/>
        <xsl:text>.Collections.New_Iterator (</xsl:text>
        <xsl:value-of
          select="/domain/class[name=$associative]/abbreviation"/>
        <xsl:text>);&#10;</xsl:text>
        <xsl:value-of select="$II"/>
        <xsl:text>Result : </xsl:text>
        <xsl:value-of select="$b"/>
        <xsl:text>.Collections.Collection;&#10;</xsl:text>

      </xsl:when>

      <xsl:otherwise>

        <!--
             {assoc-abbrev} : constant {assoc}.Handle
               := {role-a} ({a-abbrev});
             use type {assoc}.Handle;
             -->

        <xsl:value-of select="$II"/>
        <xsl:value-of
          select="/domain/class[name=$associative]/abbreviation"/>
        <xsl:text> : constant </xsl:text>
        <xsl:value-of select="$assoc"/>
        <xsl:text>.Handle&#10;</xsl:text>
        <xsl:value-of select="$IIC"/>
        <xsl:text>:= </xsl:text>
        <xsl:value-of select="$role-a/name"/>
        <xsl:text> (</xsl:text>
        <xsl:value-of
          select="/domain/class[name=$role-a/classname]/abbreviation"/>
        <xsl:text>);&#10;</xsl:text>
        <xsl:value-of select="$II"/>
        <xsl:text>use type </xsl:text>
        <xsl:value-of select="$assoc"/>
        <xsl:text>.Handle;&#10;</xsl:text>

      </xsl:otherwise>

    </xsl:choose>

    <xsl:value-of select="$I"/>
    <xsl:text>begin&#10;</xsl:text>

    <xsl:choose>

      <xsl:when test="$role-b/@multiple">

        <!--
             while not {associative}.Abstract_Containers.Is_Done (It) loop
                {b}.Collections.Append
                  (Result,
                   {role-a}
                     ({associative}.Abstract_Containers.Current_Item (It)));
                {associative}.Abstract_Containers.Next (It);
             end loop;
             return Result;
             -->

        <xsl:value-of select="$II"/>
        <xsl:text>while not </xsl:text>
        <xsl:value-of select="$assoc"/>
        <xsl:text>.Abstract_Containers.Is_Done (It) loop&#10;</xsl:text>

        <xsl:value-of select="$III"/>
        <xsl:value-of select="$b"/>
        <xsl:text>.Collections.Append&#10;</xsl:text>
        <xsl:value-of select="$IIIC"/>
        <xsl:text>(Result,&#10;</xsl:text>
        <xsl:value-of select="$IIIC"/>
        <xsl:text> </xsl:text>
        <xsl:value-of select="$role-a/name"/>
        <xsl:text>&#10;</xsl:text>
        <xsl:value-of select="$IIIIC"/>
        <xsl:text>(</xsl:text>
        <xsl:value-of select="$assoc"/>
        <xsl:text>.Abstract_Containers.Current_Item (It)));&#10;</xsl:text>
        <xsl:value-of select="$III"/>
        <xsl:value-of select="$assoc"/>
        <xsl:text>.Abstract_Containers.Next (It);&#10;</xsl:text>

        <xsl:value-of select="$II"/>
        <xsl:text>end loop;&#10;</xsl:text>
        <xsl:value-of select="$II"/>
        <xsl:text>return Result;&#10;</xsl:text>
      </xsl:when>

      <xsl:otherwise>

        <!--
             if {assoc-abbrev} = null then
               return null;
             else
               return {role-a} ({assoc-abbrev});
             end if;
             -->

        <xsl:value-of select="$II"/>
        <xsl:text>if </xsl:text>
        <xsl:value-of
          select="/domain/class[name=$associative]/abbreviation"/>
        <xsl:text> = null then&#10;</xsl:text>
        <xsl:value-of select="$III"/>
        <xsl:text>return null;&#10;</xsl:text>
        <xsl:value-of select="$II"/>
        <xsl:text>else&#10;</xsl:text>
        <xsl:value-of select="$III"/>
        <xsl:text>return </xsl:text>
        <xsl:value-of  select="$role-a/name"/>
        <xsl:text> (</xsl:text>
        <xsl:value-of
          select="/domain/class[name=$associative]/abbreviation"/>
        <xsl:text>);&#10;</xsl:text>
        <xsl:value-of select="$II"/>
        <xsl:text>end if;&#10;</xsl:text>
      </xsl:otherwise>

    </xsl:choose>

    <xsl:value-of select="$I"/>
    <xsl:text>end </xsl:text>
    <xsl:value-of select="$role-a/name"/>
    <xsl:text>;&#10;</xsl:text>
    <xsl:value-of select="$blank-line"/>

  </xsl:template>


  <!-- Called at domain/association to generate a navigation function
       body for a non-associative association. -->
  <xsl:template name="as:navigation-without-associative-body">
    <xsl:param name="role-a"/>   <!-- from this .. -->
    <xsl:param name="role-b"/>   <!-- .. to this -->

    <xsl:variable name="n" select="name"/>
    <xsl:variable name="a">
      <xsl:value-of select="$role-a/classname"/>
    </xsl:variable>
    <xsl:variable name="b">
      <xsl:value-of select="$role-b/classname"/>
    </xsl:variable>

    <xsl:call-template name="as:navigation-specification">
      <xsl:with-param name="role-a" select="$role-a"/>
      <xsl:with-param name="role-b" select="$role-b"/>
    </xsl:call-template>
    <xsl:text> is&#10;</xsl:text>

    <xsl:if test="$role-b/@multiple or $role-a/@source">

      <!--
           function Sel (This : {b}.Handle return Boolean;
           function Sel (This : {b}.Handle return Boolean is
              H : constant ColdFrame.Instances.Handle
                := {b}.Get_{ref-attr} (This);
              use type {a}.Handle;
           begin
              return {a}.Handle (H) = {a-abbrev};
           end Sel;
           function Find
           is new {b}.Selection_Function (Sel);
           Result : constant {b}.Collections.Collection := Find;
           -->

      <xsl:value-of select="$II"/>
      <xsl:text>function Sel (This : </xsl:text>
      <xsl:value-of select="$b"/>
      <xsl:text>.Handle) return Boolean;&#10;</xsl:text>
      <xsl:value-of select="$II"/>
      <xsl:text>function Sel (This : </xsl:text>
      <xsl:value-of select="$b"/>
      <xsl:text>.Handle) return Boolean is&#10;</xsl:text>
      <xsl:value-of select="$III"/>
      <xsl:text>H : constant ColdFrame.Instances.Handle&#10;</xsl:text>
      <xsl:value-of select="$IIIC"/>
      <xsl:text>:= </xsl:text>
      <xsl:value-of select="$b"/>
      <xsl:text>.Get_</xsl:text>
      <xsl:call-template name="at:attribute-name">
        <xsl:with-param
          name="a"
          select="/domain/class/attribute
                  [@relation=$n and @refers=$role-a/classname]"/>
      </xsl:call-template>
      <xsl:text> (This);&#10;</xsl:text>
      <xsl:value-of select="$III"/>
      <xsl:text>use type </xsl:text>
      <xsl:value-of select="$a"/>
      <xsl:text>.Handle;&#10;</xsl:text>
      <xsl:value-of select="$II"/>
      <xsl:text>begin&#10;</xsl:text>
      <xsl:value-of select="$III"/>
      <xsl:text>return </xsl:text>
      <xsl:value-of select="$a"/>
      <xsl:text>.Handle (H) = </xsl:text>
      <xsl:value-of
        select="/domain/class[name=$role-a/classname]/abbreviation"/>
      <xsl:text>;&#10;</xsl:text>
      <xsl:value-of select="$II"/>
      <xsl:text>end Sel;&#10;</xsl:text>
      <xsl:value-of select="$II"/>
      <xsl:text>function Find&#10;</xsl:text>
      <xsl:value-of select="$II"/>
      <xsl:text>is new </xsl:text>
      <xsl:value-of select="$b"/>
      <xsl:text>.Selection_Function (Sel);&#10;</xsl:text>
      <xsl:value-of select="$II"/>
      <xsl:text>Result : constant </xsl:text>
      <xsl:value-of select="$b"/>
      <xsl:text>.Collections.Collection := Find;&#10;</xsl:text>

    </xsl:if>

    <xsl:value-of select="$I"/>
    <xsl:text>begin&#10;</xsl:text>

    <xsl:choose>

      <xsl:when test="$role-b/@multiple">

        <!--
             return Result;
             -->

        <xsl:value-of select="$II"/>
        <xsl:text>return Result;&#10;</xsl:text>

      </xsl:when>

      <xsl:when test="$role-a/@source">

        <!--
             if {b}.Collections.Is_Empty (Result) then
                return null;
             else
                return {b}.Collections.First (Result);
             end if;
             -->

        <xsl:value-of select="$II"/>
        <xsl:text>if </xsl:text>
        <xsl:value-of select="$b"/>
        <xsl:text>.Collections.Is_Empty (Result) then&#10;</xsl:text>
        <xsl:value-of select="$III"/>
        <xsl:text>return null;&#10;</xsl:text>
        <xsl:value-of select="$II"/>
        <xsl:text>else&#10;</xsl:text>
        <!-- XXX case for checking there is only one! -->
        <xsl:value-of select="$III"/>
        <xsl:text>return </xsl:text>
        <xsl:value-of select="$b"/>
        <xsl:text>.Collections.First (Result);&#10;</xsl:text>
        <xsl:value-of select="$II"/>
        <xsl:text>end if;&#10;</xsl:text>

      </xsl:when>

      <xsl:otherwise>

        <!--
             return {b}.Handle
               ({a}.Get_{ref-attr} ({a-abbrev}));
             -->

        <xsl:value-of select="$II"/>
        <xsl:text>return </xsl:text>
        <xsl:value-of select="$b"/>
        <xsl:text>.Handle&#10;</xsl:text>
        <xsl:value-of select="$IIC"/>
        <xsl:text>(</xsl:text>
        <xsl:value-of select="$a"/>
        <xsl:text>.Get_</xsl:text>
        <xsl:call-template name="at:attribute-name">
          <xsl:with-param
            name="a"
            select="/domain/class/attribute
                    [@relation=$n and @refers=$role-b/classname]"/>
        </xsl:call-template>
        <xsl:text> (</xsl:text>
        <xsl:value-of
          select="/domain/class[name=$role-a/classname]/abbreviation"/>
        <xsl:text>));&#10;</xsl:text>

      </xsl:otherwise>

    </xsl:choose>

    <xsl:value-of select="$I"/>
    <xsl:text>end </xsl:text>
    <xsl:value-of select="$role-a/name"/>
    <xsl:text>;&#10;</xsl:text>
    <xsl:value-of select="$blank-line"/>

  </xsl:template>


  <!-- Called at domain/association to generate an associative navigation
       function body. -->
  <xsl:template name="as:navigation-from-associative-body">
    <xsl:param name="role-a"/>
    <xsl:param name="role-b"/>   <!-- .. to this -->
    <xsl:param name="assoc"/>    <!-- from this .. -->

    <xsl:variable name="n" select="name"/>
    <xsl:variable name="a">
      <xsl:value-of select="$role-a/classname"/>
    </xsl:variable>
    <xsl:variable name="b">
      <xsl:value-of select="$role-b/classname"/>
    </xsl:variable>

    <xsl:call-template name="as:navigation-from-associative-specification">
      <xsl:with-param name="role-a" select="$role-a"/>
      <xsl:with-param name="role-b" select="$role-b"/>
      <xsl:with-param name="assoc" select="$assoc"/>
    </xsl:call-template>
    <xsl:text> is&#10;</xsl:text>

    <!--
         H : constant ColdFrame.Instances.Handle
           := {c}.Get_{b-ref-attr} ({a-abbr});
         -->

    <xsl:value-of select="$II"/>
    <xsl:text>H : constant ColdFrame.Instances.Handle&#10;</xsl:text>
    <xsl:value-of select="$IIC"/>
    <xsl:text>:= </xsl:text>
    <xsl:value-of select="$assoc"/>
    <xsl:text>.Get_</xsl:text>
    <xsl:call-template name="at:attribute-name">
      <xsl:with-param
        name="a"
        select="/domain/class/attribute
                [@relation=$n and @refers=$role-b/classname]"/>
    </xsl:call-template>
    <xsl:text> (</xsl:text>
    <xsl:value-of
      select="/domain/class[name=$assoc]/abbreviation"/>
    <xsl:text>);&#10;</xsl:text>

    <xsl:value-of select="$I"/>
    <xsl:text>begin&#10;</xsl:text>

    <!--
         return {b}.Handle (H);
         -->

    <xsl:value-of select="$II"/>
    <xsl:text>return </xsl:text>
    <xsl:value-of select="$b"/>
    <xsl:text>.Handle (H);&#10;</xsl:text>

    <xsl:value-of select="$I"/>
    <xsl:text>end </xsl:text>
    <xsl:value-of select="$role-a/name"/>
    <xsl:text>;&#10;</xsl:text>
    <xsl:value-of select="$blank-line"/>

  </xsl:template>


  <!-- Called at domain/association to generate an associative navigation
       function body. -->
  <xsl:template name="as:navigation-to-associative-body">
    <xsl:param name="role-a"/>   <!-- from this .. -->
    <xsl:param name="role-b"/>
    <xsl:param name="assoc"/>    <!-- .. to this -->

    <xsl:variable name="n" select="name"/>
    <xsl:variable name="a">
      <xsl:value-of select="$role-a/classname"/>
    </xsl:variable>
    <xsl:variable name="b">
      <xsl:value-of select="$role-b/classname"/>
    </xsl:variable>
    <xsl:variable name="c">
      <xsl:value-of select="$assoc"/>
    </xsl:variable>

    <xsl:call-template name="as:navigation-to-associative-specification">
      <xsl:with-param name="role-a" select="$role-a"/>
      <xsl:with-param name="role-b" select="$role-b"/>
      <xsl:with-param name="assoc" select="$assoc"/>
    </xsl:call-template>
    <xsl:text> is&#10;</xsl:text>

    <!--
         function Sel (This : {c}.Handle) return Boolean;
         function Sel (This : {c}.Handle) return Boolean is
            H : constant ColdFrame.Instances.Handle
              := {c}.Get_{a-ref-attr} (This);
            use type {a}.Handle;
         begin
            return {a}.Handle (H) = {a-abbrev};
         end Sel;
         function Find
         is new {c}.Selection_Function (Sel);
         Result : constant {c}.Collections.Collection := Find;
         -->

    <xsl:value-of select="$II"/>
    <xsl:text>function Sel (This : </xsl:text>
    <xsl:value-of select="$c"/>
    <xsl:text>.Handle) return Boolean;&#10;</xsl:text>
    <xsl:value-of select="$II"/>
    <xsl:text>function Sel (This : </xsl:text>
    <xsl:value-of select="$c"/>
    <xsl:text>.Handle) return Boolean is&#10;</xsl:text>
    <xsl:value-of select="$III"/>
    <xsl:text>H : constant ColdFrame.Instances.Handle&#10;</xsl:text>
    <xsl:value-of select="$IIIC"/>
    <xsl:text>:= </xsl:text>
    <xsl:value-of select="$c"/>
    <xsl:text>.Get_</xsl:text>
    <xsl:call-template name="at:attribute-name">
      <xsl:with-param
        name="a"
        select="/domain/class/attribute
                [@relation=$n and @refers=$role-a/classname]"/>
    </xsl:call-template>
    <xsl:text> (This);&#10;</xsl:text>
    <xsl:value-of select="$III"/>
    <xsl:text>use type </xsl:text>
    <xsl:value-of select="$a"/>
    <xsl:text>.Handle;&#10;</xsl:text>

    <xsl:value-of select="$II"/>
    <xsl:text>begin&#10;</xsl:text>
    <xsl:value-of select="$III"/>
    <xsl:text>return </xsl:text>
    <xsl:value-of select="$a"/>
    <xsl:text>.Handle (H) = </xsl:text>
    <xsl:value-of
      select="/domain/class[name=$role-a/classname]/abbreviation"/>
    <xsl:text>;&#10;</xsl:text>
    <xsl:value-of select="$II"/>
    <xsl:text>end Sel;&#10;</xsl:text>
    <xsl:value-of select="$II"/>
    <xsl:text>function Find&#10;</xsl:text>
    <xsl:value-of select="$II"/>
    <xsl:text>is new </xsl:text>
    <xsl:value-of select="$c"/>
    <xsl:text>.Selection_Function (Sel);&#10;</xsl:text>
    <xsl:value-of select="$II"/>
    <xsl:text>Result : constant </xsl:text>
    <xsl:value-of select="$c"/>
    <xsl:text>.Collections.Collection := Find;&#10;</xsl:text>

    <xsl:value-of select="$I"/>
    <xsl:text>begin&#10;</xsl:text>

    <xsl:choose>

      <xsl:when test="$role-b/@multiple">

        <!--
             return Result;
             -->

        <xsl:value-of select="$II"/>
        <xsl:text>return Result;&#10;</xsl:text>

      </xsl:when>

      <xsl:otherwise>

         <!--
             if {c}.Collections.Is_Empty (Result) then
                return null;
             else
                return {c}.Collections.First (Result);
             end if;
             -->

       <xsl:value-of select="$II"/>
        <xsl:text>if </xsl:text>
        <xsl:value-of select="$c"/>
        <xsl:text>.Collections.Is_Empty (Result) then&#10;</xsl:text>
        <xsl:value-of select="$III"/>
        <xsl:text>return null;&#10;</xsl:text>
        <xsl:value-of select="$II"/>
        <xsl:text>else&#10;</xsl:text>
        <!-- XXX case for checking there is only one! -->
        <xsl:value-of select="$III"/>
        <xsl:text>return </xsl:text>
        <xsl:value-of select="$c"/>
        <xsl:text>.Collections.First (Result);&#10;</xsl:text>
        <xsl:value-of select="$II"/>
        <xsl:text>end if;&#10;</xsl:text>

      </xsl:otherwise>

    </xsl:choose>

    <xsl:value-of select="$I"/>
    <xsl:text>end </xsl:text>
    <xsl:value-of select="$role-a/name"/>
    <xsl:text>;&#10;</xsl:text>
    <xsl:value-of select="$blank-line"/>

  </xsl:template>


  <!-- Utilities. -->


  <!-- Called at domain/association to generate the specification (no closing
       ";" or "is") for the linking function spec for associative
       associations. -->
  <xsl:template name="as:link-function-specification">

    <!--
         function Link
           ({role-a} : {a}.Handle;
            {role-b} : {b}.Handle) return {role-c}.Handle
         -->

    <xsl:value-of select="$I"/>
    <xsl:text>function Link&#10;</xsl:text>
    <xsl:value-of select="$IC"/>
    <xsl:text>(</xsl:text>
    <xsl:value-of select="role[1]/name"/>
    <xsl:text> : </xsl:text>
    <xsl:value-of select="role[1]/classname"/>
    <xsl:text>.Handle</xsl:text>
    <xsl:text>;&#10;</xsl:text>
    <xsl:value-of select="$IC"/>
    <xsl:text> </xsl:text>
    <xsl:value-of select="role[2]/name"/>
    <xsl:text> : </xsl:text>
    <xsl:value-of select="role[2]/classname"/>
    <xsl:text>.Handle)&#10;</xsl:text>
    <xsl:value-of select="$IC"/>
    <xsl:text>return </xsl:text>
    <xsl:value-of select="associative"/>
    <xsl:text>.Handle</xsl:text>

  </xsl:template>


  <!-- Called at domain/association to generate the specification (no closing
       ";" or "is") for the linking procedure spec for non-associative
       associations. -->
  <xsl:template name="as:link-procedure-specification">

    <!--
         procedure Link
           ({role-a} : {a}.Handle;
            {role-b} : {b}.Handle)
         -->

    <xsl:value-of select="$I"/>
    <xsl:text>procedure Link&#10;</xsl:text>
    <xsl:value-of select="$IC"/>
    <xsl:text>(</xsl:text>
    <xsl:value-of select="role[1]/name"/>
    <xsl:text> : </xsl:text>
    <xsl:value-of select="role[1]/classname"/>
    <xsl:text>.Handle</xsl:text>
    <xsl:text>;&#10;</xsl:text>
    <xsl:value-of select="$IC"/>
    <xsl:text> </xsl:text>
    <xsl:value-of select="role[2]/name"/>
    <xsl:text> : </xsl:text>
    <xsl:value-of select="role[2]/classname"/>
    <xsl:text>.Handle</xsl:text>
    <xsl:text>)</xsl:text>

  </xsl:template>


  <!-- Called at domain/association[associative] to generate the
       specification (no closing ";" or "is") for the find function
       spec for associative associations. -->
  <xsl:template name="as:association-find-specification">

    <!--
         function Find
           ({role-a} : {a}.Handle;
            {role-b} : {b}.Handle) return {role-c}.Handle
         -->

    <xsl:value-of select="$I"/>
    <xsl:text>function Find&#10;</xsl:text>
    <xsl:value-of select="$IC"/>
    <xsl:text>(</xsl:text>
    <xsl:value-of select="role[1]/name"/>
    <xsl:text> : </xsl:text>
    <xsl:value-of select="role[1]/classname"/>
    <xsl:text>.Handle</xsl:text>
    <xsl:text>;&#10;</xsl:text>
    <xsl:value-of select="$IC"/>
    <xsl:text> </xsl:text>
    <xsl:value-of select="role[2]/name"/>
    <xsl:text> : </xsl:text>
    <xsl:value-of select="role[2]/classname"/>
    <xsl:text>.Handle)&#10;</xsl:text>
    <xsl:value-of select="$IC"/>
    <xsl:text>return </xsl:text>
    <xsl:value-of select="associative"/>
    <xsl:text>.Handle</xsl:text>

  </xsl:template>


  <!-- Called at domain/association to generate the unlinking procedure
       specification (no closing ";" or "is") for associative
       associations. -->
  <xsl:template name="as:unlink-associative-specification">

    <xsl:value-of select="$I"/>
    <xsl:text>procedure Unlink&#10;</xsl:text>
    <xsl:value-of select="$IC"/>
    <xsl:text>(</xsl:text>
    <xsl:value-of select="associative"/>
    <xsl:text>_Handle : </xsl:text>
    <xsl:value-of select="associative"/>
    <xsl:text>.Handle)</xsl:text>

  </xsl:template>


  <!-- Called at domain/association to generate the unlinking procedure
       specification (no closing ";" or "is") for non-associative
       associations. -->
  <xsl:template name="as:unlink-specification">

    <xsl:value-of select="$I"/>
    <xsl:text>procedure Unlink&#10;</xsl:text>
    <xsl:value-of select="$IC"/>
    <xsl:text>(</xsl:text>
    <xsl:value-of select="role[1]/name"/>
    <xsl:text> : </xsl:text>
    <xsl:value-of select="role[1]/classname"/>
    <xsl:text>.Handle</xsl:text>
    <xsl:text>;&#10;</xsl:text>
    <xsl:value-of select="$IC"/>
    <xsl:text> </xsl:text>
    <xsl:value-of select="role[2]/name"/>
    <xsl:text> : </xsl:text>
    <xsl:value-of select="role[2]/classname"/>
    <xsl:text>.Handle</xsl:text>
    <xsl:text>)</xsl:text>

  </xsl:template>


  <!-- Called at domain/association to generate a navigation function
       spec (no closing ";" or "is"). -->
  <xsl:template name="as:navigation-specification">

    <xsl:param name="role-a"/>
    <xsl:param name="role-b"/>

    <xsl:value-of select="$I"/>
    <xsl:text>function </xsl:text>
    <xsl:value-of select="$role-a/name"/>
    <xsl:text>&#10;</xsl:text>
    <xsl:value-of select="$IC"/>
    <xsl:text>(</xsl:text>
    <xsl:value-of select="/domain/class[name=$role-a/classname]/abbreviation"/>
    <xsl:text> : </xsl:text>
    <xsl:value-of select="$role-a/classname"/>
    <xsl:text>.Handle)&#10;</xsl:text>
    <xsl:value-of select="$IC"/>
    <xsl:text>return </xsl:text>
    <xsl:value-of select="$role-b/classname"/>
    <xsl:choose>
      <xsl:when test="$role-b/@multiple">
        <xsl:text>.Collections.Collection</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>.Handle</xsl:text>
      </xsl:otherwise>
    </xsl:choose>

  </xsl:template>


  <!-- Called at domain/association to generate an associative navigation
       function spec (no closing ";" or "is"). -->
  <xsl:template name="as:navigation-from-associative-specification">

    <xsl:param name="role-a"/>
    <xsl:param name="role-b"/>   <!-- .. to this -->
    <xsl:param name="assoc"/>    <!-- from this .. -->

    <xsl:value-of select="$I"/>
    <xsl:text>function </xsl:text>
    <xsl:value-of select="$role-a/name"/>
    <xsl:text>&#10;</xsl:text>
    <xsl:value-of select="$IC"/>
    <xsl:text>(</xsl:text>
    <xsl:value-of select="/domain/class[name=$assoc]/abbreviation"/>
    <xsl:text> : </xsl:text>
    <xsl:value-of select="$assoc"/>
    <xsl:text>.Handle)&#10;</xsl:text>
    <xsl:value-of select="$IC"/>
    <xsl:text>return </xsl:text>
    <xsl:value-of select="$role-b/classname"/>
    <xsl:text>.Handle</xsl:text>

  </xsl:template>


  <!-- Called at domain/association to generate an associative navigation
       function spec (no closing ";" or "is"). -->
  <xsl:template name="as:navigation-to-associative-specification">

    <xsl:param name="role-a"/>   <!-- from this .. -->
    <xsl:param name="role-b"/>
    <xsl:param name="assoc"/>    <!-- .. to this -->

    <xsl:value-of select="$I"/>
    <xsl:text>function </xsl:text>
    <xsl:value-of select="$role-a/name"/>
    <xsl:text>&#10;</xsl:text>
    <xsl:value-of select="$IC"/>
    <xsl:text>(</xsl:text>
    <xsl:value-of select="/domain/class[name=$role-a/classname]/abbreviation"/>
    <xsl:text> : </xsl:text>
    <xsl:value-of select="$role-a/classname"/>
    <xsl:text>.Handle)&#10;</xsl:text>
    <xsl:value-of select="$IC"/>
    <xsl:text>return </xsl:text>
    <xsl:value-of select="$assoc"/>
    <xsl:choose>
      <xsl:when test="$role-b/@multiple">
        <xsl:text>.Collections.Collection</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>.Handle</xsl:text>
      </xsl:otherwise>
    </xsl:choose>

  </xsl:template>


</xsl:stylesheet>
