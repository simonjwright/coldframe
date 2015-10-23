<!-- XSL stylesheet to generate Ada code for AssociationClasses. -->
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
  xmlns:asc="http://pushface.org/coldframe/association-class"
  xmlns:at="http://pushface.org/coldframe/attribute"
  xmlns:ut="http://pushface.org/coldframe/utilities"
  version="1.0">

  <!-- Called at domain/class to check for invalid association classes. -->
  <xsl:template name="asc:check-association-validity">

    <xsl:variable
      name="class-a"
      select="../class[name=current()/associative/role[1]/classname]"/>
    <xsl:variable
      name="class-b"
      select="../class[name=current()/associative/role[2]/classname]"/>

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


  <!-- Called at domain/class to generate the linking subprogram spec -->
  <xsl:template name="asc:link-spec">
    <xsl:call-template name="asc:link-function-specification"/>
    <xsl:text>;&#10;</xsl:text>
    <xsl:value-of select="$blank-line"/>
  </xsl:template>


  <!-- Called at domain/class to generate the find function spec
       if any. -->
  <xsl:template name="asc:find-spec">
    <xsl:call-template name="asc:association-find-specification"/>
    <xsl:text>;&#10;</xsl:text>
    <xsl:value-of select="$blank-line"/>
  </xsl:template>


  <!-- Called at domain/class to generate the unlinking procedure
       spec -->
  <xsl:template name="asc:unlink-spec">
    <xsl:call-template name="asc:unlink-associative-specification"/>
    <xsl:text>;&#10;</xsl:text>
    <xsl:value-of select="$blank-line"/>
  </xsl:template>


  <!-- Called at domain/class to generate context clauses for the
       body. -->
  <xsl:template name="asc:association-body-context">

    <xsl:text>with </xsl:text>
    <xsl:value-of select="/domain/name"/>
    <xsl:text>.</xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>.Selection_Function;&#10;</xsl:text>

  </xsl:template>


  <!-- Called at domain/class to generate the linking subprogram body -->
  <xsl:template name="asc:link-body">

    <xsl:variable name="n" select="name"/>

    <!--
         function Link
           ({role-a} : not null {a}.Handle;
            {role-b} : not null {b}.Handle) return not null {role-c}.Handle is
            Result : {c}.Handle;
         begin
            Result := {c}.Create
              ((
         -->

    <xsl:call-template name="asc:link-function-specification"/>
    <xsl:text> is&#10;</xsl:text>

    <xsl:value-of select="$II"/>
    <xsl:text>Result : Handle;&#10;</xsl:text>

    <xsl:value-of select="$I"/>
    <xsl:text>begin&#10;</xsl:text>

    <xsl:value-of select="$II"/>
    <xsl:text>Result := Create&#10;</xsl:text>
    <xsl:value-of select="$IIC"/>
    <xsl:text>((</xsl:text>

    <xsl:choose>

      <xsl:when test="associative/role[1]/@multiple
                      and associative/role[2]/@multiple">
        <!-- Both ends multiple; the associative class' identifier
             references both ends. -->

        <!--
             {a-role-attr} => ColdFrame.Instances.Handle ({a-role}),
             {b-role-attr} => ColdFrame.Instances.Handle ({b-role})));
             -->

        <xsl:variable name="r1" select="associative/role[1]"/>
        <xsl:variable name="r2" select="associative/role[2]"/>

        <xsl:call-template name="at:attribute-name">
          <xsl:with-param
            name="a"
            select="/domain/class/attribute
                    [@relation=$n
                    and @refers=$r1/classname
                    and (not(@role) or @role=$r1/name)]"/>
        </xsl:call-template>
        <xsl:text> => ColdFrame.Instances.Handle (</xsl:text>
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
                    and (not(@role) or @role=$r2/name)]"/>
        </xsl:call-template>
        <xsl:text> => ColdFrame.Instances.Handle (</xsl:text>
        <xsl:value-of select="$r2/name"/>

        <xsl:text>)));&#10;</xsl:text>

      </xsl:when>

      <xsl:when test="associative/role[@multiple]">
        <!-- One end multiple; the associative class' identifier
             references the multiple end, the other end is
             referenced by a plain attribute. -->

        <!--
               {multiple-role-attr} => ColdFrame.Instances.Handle ({multiple-role})));
             {c}.Set_{rel}_{single-role}
                (Result, ColdFrame.Instances.Handle ({single-role}));
             -->

        <xsl:variable
          name="multiple-role"
          select="associative/role[@multiple]"/>
        <xsl:variable
          name="single-role"
          select="associative/role[not(@multiple)]"/>

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
        <xsl:text> => ColdFrame.Instances.Handle (</xsl:text>
        <xsl:value-of select="$multiple-role/name"/>
        <xsl:text>)));&#10;</xsl:text>

        <xsl:value-of select="$II"/>
        <xsl:text>Set_</xsl:text>
        <xsl:call-template name="at:attribute-name">
          <xsl:with-param
            name="a"
            select="/domain/class/attribute
                    [@relation=$n
                    and @refers=$single-role/classname
                    and (not(@role) or @role=$single-role/name)]"/>
        </xsl:call-template>
        <xsl:text>&#10;</xsl:text>
        <xsl:value-of select="$IIC"/>
        <xsl:text>(Result, ColdFrame.Instances.Handle (</xsl:text>
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

        <xsl:variable
          name="source-role"
          select="associative/role[@source]"/>
        <xsl:variable
          name="non-source-role"
          select="associative/role[not(@source)]"/>

        <xsl:call-template name="at:attribute-name">
          <xsl:with-param
            name="a"
            select="/domain/class/attribute
                    [@relation=$n
                    and @identifier]"/>
        </xsl:call-template>
        <xsl:text> => ColdFrame.Instances.Handle (</xsl:text>
        <xsl:value-of select="$source-role/name"/>
        <xsl:text>)));&#10;</xsl:text>

        <xsl:value-of select="$II"/>
        <xsl:text>Set_</xsl:text>
        <xsl:call-template name="at:attribute-name">
          <xsl:with-param
            name="a"
            select="/domain/class/attribute
                    [@relation=$n
                    and @refers=$non-source-role/classname
                    and (not(@role) or @role=$non-source-role/name)]"/>
        </xsl:call-template>
        <xsl:text>&#10;</xsl:text>
        <xsl:value-of select="$IIC"/>
        <xsl:text>(Result, ColdFrame.Instances.Handle (</xsl:text>
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

  </xsl:template>


  <!-- Called at domain/class to generate the find function body,
       if any. -->
  <xsl:template name="asc:find-body">
    <xsl:variable name="n" select="name"/>

   <!--
        function Find
          ({role-a} : {a}.Handle;
           {role-b} : {b}.Handle) return Handle is
        begin
           return Find
             ((
        -->

   <xsl:call-template name="asc:association-find-specification"/>
   <xsl:text> is&#10;</xsl:text>

   <!-- If there is a parameter that doesn't correspond to part of the
        identifier, it won't be referenced.
        -->
   <xsl:for-each select="associative/role">
     <xsl:variable
       name="attribute"
       select="../../attribute[@role=current()/name
               and @refers=current()/classname]"/>
     <xsl:if test="not($attribute/@identifier)">
       <xsl:value-of select="$II"/>
       <xsl:text>pragma Unreferenced (</xsl:text>
       <xsl:value-of select="name"/>
       <xsl:text>);&#10;</xsl:text>
     </xsl:if>
   </xsl:for-each>

   <xsl:value-of select="$I"/>
   <xsl:text>begin&#10;</xsl:text>

   <xsl:value-of select="$II"/>
   <xsl:text>return Find&#10;</xsl:text>
   <xsl:value-of select="$IIC"/>
   <xsl:text>((</xsl:text>

   <xsl:choose>

     <xsl:when test="associative/role[1]/@multiple
                     and associative/role[2]/@multiple">
       <!-- Both ends multiple; the associative class' identifier
            references both ends. -->

       <!--
            {a-role-attr} => ColdFrame.Instances.Handle ({a-role}),
            {b-role-attr} => ColdFrame.Instances.Handle ({b-role})));
            -->

       <xsl:variable name="r1" select="associative/role[1]"/>
       <xsl:variable name="r2" select="associative/role[2]"/>

       <xsl:call-template name="at:attribute-name">
         <xsl:with-param
           name="a"
           select="/domain/class/attribute
                   [@relation=$n
                   and @refers=$r1/classname
                   and (not(@role) or @role=$r1/name)]"/>
       </xsl:call-template>
       <xsl:text> => ColdFrame.Instances.Handle (</xsl:text>
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
                   and (not(@role) or @role=$r2/name)]"/>
       </xsl:call-template>
       <xsl:text> => ColdFrame.Instances.Handle (</xsl:text>
       <xsl:value-of select="$r2/name"/>

       <xsl:text>)));&#10;</xsl:text>

     </xsl:when>

     <xsl:when test="associative/role[@multiple]">

       <!-- One end multiple; the associative class' identifier
            references the multiple end, the other end is referenced
            by a plain attribute. -->

       <!--
            {multiple-role-attr} => ColdFrame.Instances.Handle ({multiple-role})));
            -->

       <xsl:variable
         name="multiple-role"
         select="associative/role[@multiple]"/>
       <xsl:variable
         name="single-role"
         select="associative/role[not(@multiple)]"/>

       <xsl:call-template name="at:attribute-name">
         <xsl:with-param
           name="a"
           select="/domain/class/attribute
                   [@relation=$n
                   and @refers=$multiple-role/classname
                   and @role=$multiple-role/name
                   and @identifier]"/>
         <!-- NB, had to be careful here because of possible reflexive
              associations. -->
       </xsl:call-template>
       <xsl:text> => ColdFrame.Instances.Handle (</xsl:text>
       <xsl:value-of select="$multiple-role/name"/>
       <xsl:text>)));&#10;</xsl:text>

     </xsl:when>

     <xsl:otherwise>
       <!-- Neither end multiple -->

       <!--
            {source-role-attr} => ColdFrame.Instances.Handle ({source-role})));
            -->

       <xsl:variable
         name="source-role"
         select="associative/role[@source]"/>
       <xsl:variable
         name="non-source-role"
         select="associative/role[not(@source)]"/>

       <xsl:call-template name="at:attribute-name">
         <xsl:with-param
           name="a"
           select="/domain/class/attribute
                   [@relation=$n
                   and @identifier]"/>
       </xsl:call-template>
       <xsl:text> => ColdFrame.Instances.Handle (</xsl:text>
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

  </xsl:template>


  <!-- Called at domain/class to generate the unlinking procedure
       body -->
  <xsl:template name="asc:unlink-body">

    <xsl:variable name="n" select="name"/>

     <!-- There's no point in trying to null out the referential
          attributes, since (presumably) the user's about to delete
          the associative class instance anyway.

             pragma Unreferenced ({associative}_Handle);
          begin
             null;
          end Unlink;
          -->
     <xsl:call-template name="asc:unlink-associative-specification"/>
     <xsl:text> is&#10;</xsl:text>

     <xsl:value-of select="$II"/>
     <xsl:text>pragma Unreferenced (</xsl:text>
     <xsl:value-of select="name"/>
     <xsl:text>_Handle);&#10;</xsl:text>

     <xsl:value-of select="$I"/>
     <xsl:text>begin&#10;</xsl:text>
     <xsl:value-of select="$II"/>
     <xsl:text>null;&#10;</xsl:text>
     <xsl:value-of select="$I"/>
     <xsl:text>end Unlink;&#10;</xsl:text>
     <xsl:value-of select="$blank-line"/>

   </xsl:template>


   <!-- Called at domain/class to generate the navigation function
        specs. -->
   <xsl:template name="asc:navigation-specs">

     <xsl:variable name="role-1" select="associative/role[1]"/>
     <xsl:variable
       name="singleton-1"
       select="/domain/class[name=$role-1/classname]/@singleton"/>
     <xsl:variable name="role-2" select="associative/role[2]"/>
     <xsl:variable
       name="singleton-2"
       select="/domain/class[name=$role-2/classname]/@singleton"/>

    <!-- First direction: from one -->

    <xsl:call-template name="asc:navigation-specification">
      <xsl:with-param name="role-a" select="associative/role[1]"/>
      <xsl:with-param name="role-b" select="associative/role[2]"/>
    </xsl:call-template>
    <xsl:text>;&#10;</xsl:text>
    <xsl:value-of select="$blank-line"/>

   <xsl:call-template name="asc:navigation-to-associative-specification">
     <xsl:with-param name="role-a" select="associative/role[1]"/>
     <xsl:with-param name="role-b" select="associative/role[2]"/>
     <xsl:with-param name="assoc" select="name"/>
   </xsl:call-template>
   <xsl:text>;&#10;</xsl:text>
   <xsl:value-of select="$blank-line"/>

   <xsl:call-template name="asc:navigation-from-associative-specification">
     <xsl:with-param name="role-a" select="associative/role[1]"/>
     <xsl:with-param name="role-b" select="associative/role[2]"/>
     <xsl:with-param name="assoc" select="name"/>
   </xsl:call-template>
   <xsl:text>;&#10;</xsl:text>
   <xsl:value-of select="$blank-line"/>

    <!-- Second direction: from one -->

    <xsl:call-template name="asc:navigation-specification">
      <xsl:with-param name="role-a" select="associative/role[2]"/>
      <xsl:with-param name="role-b" select="associative/role[1]"/>
    </xsl:call-template>
    <xsl:text>;&#10;</xsl:text>
    <xsl:value-of select="$blank-line"/>

   <xsl:call-template name="asc:navigation-to-associative-specification">
     <xsl:with-param name="role-a" select="associative/role[2]"/>
     <xsl:with-param name="role-b" select="associative/role[1]"/>
     <xsl:with-param name="assoc" select="name"/>
   </xsl:call-template>
   <xsl:text>;&#10;</xsl:text>
   <xsl:value-of select="$blank-line"/>

   <xsl:call-template name="asc:navigation-from-associative-specification">
     <xsl:with-param name="role-a" select="associative/role[2]"/>
     <xsl:with-param name="role-b" select="associative/role[1]"/>
     <xsl:with-param name="assoc" select="name"/>
   </xsl:call-template>
   <xsl:text>;&#10;</xsl:text>
   <xsl:value-of select="$blank-line"/>

  </xsl:template>


  <!-- Called at domain/class to generate the navigation function
       bodies. -->
  <xsl:template name="asc:navigation-bodies">

    <xsl:variable name="role-1" select="associative/role[1]"/>
    <xsl:variable
      name="singleton-1"
      select="/domain/class[name=$role-1/classname]/@singleton"/>
    <xsl:variable name="role-2" select="associative/role[2]"/>
    <xsl:variable
      name="singleton-2"
      select="/domain/class[name=$role-2/classname]/@singleton"/>

    <!-- First direction: from one -->

    <xsl:call-template name="asc:navigation-body">
      <xsl:with-param name="role-a" select="associative/role[1]"/>
      <xsl:with-param name="role-b" select="associative/role[2]"/>
    </xsl:call-template>

   <xsl:call-template name="asc:navigation-to-associative-body">
     <xsl:with-param name="role-a" select="associative/role[1]"/>
     <xsl:with-param name="role-b" select="associative/role[2]"/>
     <xsl:with-param name="assoc" select="name"/>
   </xsl:call-template>

   <xsl:call-template name="asc:navigation-from-associative-body">
     <xsl:with-param name="role-a" select="associative/role[1]"/>
     <xsl:with-param name="role-b" select="associative/role[2]"/>
     <xsl:with-param name="assoc" select="name"/>
   </xsl:call-template>

    <!-- Second direction: from one -->

    <xsl:call-template name="asc:navigation-body">
      <xsl:with-param name="role-a" select="associative/role[2]"/>
      <xsl:with-param name="role-b" select="associative/role[1]"/>
    </xsl:call-template>

   <xsl:call-template name="asc:navigation-to-associative-body">
     <xsl:with-param name="role-a" select="associative/role[2]"/>
     <xsl:with-param name="role-b" select="associative/role[1]"/>
     <xsl:with-param name="assoc" select="name"/>
   </xsl:call-template>

   <xsl:call-template name="asc:navigation-from-associative-body">
     <xsl:with-param name="role-a" select="associative/role[2]"/>
     <xsl:with-param name="role-b" select="associative/role[1]"/>
     <xsl:with-param name="assoc" select="name"/>
   </xsl:call-template>

  </xsl:template>


  <!-- Called at domain/class to generate a navigation function
       body. -->
  <xsl:template name="asc:navigation-body">
    <xsl:param name="role-a"/>   <!-- from this .. -->
    <xsl:param name="role-b"/>   <!-- .. to this -->

    <xsl:call-template name="asc:navigation-with-associative-body">
      <xsl:with-param name="role-a" select="$role-a"/>
      <xsl:with-param name="role-b" select="$role-b"/>
    </xsl:call-template>
  </xsl:template>


  <!-- Called at domain/class to generate a navigation function
       body for an associative association. -->
  <xsl:template name="asc:navigation-with-associative-body">
    <xsl:param name="role-a"/>   <!-- from this .. -->
    <xsl:param name="role-b"/>   <!-- .. to this -->

    <xsl:variable name="n" select="name"/>
    <xsl:variable name="a">
      <xsl:value-of select="$role-a/classname"/>
    </xsl:variable>
    <xsl:variable name="b">
      <xsl:value-of select="$role-b/classname"/>
    </xsl:variable>

    <xsl:call-template name="asc:navigation-specification">
      <xsl:with-param name="role-a" select="$role-a"/>
      <xsl:with-param name="role-b" select="$role-b"/>
    </xsl:call-template>
    <xsl:text> is&#10;</xsl:text>

    <xsl:choose>

      <xsl:when test="$role-b/@multiple">

        <!--
             {abbrev} : constant {class-name}.Vectors.Vector
               := {role-a} ({a-abbrev});
             It : {class-name}.Vectors.Cursor := {abbrev}.First;
             Result : {b}.Vectors.Vector (Capacity => {b-max});
             use type {class-name}.Vectors.Cursor;
             -->

        <!-- Calculate the maximum number of b (target) instances. -->
        <xsl:variable name="b-max">
          <xsl:call-template name="ut:number-of-instances">
            <xsl:with-param
              name="c"
              select="/domain/class[name=$role-b/classname]"/>
          </xsl:call-template>
        </xsl:variable>

        <xsl:value-of select="$II"/>
        <xsl:value-of
          select="abbreviation"/>
        <xsl:text> : constant </xsl:text>
        <xsl:value-of select="name"/>
        <xsl:text>.Vectors.Vector&#10;</xsl:text>
        <xsl:value-of select="$IIC"/>
        <xsl:text>:= </xsl:text>
        <xsl:value-of select="$role-a/name"/>
        <xsl:text> (</xsl:text>
        <xsl:value-of
          select="/domain/class[name=$role-a/classname]/abbreviation"/>
        <xsl:text>);&#10;</xsl:text>
        <xsl:value-of select="$II"/>
        <xsl:text>It : </xsl:text>
        <xsl:value-of select="name"/>
        <xsl:text>.Vectors.Cursor&#10;</xsl:text>
        <xsl:value-of select="$IIC"/>
        <xsl:text> := </xsl:text>
        <xsl:value-of select="abbreviation"/>
        <xsl:text>.First;&#10;</xsl:text>
        <xsl:value-of select="$II"/>
        <xsl:text>Result : </xsl:text>
        <xsl:value-of select="$b"/>
        <xsl:text>.Vectors.Vector</xsl:text>
        <xsl:if test="$b-max &lt;= $max-bounded-container">
          <xsl:text> (Capacity =&gt; </xsl:text>
          <xsl:value-of select="$b-max"/>
          <xsl:text>)</xsl:text>
        </xsl:if>
        <xsl:text>;&#10;</xsl:text>
        <xsl:value-of select="$II"/>
        <xsl:text>use type </xsl:text>
        <xsl:value-of select="name"/>
        <xsl:text>.Vectors.Cursor;&#10;</xsl:text>
      </xsl:when>

      <xsl:otherwise>

        <!--
             {abbrev} : constant Handle
               := {role-a} ({a-abbrev});
             -->

        <xsl:value-of select="$II"/>
        <xsl:value-of select="abbreviation"/>
        <xsl:text> : constant Handle&#10;</xsl:text>
        <xsl:value-of select="$IIC"/>
        <xsl:text>:= </xsl:text>
        <xsl:value-of select="$role-a/name"/>
        <xsl:text> (</xsl:text>
        <xsl:value-of
          select="/domain/class[name=$role-a/classname]/abbreviation"/>
        <xsl:text>);&#10;</xsl:text>

      </xsl:otherwise>

    </xsl:choose>

    <xsl:value-of select="$I"/>
    <xsl:text>begin&#10;</xsl:text>

    <xsl:choose>

      <xsl:when test="$role-b/@multiple">

        <!--
             while It /= {class-name}.Vectors.No_Element loop
                Result.Append
                  ({role-a}
                     ({class-name}.Vectors.Element (It)));
                {class-name}.Vectors.Next (It);
             end loop;
             return Result;
             -->

        <xsl:value-of select="$II"/>
        <xsl:text>while It /= </xsl:text>
        <xsl:value-of select="name"/>
        <xsl:text>.Vectors.No_Element loop&#10;</xsl:text>

        <xsl:value-of select="$III"/>
        <xsl:text>Result.Append&#10;</xsl:text>
        <xsl:value-of select="$IIIC"/>
        <xsl:text>(</xsl:text>
        <xsl:value-of select="$role-a/name"/>
        <xsl:text>&#10;</xsl:text>
        <xsl:value-of select="$IIIIC"/>
        <xsl:text>(</xsl:text>
        <xsl:value-of select="name"/>
        <xsl:text>.Vectors.Element (It)));&#10;</xsl:text>
        <xsl:value-of select="$III"/>
        <xsl:value-of select="name"/>
        <xsl:text>.Vectors.Next (It);&#10;</xsl:text>

        <xsl:value-of select="$II"/>
        <xsl:text>end loop;&#10;</xsl:text>
        <xsl:value-of select="$II"/>
        <xsl:text>return Result;&#10;</xsl:text>
      </xsl:when>

      <xsl:otherwise>

        <!--
             if {abbrev} = null then
               return null;
             else
               return {role-a} ({abbrev});
             end if;
             -->

        <xsl:value-of select="$II"/>
        <xsl:text>if </xsl:text>
        <xsl:value-of select="abbreviation"/>
        <xsl:text> = null then&#10;</xsl:text>
        <xsl:value-of select="$III"/>
        <xsl:text>return null;&#10;</xsl:text>
        <xsl:value-of select="$II"/>
        <xsl:text>else&#10;</xsl:text>
        <xsl:value-of select="$III"/>
        <xsl:text>return </xsl:text>
        <xsl:value-of  select="$role-a/name"/>
        <xsl:text> (</xsl:text>
        <xsl:value-of select="abbreviation"/>
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


  <!-- Called at domain/association to generate an associative navigation
       function body. -->
  <xsl:template name="asc:navigation-from-associative-body">
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

    <xsl:call-template name="asc:navigation-from-associative-specification">
      <xsl:with-param name="role-a" select="$role-a"/>
      <xsl:with-param name="role-b" select="$role-b"/>
      <xsl:with-param name="assoc" select="$assoc"/>
    </xsl:call-template>
    <xsl:text> is&#10;</xsl:text>

    <!--
         use type {assoc}.Handle;
         -->

    <xsl:value-of select="$II"/>
    <xsl:text>use type </xsl:text>
    <xsl:value-of select="$assoc"/>
    <xsl:text>.Handle;&#10;</xsl:text>

    <xsl:value-of select="$I"/>
    <xsl:text>begin&#10;</xsl:text>

    <!--
         if {assoc-abbr} = null then
            return null;
         else
            return {b}.Handle
              ({c}.Get_{b-ref-attr} ({assoc-abbr});
         end if;
         -->

    <xsl:value-of select="$II"/>
    <xsl:text>if </xsl:text>
    <xsl:value-of select="/domain/class[name=$assoc]/abbreviation"/>
    <xsl:text> = null then&#10;</xsl:text>
    <xsl:value-of select="$III"/>
    <xsl:text>return null;&#10;</xsl:text>
    <xsl:value-of select="$II"/>
    <xsl:text>else&#10;</xsl:text>
    <xsl:value-of select="$III"/>
    <xsl:text>return </xsl:text>
    <xsl:value-of select="$b"/>
    <xsl:text>.Handle&#10;</xsl:text>
    <xsl:value-of select="$IIIC"/>
    <xsl:text>(</xsl:text>
    <xsl:value-of select="$assoc"/>
    <xsl:text>.Get_</xsl:text>
    <xsl:call-template name="at:attribute-name">
      <xsl:with-param
        name="a"
        select="/domain/class/attribute
                [@relation=$n
                 and @refers=$role-b/classname
                 and (not(@role) or @role=$role-b/name)]"/>
    </xsl:call-template>
    <xsl:text> (</xsl:text>
    <xsl:value-of
      select="/domain/class[name=$assoc]/abbreviation"/>
    <xsl:text>));&#10;</xsl:text>
    <xsl:value-of select="$II"/>
    <xsl:text>end if;&#10;</xsl:text>

    <xsl:value-of select="$I"/>
    <xsl:text>end </xsl:text>
    <xsl:value-of select="$role-a/name"/>
    <xsl:text>;&#10;</xsl:text>
    <xsl:value-of select="$blank-line"/>

  </xsl:template>


  <!-- Called at domain/association to generate an associative navigation
       function body. -->
  <xsl:template name="asc:navigation-to-associative-body">
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

    <xsl:call-template name="asc:navigation-to-associative-specification">
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
         Result : constant {c}.Vectors.Vector := Find;
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
                [@relation=$n
                 and @refers=$role-a/classname
                 and (not(@role) or @role=$role-a/name)]"/>
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
    <xsl:text>.Vectors.Vector := Find;&#10;</xsl:text>

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
             if Result.Is_Empty then
                return null;
             else
                return Result.First_Element;
             end if;
             -->

       <xsl:value-of select="$II"/>
        <xsl:text>if Result.Is_Empty then&#10;</xsl:text>
        <xsl:value-of select="$III"/>
        <xsl:text>return null;&#10;</xsl:text>
        <xsl:value-of select="$II"/>
        <xsl:text>else&#10;</xsl:text>
        <!-- XXX case for checking there is only one! -->
        <xsl:value-of select="$III"/>
        <xsl:text>return Result.First_Element;</xsl:text>
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


  <!-- Called at domain/class[associative] to generate the
       specification (no closing ";" or "is") for the linking function
       spec for associative associations. -->
  <xsl:template
    name="asc:link-function-specification">

    <!--
         function Link
           ({role-a} : not null {a}.Handle;
            {role-b} : not null {b}.Handle) return not null Handle
         -->

    <xsl:value-of select="$I"/>
    <xsl:text>function Link&#10;</xsl:text>
    <xsl:value-of select="$IC"/>
    <xsl:text>(</xsl:text>
    <xsl:value-of select="associative/role[1]/name"/>
    <xsl:text> : not null </xsl:text>
    <xsl:value-of select="associative/role[1]/classname"/>
    <xsl:text>.Handle</xsl:text>
    <xsl:text>;&#10;</xsl:text>
    <xsl:value-of select="$IC"/>
    <xsl:text> </xsl:text>
    <xsl:value-of select="associative/role[2]/name"/>
    <xsl:text> : not null </xsl:text>
    <xsl:value-of select="associative/role[2]/classname"/>
    <xsl:text>.Handle)&#10;</xsl:text>
    <xsl:value-of select="$IC"/>
    <xsl:text>return not null Handle</xsl:text>

  </xsl:template>


  <!-- Called at domain/class[associative] to generate the
       specification (no closing ";" or "is") for the find function
       spec for associative associations. -->
  <xsl:template name="asc:association-find-specification">

    <!--
         function Find
           ({role-a} : not null {a}.Handle;
            {role-b} : not null {b}.Handle) return not null Handle
         -->

    <xsl:value-of select="$I"/>
    <xsl:text>function Find&#10;</xsl:text>
    <xsl:value-of select="$IC"/>
    <xsl:text>(</xsl:text>
    <xsl:value-of select="associative/role[1]/name"/>
    <xsl:text> : not null </xsl:text>
    <xsl:value-of select="associative/role[1]/classname"/>
    <xsl:text>.Handle</xsl:text>
    <xsl:text>;&#10;</xsl:text>
    <xsl:value-of select="$IC"/>
    <xsl:text> </xsl:text>
    <xsl:value-of select="associative/role[2]/name"/>
    <xsl:text> : not null </xsl:text>
    <xsl:value-of select="associative/role[2]/classname"/>
    <xsl:text>.Handle)&#10;</xsl:text>
    <xsl:value-of select="$IC"/>
    <xsl:text>return not null Handle</xsl:text>

  </xsl:template>


  <!-- Called at domain/class[associative] to generate the unlinking
       procedure specification (no closing ";" or "is") for
       associative associations. -->
  <xsl:template
    name="asc:unlink-associative-specification">

    <xsl:value-of select="$I"/>
    <xsl:text>procedure Unlink&#10;</xsl:text>
    <xsl:value-of select="$IC"/>
    <xsl:text>(</xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>_Handle : not null Handle)</xsl:text>

  </xsl:template>


  <!-- Called at domain/class[association] to generate a navigation
       function spec (no closing ";" or "is"). -->
  <xsl:template
    name="asc:navigation-specification">

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
        <xsl:text>.Vectors.Vector</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>.Handle</xsl:text>
      </xsl:otherwise>
    </xsl:choose>

  </xsl:template>


  <!-- Called at domain/association to generate an associative navigation
       function spec (no closing ";" or "is"). -->
  <xsl:template name="asc:navigation-from-associative-specification">

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
  <xsl:template name="asc:navigation-to-associative-specification">

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
        <xsl:text>.Vectors.Vector</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>.Handle</xsl:text>
      </xsl:otherwise>
    </xsl:choose>

  </xsl:template>


</xsl:stylesheet>
