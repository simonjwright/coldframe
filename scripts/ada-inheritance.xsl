<!-- XSL stylesheet to generate Ada code for Inheritance relationships. -->
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
  xmlns:in="http://pushface.org/coldframe/inheritance"
  xmlns:ut="http://pushface.org/coldframe/utilities"
  version="1.0">


  <!-- Generate specs for inheritance support. -->
  <xsl:template
    match="class"
    mode="in:inheritance-spec">

    <xsl:call-template name="ut:do-not-edit"/>
    <xsl:text>pragma Style_Checks (Off);&#10;</xsl:text>
    <xsl:call-template name="ut:identification-info"/>

    <xsl:call-template name="in:inheritance-spec-context"/>

    <xsl:text>package </xsl:text>
    <xsl:value-of select="../name"/>
    <xsl:text>.</xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>.Inheritance is&#10;</xsl:text>

    <xsl:value-of select="$blank-line"/>
    <xsl:call-template name="in:create-tree-heading"/>
    <xsl:text>;&#10;</xsl:text>

    <xsl:if test="../inheritance[parent=current()/name]">
      <xsl:value-of select="$blank-line"/>
      <xsl:call-template name="in:delete-child-heading"/>
      <xsl:text>;&#10;</xsl:text>
    </xsl:if>

    <xsl:call-template name="in:inheritance-find-spec"/>

    <xsl:value-of select="$blank-line"/>
    <xsl:text>end </xsl:text>
    <xsl:value-of select="../name"/>
    <xsl:text>.</xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>.Inheritance;&#10;</xsl:text>

  </xsl:template>


  <!-- Generate bodies for inheritance support. -->
  <xsl:template
    match="class"
    mode="in:inheritance-body">

    <xsl:call-template name="ut:do-not-edit"/>
    <xsl:text>pragma Style_Checks (Off);&#10;</xsl:text>
    <xsl:call-template name="ut:identification-info"/>

    <xsl:call-template name="in:inheritance-body-context"/>

    <xsl:text>package body </xsl:text>
    <xsl:value-of select="../name"/>
    <xsl:text>.</xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>.Inheritance is&#10;</xsl:text>

    <xsl:value-of select="$blank-line"/>
    <xsl:call-template name="in:create-tree-body"/>

    <xsl:if test="../inheritance[parent=current()/name]">
      <xsl:value-of select="$blank-line"/>
      <xsl:call-template name="in:delete-child-body"/>
    </xsl:if>

    <xsl:call-template name="in:inheritance-find-body"/>

    <xsl:value-of select="$blank-line"/>
    <xsl:text>end </xsl:text>
    <xsl:value-of select="../name"/>
    <xsl:text>.</xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>.Inheritance;&#10;</xsl:text>

  </xsl:template>


  <!-- Called at domain/class to generate the context for the inheritance
       support spec. -->
  <xsl:template name="in:inheritance-spec-context">

    <!-- We need all the parental classes. -->

    <xsl:param name="starting-at" select="."/>
    <xsl:param name="result" select="/.."/>

    <xsl:choose>

      <xsl:when test="../inheritance[child=$starting-at/name]">

        <xsl:call-template name="in:inheritance-spec-context">
          <xsl:with-param
            name="starting-at"
            select="/domain/class
                    [name=/domain/inheritance
                    [child=$starting-at/name]/parent]"/>
          <xsl:with-param
            name="result"
            select="../inheritance[child=$starting-at/name]/parent | $result"/>
        </xsl:call-template>

      </xsl:when>

      <xsl:otherwise>

        <xsl:for-each select="$result">
          <xsl:sort/>

          <xsl:text>with </xsl:text>
          <xsl:value-of select="/domain/name"/>
          <xsl:text>.</xsl:text>
          <xsl:value-of select="."/>
          <xsl:text>;&#10;</xsl:text>
        </xsl:for-each>

      </xsl:otherwise>

    </xsl:choose>

  </xsl:template>


  <!-- Called at domain/class to generate the context for the inheritance
       support body. -->
  <xsl:template name="in:inheritance-body-context">

    <!-- We need the immediate child classes. -->

    <xsl:for-each select="../inheritance[parent=current()/name]/child">
      <xsl:sort/>

      <xsl:text>with </xsl:text>
      <xsl:value-of select="/domain/name"/>
      <xsl:text>.</xsl:text>
      <xsl:value-of select="."/>
      <xsl:text>;&#10;</xsl:text>
    </xsl:for-each>

    <!-- We need the Inheritance packages for the immediate ancestors. -->
    <xsl:for-each select="../inheritance[child=current()/name]/parent">
      <xsl:sort/>
      <xsl:text>with </xsl:text>
      <xsl:value-of select="/domain/name"/>
      <xsl:text>.</xsl:text>
      <xsl:value-of select="."/>
      <xsl:text>.Inheritance;&#10;</xsl:text>
    </xsl:for-each>

    <!-- We need ColdFrame.Exceptions if we are a root class, or if
         we have more than one ultimate ancestor. -->
    <xsl:variable name="roots">
      <xsl:call-template name="in:ultimate-ancestors">
        <xsl:with-param
          name="starting-at"
          select="."/>
      </xsl:call-template>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="not(../inheritance[child=current()/name])">
        <xsl:text>with ColdFrame.Exceptions;&#10;</xsl:text>
      </xsl:when>
      <xsl:when test="count($roots/result/root)&gt;1">
        <xsl:text>with ColdFrame.Exceptions;&#10;</xsl:text>
      </xsl:when>
    </xsl:choose>

    <!-- XXX ColdFrame.Instances? (state machines) -->

  </xsl:template>


  <!-- Called at domain/class to generate the heading for the
       Create_Tree function. -->
  <xsl:template name="in:create-tree-heading">

    <xsl:variable name="roots">
      <xsl:call-template name="in:ultimate-ancestors"/>
    </xsl:variable>

    <xsl:value-of select="$I"/>
    <xsl:text>function Create_Tree&#10;</xsl:text>
    <xsl:value-of select="$IC"/>
    <xsl:text>(</xsl:text>

    <xsl:for-each select="$roots/result/root">
      <xsl:sort/>

      <xsl:value-of
        select="$main-document/domain/class[name=current()]/abbreviation"/>
      <xsl:text> : ColdFrame.Instances.Handle</xsl:text>
      <xsl:if test="not(position()=last())">
        <xsl:text>;&#10;</xsl:text>
        <xsl:value-of select="$IC"/>
        <xsl:text> </xsl:text>
      </xsl:if>
    </xsl:for-each>

    <xsl:text>) return Handle</xsl:text>

  </xsl:template>


  <!-- Called at domain/class to generate the heading for the
       Delete_Child procedure. -->
  <xsl:template name="in:delete-child-heading">
    <xsl:value-of select="$I"/>
    <xsl:text>procedure Delete_Child (This : Handle)</xsl:text>
  </xsl:template>


  <!-- Called at domain/class to generate Create_Tree functions. -->
  <xsl:template name="in:create-tree-body">

    <!--
         function Create_Tree
           ({rp1-abbrev} : ColdFrame.Instances.Handle;
            {rp2-abbrev} : ColdFrame.Instances.Handle;
            {rp3-abbrev} : ColdFrame.Instances.Handle) return Handle is
            {rp1-abbrev}_H : {p1}.Handle;
            {rp2-abbrev}_H : {p2}.Handle;
            {rp3-abbrev}_H : {p2}.Handle;
            use type ColdFrame.Instances.Handle;
         begin
           {body - see below}
         end Create_Tree;
         -->

    <!-- The relationships in which this class is a child.
         The structure is
            relation
               name
               parent-name
               parent-abbrev
               root-abbrev*
         -->
    <xsl:variable name="rels">
      <xsl:for-each select="../inheritance[child=current()/name]">
        <xsl:element name="relation">
          <xsl:element name="name">
            <xsl:value-of select="name"/>
          </xsl:element>
          <xsl:element name="parent-name">
            <xsl:value-of select="parent"/>
          </xsl:element>
          <xsl:element name="parent-abbrev">
            <xsl:value-of
              select="../class[name=current()/parent]/abbreviation"/>
          </xsl:element>
          <xsl:variable name="roots">
            <xsl:call-template name="in:ultimate-ancestors">
              <xsl:with-param
                name="starting-at"
                select="../class[name=current()/parent]"/>
            </xsl:call-template>
          </xsl:variable>
          <xsl:for-each select="$roots/result/root">
            <xsl:element name="root-abbrev">
              <xsl:value-of
                select="$main-document/domain/class[name=current()]
                        /abbreviation"/>
            </xsl:element>
          </xsl:for-each>
        </xsl:element>
      </xsl:for-each>
    </xsl:variable>

    <xsl:call-template name="in:create-tree-heading"/>
    <xsl:text> is&#10;</xsl:text>

    <xsl:for-each select="$rels/relation">
      <xsl:sort select="parent-name"/>

      <xsl:value-of select="$II"/>
      <xsl:value-of select="parent-abbrev"/>
      <xsl:text>_H : </xsl:text>
      <xsl:value-of select="parent-name"/>
      <xsl:text>.Handle;&#10;</xsl:text>

    </xsl:for-each>

    <xsl:value-of select="$II"/>
    <xsl:text>use type ColdFrame.Instances.Handle;&#10;</xsl:text>
    <xsl:value-of select="$I"/>
    <xsl:text>begin&#10;</xsl:text>

    <xsl:choose>

      <xsl:when test="not($rels/relation)">

        <!-- This is a root node, so can have only one parameter. -->

        <!--
             if {abbrev} = null then
                return Create; - or raise No_Default_Create if not auto-id
             else
                if not ({abbrev}.all in Instance'Class) then
                   raise ColdFrame.Exceptions.Unexpected_Class;
                end if;
                - pragma Assert
                -   (Maps.Is_Bound (The_Container, (Id => Handle ({abbrev}).Id)),
                -    "unbound handle in Create_Tree");
                return Handle ({abbrev});
             end if;
             -->

        <xsl:value-of select="$II"/>
        <xsl:text>if </xsl:text>
        <xsl:value-of select="abbreviation"/>
        <xsl:text> = null then&#10;</xsl:text>

        <xsl:choose>

          <xsl:when test="count(attribute[@identifier])=1
                          and attribute[@identifier]/type='Autonumber'">
            <xsl:value-of select="$III"/>
            <xsl:text>return Create;&#10;</xsl:text> <!-- XXX -->
          </xsl:when>

          <xsl:otherwise>
             <xsl:value-of select="$III"/>
             <xsl:text>raise ColdFrame.Exceptions.No_Default_Create;&#10;</xsl:text>
             <xsl:value-of select="$III"/>
             <xsl:text>return null;&#10;</xsl:text>
         </xsl:otherwise>

        </xsl:choose>

        <xsl:value-of select="$II"/>
        <xsl:text>else&#10;</xsl:text>
        <xsl:value-of select="$III"/>
        <xsl:text>if not (</xsl:text>
        <xsl:value-of select="abbreviation"/>
        <xsl:text>.all in Instance'Class) then&#10;</xsl:text>
        <xsl:value-of select="$IIII"/>
        <xsl:text>raise ColdFrame.Exceptions.Unexpected_Class;&#10;</xsl:text>
        <xsl:value-of select="$III"/>
        <xsl:text>end if;&#10;</xsl:text>

        <!-- XXX this is hard; we'd have to make up the identifier.
        <xsl:value-of select="$III"/>
        <xsl:text>pragma Assert&#10;</xsl:text>
        <xsl:value-of select="$IIIC"/>
        <xsl:text>(Maps.Is_Bound (The_Container, (Id =&gt; Handle (</xsl:text>
        <xsl:value-of select="abbreviation"/>
        <xsl:text>).Id)),&#10;</xsl:text>
        <xsl:value-of select="$IIIC"/>
        <xsl:text> "unbound handle in Create_Tree");&#10;</xsl:text>
        -->

        <xsl:value-of select="$III"/>
        <xsl:text>return Handle (</xsl:text>
        <xsl:value-of select="abbreviation"/>
        <xsl:text>);&#10;</xsl:text>
        <xsl:value-of select="$II"/>
        <xsl:text>end if;&#10;</xsl:text>

      </xsl:when>

      <xsl:otherwise>

        <!--
             if {rp1-abbrev} = null
               or else not ({rp1-abbrev}.all in Instance'Class)
               or else {rp2-abbrev} = null
               or else not ({rp2-abbrev}.all in Instance'Class)
               or else {rp3-abbrev} = null
               or else not ({rp3-abbrev}.all in Instance'Class) then
                {p1-abbrev}_H := {p1}.Inheritance.Create_Tree
                   ({rp1-abbrev} => {rp1-abbrev},
                    {rp2-abbrev} => {rp2-abbrev});
                {p2-abbrev}_H := {p2}.Inheritance.Create_Tree
                   ({rp3-abbrev} => {rp3-abbrev});
                return Create
                  (({rel-1}_Parent => ColdFrame.Instances.Handle ({p1-abbrev}_H)),
                    {rel-2}_Parent => ColdFrame.Instances.Handle ({p2-abbrev}_H));
             else
                if {rp1-abbrev} /= {rp2-abbrev}
                  or else rp2-abbrev} /= {rp3-abbrev} then
                   raise ColdFrame.Exceptions.Mismatched_Instances;
                end if;
                - pragma Assert
                -   (Maps.Is_Bound (The_Container, (Id => Handle ({rp1-abbrev}).Id)),
                -    "unbound handle in Create_Tree");
                return Handle ({rp1-abbrev});
             end if;
             -->

        <xsl:value-of select="$II"/>
        <xsl:text>if </xsl:text>
        <xsl:for-each select="$rels/relation/root-abbrev">
          <xsl:sort/>

          <xsl:value-of select="."/>
          <xsl:text> = null&#10;</xsl:text>
          <xsl:value-of select="$IIC"/>
          <xsl:text>or else not (</xsl:text>
          <xsl:value-of select="."/>
          <xsl:text>.all in Instance'Class)</xsl:text>
          <xsl:if test="not(position()=last())">
            <xsl:text>&#10;</xsl:text>
            <xsl:value-of select="$IIC"/>
            <xsl:text>or else </xsl:text>
          </xsl:if>
        </xsl:for-each>
        <xsl:text> then&#10;</xsl:text>

        <!-- We need to create the parents and then the instance. -->

        <xsl:for-each select="$rels/relation">
          <xsl:sort select="parent-name"/>

          <xsl:value-of select="$III"/>
          <xsl:value-of select="parent-abbrev"/>
          <xsl:text>_H := </xsl:text>
          <xsl:value-of select="parent-name"/>
          <xsl:text>.Inheritance.Create_Tree&#10;</xsl:text>

          <xsl:value-of select="$IIIC"/>
          <xsl:text>(</xsl:text>
          <xsl:for-each select="root-abbrev">
            <xsl:sort/>
            <xsl:value-of select="."/>
            <xsl:text> =&gt; </xsl:text>
            <xsl:value-of select="."/>
            <xsl:if test="not(position()=last())">
              <xsl:text>,&#10;</xsl:text>
              <xsl:value-of select="$IIIC"/>
              <xsl:text> </xsl:text>
            </xsl:if>
          </xsl:for-each>
          <xsl:text>);&#10;</xsl:text>

        </xsl:for-each>

        <xsl:value-of select="$III"/>
        <xsl:text>return Create&#10;</xsl:text>
        <xsl:value-of select="$IIIC"/>
        <xsl:text>((</xsl:text>

        <xsl:for-each select="$rels/relation">
          <xsl:sort select="name"/>

          <xsl:value-of select="name"/>
          <xsl:text>_Parent =&gt; ColdFrame.Instances.Handle (</xsl:text>
          <xsl:value-of select="parent-abbrev"/>
          <xsl:text>_H)</xsl:text>
          <xsl:if test="not(position()=last())">
            <xsl:text>,&#10;</xsl:text>
            <xsl:value-of select="$IIIC"/>
            <xsl:text>  </xsl:text>
          </xsl:if>
        </xsl:for-each>
        <xsl:text>));&#10;</xsl:text>

        <xsl:value-of select="$II"/>
        <xsl:text>else&#10;</xsl:text>

        <xsl:if test="count($rels/relation/root-abbrev)&gt;1">

          <!-- We need to check that all the "root" handles are the same. -->

          <xsl:value-of select="$III"/>
          <xsl:text>if </xsl:text>

          <!-- Need a variable here .. following-sibling won't match
               otherwise, because we'd have to go back up (and the
               sort would be wrong, too). -->
          <xsl:variable name="roots">
            <xsl:for-each select="$rels/relation/root-abbrev">
              <xsl:sort/>
              <xsl:element name="root">
                <xsl:value-of select="."/>
              </xsl:element>
            </xsl:for-each>
          </xsl:variable>

          <xsl:for-each select="$roots/root">
            <xsl:if test="not(position()=last())">
              <xsl:value-of select="."/>
              <xsl:text> /= </xsl:text>
              <xsl:value-of select="following-sibling::node()"/>
              <xsl:if test="position()&lt;(last()-1)">
                <xsl:text>&#10;</xsl:text>
                <xsl:value-of select="$IIIC"/>
                <xsl:text> or else </xsl:text>
              </xsl:if>
            </xsl:if>
          </xsl:for-each>
          <xsl:text> then&#10;</xsl:text>
          <xsl:value-of select="$IIII"/>
          <xsl:text>raise ColdFrame.Exceptions.Mismatched_Instances;&#10;</xsl:text>
          <xsl:value-of select="$III"/>
          <xsl:text>end if;&#10;</xsl:text>

        </xsl:if>

        <!-- XXX difficult .. need to create identifiers ..
        <xsl:value-of select="$III"/>
        <xsl:text>pragma Assert&#10;</xsl:text>
        <xsl:value-of select="$IIIC"/>
        <xsl:text>(Maps.Is_Bound (The_Container, (Id =&gt; Handle (</xsl:text>
        <xsl:value-of select="$rels/relation[1]/root-abbrev[1]"/>
        <xsl:text>).Id)),&#10;</xsl:text>
        <xsl:value-of select="$IIIC"/>
        <xsl:text> "unbound handle in Create_Tree");&#10;</xsl:text>
        -->

        <xsl:value-of select="$III"/>
        <xsl:text>return Handle (</xsl:text>
        <xsl:value-of select="$rels/relation/root-abbrev"/>
        <xsl:text>);&#10;</xsl:text>

        <xsl:value-of select="$II"/>
        <xsl:text>end if;&#10;</xsl:text>

      </xsl:otherwise>

    </xsl:choose>

    <xsl:value-of select="$I"/>
    <xsl:text>end Create_Tree;&#10;</xsl:text>

  </xsl:template>


  <!-- Called at domain/class to generate deletions for child instances. -->
  <xsl:template name="in:delete-child-body">

    <!--
         procedure Delete_Child (This : Handle) is
         begin
            case This.{rel-name}_Current_Child.Current is
               when {child}_T =>
                  {child}.Delete
                    ({child}.Handle
                       (This.{rel-name}_Current_Child.{child-abbrev}));
               when Null_T =>
                  null;
            end case;
         end Delete_Child;
         -->

    <xsl:variable name="rel" select="../inheritance[parent=current()/name]"/>

    <xsl:call-template name="in:delete-child-heading"/>
    <xsl:text> is&#10;</xsl:text>
    <xsl:value-of select="$I"/>
    <xsl:text>begin&#10;</xsl:text>

    <xsl:value-of select="$II"/>
    <xsl:text>case This.</xsl:text>
    <xsl:value-of select="$rel/name"/>
    <xsl:text>_Current_Child.Current is&#10;</xsl:text>

    <xsl:for-each select="$rel/child">
      <xsl:sort/>

      <xsl:value-of select="$III"/>
      <xsl:text>when </xsl:text>
      <xsl:value-of select="."/>
      <xsl:text>_T =&gt;&#10;</xsl:text>
      <xsl:value-of select="$IIII"/>
      <xsl:value-of select="."/>
      <xsl:text>.Delete&#10;</xsl:text>
      <xsl:value-of select="$IIIIC"/>
      <xsl:text>(</xsl:text>
      <xsl:value-of select="."/>
      <xsl:text>.Handle&#10;</xsl:text>
      <xsl:value-of select="$IIIIIC"/>
      <xsl:text>(This.</xsl:text>
      <xsl:value-of select="$rel/name"/>
      <xsl:text>_Current_Child.</xsl:text>
      <xsl:value-of select="../../class[name=current()]/abbreviation"/>
      <xsl:text>));&#10;</xsl:text>
    </xsl:for-each>

    <xsl:value-of select="$III"/>
    <xsl:text>when Null_T =&gt;&#10;</xsl:text>
    <xsl:value-of select="$IIII"/>
    <xsl:text>null;&#10;</xsl:text>

    <xsl:value-of select="$II"/>
    <xsl:text>end case;&#10;</xsl:text>

    <xsl:value-of select="$I"/>
    <xsl:text>end Delete_Child;&#10;</xsl:text>

  </xsl:template>


  <!-- Called at domain/class to generate find-parent function specs. -->
  <xsl:template name="in:inheritance-find-spec">

    <xsl:variable name="parents">
      <xsl:call-template name="in:inheritance-find-parents"/>
    </xsl:variable>

    <xsl:for-each select="$parents/relation/ancestor">
      <xsl:sort/>

      <xsl:value-of select="$blank-line"/>
      <xsl:call-template name="in:inheritance-find-function-heading">
        <xsl:with-param
          name="class"
          select="."/>
      </xsl:call-template>
      <xsl:text>;&#10;</xsl:text>

    </xsl:for-each>

  </xsl:template>


  <!-- Called at domain/class to generate find-parent function bodies. -->
  <xsl:template name="in:inheritance-find-body">

    <xsl:variable name="parents">
      <xsl:call-template name="in:inheritance-find-parents"/>
    </xsl:variable>

    <xsl:for-each select="$parents/relation/ancestor">
      <xsl:sort/>

      <xsl:value-of select="$blank-line"/>
      <xsl:call-template name="in:inheritance-find-function-heading">
        <xsl:with-param
          name="class"
          select="."/>
      </xsl:call-template>
      <xsl:text> is&#10;</xsl:text>
      <xsl:value-of select="$I"/>
      <xsl:text>begin&#10;</xsl:text>

      <xsl:choose>

        <xsl:when test="@parent">

          <!-- The immediate parent
               return {class}.Handle (This.{rel}_Parent);
               -->

          <xsl:value-of select="$II"/>
          <xsl:text>return </xsl:text>
          <xsl:value-of select="."/>
          <xsl:text>.Handle (This.</xsl:text>
          <xsl:value-of select="../name"/>
          <xsl:text>_Parent);&#10;</xsl:text>

        </xsl:when>

        <xsl:otherwise>

          <!-- A more distant parent
               return {parent}.Inheritance.Find_{class}_Parent
                  (Find_{parent}_Parent (This));
               -->

          <xsl:variable name="parent" select="../ancestor[@parent]"/>

          <xsl:value-of select="$II"/>
          <xsl:text>return </xsl:text>
          <xsl:value-of select="$parent"/>
          <xsl:text>.Inheritance.Find_</xsl:text>
          <xsl:value-of select="."/>
          <xsl:text>_Parent&#10;</xsl:text>
          <xsl:value-of select="$IIC"/>
          <xsl:text>(Find_</xsl:text>
          <xsl:value-of select="$parent"/>
          <xsl:text>_Parent (This));&#10;</xsl:text>

       </xsl:otherwise>

      </xsl:choose>

      <xsl:value-of select="$I"/>
      <xsl:text>end Find_</xsl:text>
      <xsl:value-of select="."/>
      <xsl:text>_Parent;&#10;</xsl:text>

    </xsl:for-each>

  </xsl:template>


  <!-- Called to output a find-parent function heading. -->
  <xsl:template name="in:inheritance-find-function-heading">
    <xsl:param name="class"/>

    <!--
         function Find_{class}_Parent (This : Handle) return {class}.Handle
         -->

    <xsl:value-of select="$I"/>
    <xsl:text>function Find_</xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>_Parent (This : Handle) return </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Handle</xsl:text>

  </xsl:template>


  <!-- Called at domain/class to find the parent relationships for
       find-parent functions.
       The result is a result tree fragment (I think) containing
       <relation>
         <name>relation name</name>
         <ancestor>class name</ancestor>
         <ancestor parent='yes'>class name of first parent</ancestor>
       </relation>
       -->
  <xsl:template name="in:inheritance-find-parents">

    <xsl:for-each select="../inheritance[child=current()/name]">

      <xsl:element name="relation">
        <xsl:element name="name">
          <xsl:value-of select="name"/>
        </xsl:element>
        <xsl:element name="ancestor">
          <xsl:attribute name="parent">yes</xsl:attribute>
          <xsl:value-of select="parent"/>
        </xsl:element>
        <xsl:call-template name="in:inheritance-find-remote">
          <xsl:with-param
            name="starting-at"
            select="/domain/class[name=current()/parent]"/>
        </xsl:call-template>
      </xsl:element>

    </xsl:for-each>

  </xsl:template>


  <!-- Called at domain/class to find all the ancestors of a given class.
       Returns a result tree fragment containing multiple
       <ancestor>name of class</ancestor>
       -->
  <xsl:template name="in:inheritance-find-remote">
    <xsl:param name="starting-at" select="/.."/>
    <xsl:param name="result" select="/.."/>

     <xsl:variable
      name="rels"
      select="/domain/inheritance[child=$starting-at/name]"/>

    <xsl:choose>

      <xsl:when test="$rels">
        <xsl:call-template name="in:inheritance-find-remote">
          <xsl:with-param
            name="starting-at"
            select="/domain/class[name=$rels/parent]"/>
          <xsl:with-param
            name="result"
            select="$result | $rels/parent"/>
        </xsl:call-template>
      </xsl:when>

      <xsl:otherwise>

        <xsl:for-each select="$result">
          <xsl:element name="ancestor">
            <xsl:value-of select="."/>
          </xsl:element>
        </xsl:for-each>

      </xsl:otherwise>

    </xsl:choose>

  </xsl:template>


  <!-- Called to collect all the ultimate ancestors of the class.
       Result is returned in a nodeset containing the names in result/root. -->
  <xsl:template name="in:ultimate-ancestors">

    <xsl:param name="starting-at" select="."/>
    <xsl:param name="result" select="/.."/>

    <xsl:choose>

      <xsl:when test="$starting-at">

        <!-- This variable collects the names of all the classes in
             $starting-at which are not children (including the starting
             class).
             The reason is that when we do the recursive call below,
             where we'd like to select the nodes that aren't children,
             we can't select the ones in $starting-at (current() there
             refers to the current node when this template was called).
             We need version 1.1 to collect this nodeset. -->
        <xsl:variable name="roots">
          <xsl:for-each select="$starting-at">
            <xsl:if test="not(/domain/inheritance[child=current()/name])">
              <xsl:element name="root">
                <xsl:value-of select="name"/>
              </xsl:element>
            </xsl:if>
          </xsl:for-each>
        </xsl:variable>

        <xsl:call-template name="in:ultimate-ancestors">
          <xsl:with-param
            name="starting-at"
            select="../class[name=/domain/inheritance[child=$starting-at/name]
                    /parent]"/>
          <xsl:with-param
            name="result"
            select="$roots/root | $result"/>
        </xsl:call-template>
      </xsl:when>

      <xsl:otherwise>
        <xsl:element name="result">
          <xsl:for-each select="$result">
            <xsl:element name="root">
              <xsl:value-of select="."/>
            </xsl:element>
          </xsl:for-each>
        </xsl:element>
      </xsl:otherwise>

    </xsl:choose>

  </xsl:template>

</xsl:stylesheet>
