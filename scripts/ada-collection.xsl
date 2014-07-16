<!-- XSL stylesheet to generate Ada code for Collections, which are
     Ada.Containers.Vectors. -->
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
  xmlns:cl="http://pushface.org/coldframe/class"
  xmlns:co="http://pushface.org/coldframe/collection"
  xmlns:ut="http://pushface.org/coldframe/utilities"
  version="1.0">

  <!-- Called to generate Collection support packages (only for
       non-public, non-utility classes). -->
  <xsl:template
    match="class[not(@public or @utility)]"
    mode="co:collection-support">
    <xsl:apply-templates select="." mode="co:collection-support-spec"/>
    <xsl:apply-templates select="." mode="co:collection-support-body"/>
  </xsl:template>

  <xsl:template mode="co:collection-support" match="*"/>


  <!-- Called to generate collection support package specs. -->
  <xsl:template match="class" mode="co:collection-support-spec">

    <!-- Make the name of the parent class (Domain.Class) -->
    <xsl:variable name="class">
      <xsl:value-of select="../name"/>.<xsl:value-of select="name"/>
    </xsl:variable>

    <!-- Concrete Vectors package -->
    <xsl:call-template name="ut:do-not-edit"/>
    <xsl:text>pragma Style_Checks (Off);&#10;</xsl:text>
    <xsl:call-template name="ut:identification-info"/>
    <xsl:text>with Ada.Containers.Vectors;&#10;</xsl:text>
    <xsl:text>package </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Vectors&#10;</xsl:text>
    <xsl:text>is new Ada.Containers.Vectors&#10;</xsl:text>
    <xsl:value-of select="$C"/>
    <xsl:text>(Index_Type => Positive,&#10;</xsl:text>
    <xsl:value-of select="$C"/>
    <xsl:text> Element_Type => Handle);&#10;</xsl:text>

    <!-- Hash function for Handles -->
    <xsl:call-template name="ut:do-not-edit"/>
    <xsl:text>pragma Style_Checks (Off);&#10;</xsl:text>
    <xsl:call-template name="ut:identification-info"/>
    <xsl:text>with ColdFrame.Hash.Access_Hash;&#10;</xsl:text>
    <xsl:text>function </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Handle_Hash&#10;</xsl:text>
    <xsl:text>is new ColdFrame.Hash.Access_Hash&#10;</xsl:text>
    <xsl:value-of select="$C"/>
    <xsl:text>(T =&gt; Instance,&#10;</xsl:text>
    <xsl:value-of select="$C"/>
    <xsl:text> Access_T =&gt; Handle);&#10;</xsl:text>

    <!-- Concrete Sets package -->
    <!-- These are no longer needed to do navigation 'From_Vectors'.
         There's also the issue of how to create a set.
         Could generate something like

         with Van_Fleet.Van.Handle_Hash;
         with Van_Fleet.Van.Vectors;
         with Ada.Containers.Hashed_Sets;
         package Van_Fleet.Van.Sets is

            package Sets is new Ada.Containers.Hashed_Sets
              (Element_Type => Handle,
               Hash => Handle_Hash,
               Equivalent_Elements => "=",
               "=" => "=");

            type Set is new Sets.Set with null record;

            function Create (From : Vectors.Vector) return Set;

         end Van_Fleet.Van.Sets;

         package body Van_Fleet.Van.Sets is

            function Create (From : Vectors.Vector) return Set is
               C : Vectors.Cursor := Vectors.First (From);
               Result : Set;
               use type Vectors.Cursor;
            begin
               while C /= Vectors.No_Element loop
                  Result.Insert (Vectors.Element (C));
                  Vectors.Next (C);
               end loop;
               return Result;
            end Create;

         end Van_Fleet.Van.Sets;
         -->

    <!-- Function to return a Collection of all the Instances -->
    <!--
         with {domain}.{class}.Vectors;
         function {domain}.{class}.All_Instances
         return {domain}.{class}.Vectors.Vector;
         pragma Elaborate_Body
            ({domain}.{class}.All_Instances);
         -->
    <xsl:call-template name="ut:do-not-edit"/>
    <xsl:text>pragma Style_Checks (Off);&#10;</xsl:text>
    <xsl:call-template name="ut:identification-info"/>
    <xsl:text>with </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Vectors;&#10;</xsl:text>
    <xsl:text>function </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.All_Instances&#10;</xsl:text>
    <xsl:value-of select="$C"/>
    <xsl:text>return </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Vectors.Vector;&#10;</xsl:text>
    <xsl:text>pragma Elaborate_Body&#10;</xsl:text>
    <xsl:value-of select="$C"/>
    <xsl:text>(</xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.All_Instances);&#10;</xsl:text>

    <!-- Generic filter function to return a Collection of selected
         Instances -->
    <!--
         with {domain}.{class}.Vectors;
         generic
            with function Pass (This : Handle) return Boolean is <>;
         function {domain}.{class}.Selection_Function
         return {domain}.{class}.Vectors.Vector;
         pragma Elaborate_Body
            ({domain}.{class}.Selection_Function);
         -->
    <xsl:call-template name="ut:do-not-edit"/>
    <xsl:text>pragma Style_Checks (Off);&#10;</xsl:text>
    <xsl:call-template name="ut:identification-info"/>
    <xsl:text>with </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Vectors;&#10;</xsl:text>
    <xsl:text>generic&#10;</xsl:text>
    <xsl:value-of select="$I"/>
    <xsl:text>with function Pass (This : Handle) return Boolean is &lt;&gt;;&#10;</xsl:text>
    <xsl:text>function </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Selection_Function&#10;</xsl:text>
    <xsl:value-of select="$C"/>
    <xsl:text>return </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Vectors.Vector;&#10;</xsl:text>
    <xsl:text>pragma Elaborate_Body&#10;</xsl:text>
    <xsl:value-of select="$C"/>
    <xsl:text>(</xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Selection_Function);&#10;</xsl:text>

    <!-- Generic filter function for collections of Instances -->
    <!--
         with {domain}.{class}.Vectors;
         generic
            with function Pass (This : Handle) return Boolean is <>;
         function {domain}.{class}.Filter_Function
            (The_Vector : {domain}.{class}.Vectors.Vector)
         return {domain}.{class}.Vectors.Vector;
         pragma Elaborate_Body
            ({domain}.{class}.Filter_Function);
         -->
    <xsl:call-template name="ut:do-not-edit"/>
    <xsl:call-template name="ut:identification-info"/>
    <xsl:text>pragma Style_Checks (Off);&#10;</xsl:text>
    <xsl:text>with </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Vectors;&#10;</xsl:text>
    <xsl:text>generic&#10;</xsl:text>
    <xsl:value-of select="$I"/>
    <xsl:text>with function Pass (This : Handle) return Boolean is &lt;&gt;;&#10;</xsl:text>
    <xsl:text>function </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Filter_Function&#10;</xsl:text>
    <xsl:value-of select="$C"/>
    <xsl:text>(The_Vector : </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Vectors.Vector)&#10;</xsl:text>
    <xsl:value-of select="$C"/>
    <xsl:text>return </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Vectors.Vector;&#10;</xsl:text>
    <xsl:text>pragma Elaborate_Body&#10;</xsl:text>
    <xsl:value-of select="$C"/>
    <xsl:text>(</xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Filter_Function);&#10;</xsl:text>

    <!-- Iteration support -->
    <!--
         with {domain}.{class}.Vectors;
         generic
           with procedure Process (H : Handle);
         procedure {domain}.{class}.Iterate
            (Over : {domain}.{class}.Vectors.Vector);
         pragma Elaborate_Body
            ({domain}.{class}.Iterate);
         -->
    <xsl:call-template name="ut:do-not-edit"/>
    <xsl:text>pragma Style_Checks (Off);&#10;</xsl:text>
    <xsl:call-template name="ut:identification-info"/>
    <xsl:text>with </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Vectors;&#10;</xsl:text>
    <xsl:text>generic&#10;</xsl:text>
    <xsl:value-of select="$I"/>
    <xsl:text>with procedure Process (H : Handle);&#10;</xsl:text>
    <xsl:text>procedure </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Iterate&#10;</xsl:text>
    <xsl:value-of select="$C"/>
    <xsl:text>(Over : </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Vectors.Vector);&#10;</xsl:text>
    <xsl:text>pragma Elaborate_Body&#10;</xsl:text>
    <xsl:value-of select="$C"/>
    <xsl:text>(</xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Iterate);&#10;</xsl:text>

  </xsl:template>

  <xsl:template mode="co:collection-support-spec" match="*"/>


  <!-- Called to generate Vector support package bodies. -->
  <xsl:template match="class" mode="co:collection-support-body">

    <!-- Make the name of the parent class (Domain.Class) -->
    <xsl:variable name="class">
      <xsl:value-of select="../name"/>
      <xsl:text>.</xsl:text>
      <xsl:value-of select="name"/>
    </xsl:variable>

    <!-- Calculate the maximum number of instances. -->
    <xsl:variable name="max">
      <xsl:call-template name="ut:number-of-instances"/>
    </xsl:variable>

    <!-- Determine whether an array can be used. -->
    <xsl:variable name="array">
      <xsl:call-template name="ut:can-use-array"/>
    </xsl:variable>

    <!-- Function to return a Vector of all the Instances -->
    <!-- full version ..
         function {dom}.{class}.All_Instances
           return {dom}.{class}.Vectors.Vector is
            It : Vectors.Cursor := The_Container.First;
            Result : Vectors.Vector;
            use type Vectors.Cursor;
         begin
            while It /= No_Element loop
               Result.Append (Vectors.Element (It)));
               Vectors.Next (It);
            end loop;
            return Result;
         end {dom}.{class}.All_Instances;
         -->
    <xsl:call-template name="ut:do-not-edit"/>
    <xsl:text>pragma Style_Checks (Off);&#10;</xsl:text>
    <xsl:call-template name="ut:identification-info"/>

    <xsl:text>function </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.All_Instances&#10;</xsl:text>
    <xsl:value-of select="$C"/>
    <xsl:text>return </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Vectors.Vector is&#10;</xsl:text>

    <xsl:if test="$max &gt; 1 and $array = 'no'">
      <xsl:value-of select="$I"/>
      <xsl:text>It : Containers.Cursor := The_Container.First;&#10;</xsl:text>
      <xsl:value-of select="$I"/>
      <xsl:text>use type Containers.Cursor;&#10;</xsl:text>
    </xsl:if>

    <xsl:value-of select="$I"/>
    <xsl:text>Result : Vectors.Vector;&#10;</xsl:text>
    <xsl:text>begin&#10;</xsl:text>

    <xsl:choose>

      <xsl:when test="$max = 1">

        <xsl:value-of select="$I"/>
        <xsl:text>if This /= null then&#10;</xsl:text>
        <xsl:value-of select="$II"/>
        <xsl:text>Result.Append (This);&#10;</xsl:text>
        <xsl:value-of select="$I"/>
        <xsl:text>end if;&#10;</xsl:text>

      </xsl:when>

      <xsl:when test="$array = 'yes'">

        <xsl:value-of select="$I"/>
        <xsl:text>for H in The_Container'Range loop&#10;</xsl:text>
        <xsl:value-of select="$II"/>
        <xsl:text>if The_Container (H) /= null then&#10;</xsl:text>
        <xsl:value-of select="$III"/>
        <xsl:text>Result.Append (The_Container (H));&#10;</xsl:text>
        <xsl:value-of select="$II"/>
        <xsl:text>end if;&#10;</xsl:text>
        <xsl:value-of select="$I"/>
        <xsl:text>end loop;&#10;</xsl:text>

      </xsl:when>

      <xsl:otherwise>

        <xsl:value-of select="$I"/>
        <xsl:text>while It /= Containers.No_Element loop&#10;</xsl:text>
        <xsl:value-of select="$II"/>
        <xsl:text>Result.Append (Containers.Element (It));&#10;</xsl:text>
        <xsl:value-of select="$II"/>
        <xsl:text>Containers.Next (It);&#10;</xsl:text>
        <xsl:value-of select="$I"/>
        <xsl:text>end loop;&#10;</xsl:text>

      </xsl:otherwise>

    </xsl:choose>

    <xsl:value-of select="$I"/>
    <xsl:text>return Result;&#10;</xsl:text>
    <xsl:text>end </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.All_Instances;&#10;</xsl:text>

    <!-- Generic filter function to return a Vector of selected
         Instances -->
    <!-- full version ..
         function {dom}.{class}.Selection_Function
           return {dom}.{class}.Vectors.Vector is
            It : Containers.Cursor := The_Container.First;
            use type Containers.Cursor;
            Result : Vectors.Vector;
         begin
            while It /= Containers.No_Element loop
               declare
                  H : constant Handle := Handle (Containers.Element (It));
               begin
                  if Pass (H) then
                     Result.Append (H);
                  end if;
               end;
               Containers.Next (It);
            end loop;
            return Result;
         end {dom}.{class}.Selection_Function;
         -->
    <xsl:call-template name="ut:do-not-edit"/>
    <xsl:text>pragma Style_Checks (Off);&#10;</xsl:text>
    <xsl:call-template name="ut:identification-info"/>

    <xsl:text>function </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Selection_Function&#10;</xsl:text>
    <xsl:value-of select="$C"/>
    <xsl:text>return </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Vectors.Vector is&#10;</xsl:text>

    <xsl:if  test="$max &gt; 1 and $array = 'no'">
      <xsl:value-of select="$I"/>
      <xsl:text>It : Containers.Cursor := The_Container.First;&#10;</xsl:text>
      <xsl:value-of select="$I"/>
      <xsl:text>use type Containers.Cursor;&#10;</xsl:text>
    </xsl:if>

    <xsl:value-of select="$I"/>
    <xsl:text>Result : Vectors.Vector;&#10;</xsl:text>
    <xsl:text>begin&#10;</xsl:text>

    <xsl:choose>

      <xsl:when test="$max = 1">

        <xsl:value-of select="$I"/>
        <xsl:text>if This /= null and then Pass (This) then&#10;</xsl:text>
        <xsl:value-of select="$II"/>
        <xsl:text>Result.Append (This);&#10;</xsl:text>
        <xsl:value-of select="$I"/>
        <xsl:text>end if;&#10;</xsl:text>

      </xsl:when>

      <xsl:when test="$array = 'yes'">

        <xsl:value-of select="$I"/>
        <xsl:text>for H in The_Container'Range loop&#10;</xsl:text>
        <xsl:value-of select="$II"/>
        <xsl:text>if The_Container (H) /= null&#10;</xsl:text>
        <xsl:value-of select="$IIC"/>
        <xsl:text>and then Pass (The_Container (H)) then&#10;</xsl:text>
        <xsl:value-of select="$III"/>
        <xsl:text>Result.Append (The_Container (H));&#10;</xsl:text>
        <xsl:value-of select="$II"/>
        <xsl:text>end if;&#10;</xsl:text>
        <xsl:value-of select="$I"/>
        <xsl:text>end loop;&#10;</xsl:text>

      </xsl:when>

      <xsl:otherwise>

        <xsl:value-of select="$I"/>
        <xsl:text>while It /= Containers.No_Element loop&#10;</xsl:text>

        <xsl:value-of select="$II"/>
        <xsl:text>declare&#10;</xsl:text>
        <xsl:value-of select="$III"/>
        <xsl:text>H : constant Handle := Handle (Containers.Element (It));&#10;</xsl:text>
        <xsl:value-of select="$II"/>
        <xsl:text>begin&#10;</xsl:text>

        <xsl:value-of select="$III"/>
        <xsl:text>if Pass (H) then&#10;</xsl:text>
        <xsl:value-of select="$IIII"/>
        <xsl:text>Result.Append (H);&#10;</xsl:text>
        <xsl:value-of select="$III"/>
        <xsl:text>end if;&#10;</xsl:text>

        <xsl:value-of select="$II"/>
        <xsl:text>end;&#10;</xsl:text>

        <xsl:value-of select="$II"/>
        <xsl:text>Containers.Next (It);&#10;</xsl:text>
        <xsl:value-of select="$I"/>
        <xsl:text>end loop;&#10;</xsl:text>

      </xsl:otherwise>

    </xsl:choose>

    <xsl:value-of select="$I"/>
    <xsl:text>return Result;&#10;</xsl:text>
    <xsl:text>end </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Selection_Function;&#10;</xsl:text>

    <!-- Generic filter function for collections of Instances -->
    <!--
         function {dom}.{class}.Filter_Function
           (The_Vector : {dom}.{class}.Vectors.Vector)
           return {dom}.{class}.Vectors.Vector is
            It : Vectors.Cursor := The_Vector.First;
            Result : Vectors.Vector;
            use type Vectors.Cursor;
         begin
            while It /= Vectors.No_Element loop
               declare
                  H : constant Handle := Vectors.Element (It);
               begin
                  if Pass (H) then
                     Result.Append (H);
                  end if;
               end;
               Vectors.Next (It);
            end loop;
            return Result;
         end {dom}.{class}.Filter_Function;
         -->
    <xsl:call-template name="ut:do-not-edit"/>
    <xsl:text>pragma Style_Checks (Off);&#10;</xsl:text>
    <xsl:call-template name="ut:identification-info"/>

    <xsl:text>function </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Filter_Function&#10;</xsl:text>
    <xsl:value-of select="$C"/>
    <xsl:text>(The_Vector : </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Vectors.Vector)&#10;</xsl:text>
    <xsl:value-of select="$C"/>
    <xsl:text>return </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Vectors.Vector is&#10;</xsl:text>

    <xsl:value-of select="$I"/>
    <xsl:text>It : Vectors.Cursor := The_Vector.First;&#10;</xsl:text>
    <xsl:value-of select="$I"/>
    <xsl:text>Result : Vectors.Vector;&#10;</xsl:text>
    <xsl:value-of select="$I"/>
    <xsl:text>use type Vectors.Cursor;&#10;</xsl:text>
    <xsl:text>begin&#10;</xsl:text>

    <xsl:value-of select="$I"/>
    <xsl:text>while It /= Vectors.No_Element loop&#10;</xsl:text>

    <xsl:value-of select="$II"/>
    <xsl:text>declare&#10;</xsl:text>
    <xsl:value-of select="$III"/>
    <xsl:text>H : constant Handle := Vectors.Element (It);&#10;</xsl:text>
    <xsl:value-of select="$II"/>
    <xsl:text>begin&#10;</xsl:text>

    <xsl:value-of select="$III"/>
    <xsl:text>if Pass (H) then&#10;</xsl:text>
    <xsl:value-of select="$IIII"/>
    <xsl:text>Result.Append (H);&#10;</xsl:text>
    <xsl:value-of select="$III"/>
    <xsl:text>end if;&#10;</xsl:text>

    <xsl:value-of select="$II"/>
    <xsl:text>end;&#10;</xsl:text>

    <xsl:value-of select="$II"/>
    <xsl:text>Vectors.Next (It);&#10;</xsl:text>
    <xsl:value-of select="$I"/>
    <xsl:text>end loop;&#10;</xsl:text>

    <xsl:value-of select="$I"/>
    <xsl:text>return Result;&#10;</xsl:text>
    <xsl:text>end </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Filter_Function;&#10;</xsl:text>

    <!-- Iteration support -->
    <!--
         procedure {domain}.{class}.Iterate
           (Over : {domain}.{class}.Vectors.Vector) is
            It : Vectors.Cursor := Over.First;
            use type Vectors.Cursor;
         begin
         while It /= Vectors.No_Element loop
               Process (Vectors.Element (It));
               Vectors.Next (It);
            end loop;
         end {domain}.{class}.Iterate;
         -->
    <xsl:call-template name="ut:do-not-edit"/>
    <xsl:text>pragma Style_Checks (Off);&#10;</xsl:text>
    <xsl:call-template name="ut:identification-info"/>
    <xsl:text>procedure </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Iterate&#10;</xsl:text>
    <xsl:value-of select="$C"/>
    <xsl:text>(Over : </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Vectors.Vector) is&#10;</xsl:text>
    <xsl:value-of select="$I"/>
    <xsl:text>It : Vectors.Cursor := Over.First;&#10;</xsl:text>
    <xsl:value-of select="$I"/>
    <xsl:text>use type Vectors.Cursor;&#10;</xsl:text>
    <xsl:text>begin&#10;</xsl:text>
    <xsl:value-of select="$I"/>
    <xsl:text>while It /= Vectors.No_Element loop&#10;</xsl:text>
    <xsl:value-of select="$II"/>
    <xsl:text>Process (Vectors.Element (It));&#10;</xsl:text>
    <xsl:value-of select="$II"/>
    <xsl:text>Vectors.Next (It);&#10;</xsl:text>
    <xsl:value-of select="$I"/>
    <xsl:text>end loop;&#10;</xsl:text>
    <xsl:text>end </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Iterate;&#10;</xsl:text>


  </xsl:template>

  <xsl:template mode="co:collection-support-body" match="*"/>


</xsl:stylesheet>
