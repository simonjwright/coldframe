<!-- $Id: ada-collection.xsl,v 55643a9356e6 2003/09/06 06:49:24 simon $ -->
<!-- XSL stylesheet to generate Ada code for Collections. -->
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

  <!-- Called to generate Collection support packages (only for
       non-singleton classes). -->
  <xsl:template match="class[not(@singleton)]" mode="collection-support">
    <xsl:apply-templates select="." mode="collection-support-spec"/>
    <xsl:apply-templates select="." mode="collection-support-body"/>
  </xsl:template>

  <xsl:template mode="collection-support" match="*"/>


  <!-- Called to generate Collection support package specs. -->
  <xsl:template match="class" mode="collection-support-spec">

    <!-- Make the name of the parent class (Domain.Class) -->
    <xsl:variable name="class">
      <xsl:value-of select="../name"/>.<xsl:value-of select="name"/>
    </xsl:variable>

    <!-- Calculate the maximum number of instances. -->
    <xsl:variable name="max">
      <xsl:call-template name="number-of-instances"/>
    </xsl:variable>

    <!-- Abstract Containers package -->
    <xsl:call-template name="do-not-edit"/>
    <xsl:call-template name="identification-info"/>
    <xsl:text>with BC.Containers;&#10;</xsl:text>
    <xsl:text>package </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Abstract_Containers&#10;</xsl:text>
    <xsl:text>is new BC.Containers (Handle);&#10;</xsl:text>

    <!-- Abstract Collections package -->
    <xsl:call-template name="do-not-edit"/>
    <xsl:call-template name="identification-info"/>
    <xsl:text>with BC.Containers.Collections;&#10;</xsl:text>
    <xsl:text>with </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Abstract_Containers;&#10;</xsl:text>
    <xsl:text>package </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Abstract_Collections&#10;</xsl:text>
    <xsl:text>is new </xsl:text>
    <xsl:text>Abstract_Containers.Collections;&#10;</xsl:text>

    <!-- Abstract Sets package -->
    <xsl:call-template name="do-not-edit"/>
    <xsl:call-template name="identification-info"/>
    <xsl:text>with BC.Containers.Sets;&#10;</xsl:text>
    <xsl:text>with </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Abstract_Containers;&#10;</xsl:text>
    <xsl:text>package </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Abstract_Sets&#10;</xsl:text>
    <xsl:text>is new </xsl:text>
    <xsl:text>Abstract_Containers.Sets;&#10;</xsl:text>

    <!-- Concrete Collections package -->
    <xsl:choose>

      <xsl:when test="$max &lt;= $max-bounded-container">
        <!-- Wnen the size isn't too large, use the Bounded version -->
        <xsl:call-template name="do-not-edit"/>
        <xsl:call-template name="identification-info"/>
        <xsl:text>with BC.Containers.Collections.Bounded;&#10;</xsl:text>
        <xsl:text>with </xsl:text>
        <xsl:value-of select="$class"/>
        <xsl:text>.Abstract_Collections;&#10;</xsl:text>
        <xsl:text>package </xsl:text>
        <xsl:value-of select="$class"/>
        <xsl:text>.Collections&#10;</xsl:text>
        <xsl:text>is new Abstract_Collections.Bounded (Maximum_Size =&gt; </xsl:text>
        <xsl:value-of select="$max"/>
        <xsl:text>);&#10;</xsl:text>
      </xsl:when>

      <xsl:otherwise>
        <!-- Use the Unbounded version -->
        <xsl:call-template name="do-not-edit"/>
        <xsl:call-template name="identification-info"/>
        <xsl:text>with BC.Containers.Collections.Unbounded;&#10;</xsl:text>
        <xsl:text>with ColdFrame.Project.Storage_Pools;&#10;</xsl:text>
        <xsl:text>with </xsl:text>
        <xsl:value-of select="$class"/>
        <xsl:text>.Abstract_Collections;&#10;</xsl:text>
        <xsl:text>package </xsl:text>
        <xsl:value-of select="$class"/>
        <xsl:text>.Collections&#10;</xsl:text>
        <xsl:text>is new Abstract_Collections.Unbounded&#10;</xsl:text>
        <xsl:value-of select="$C"/>
        <xsl:text>(Storage =&gt; ColdFrame.Project.Storage_Pools.Pool);&#10;</xsl:text>
      </xsl:otherwise>

    </xsl:choose>

    <!-- Hash function for Handles -->
    <xsl:call-template name="do-not-edit"/>
    <xsl:call-template name="identification-info"/>
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
    <xsl:call-template name="do-not-edit"/>
    <xsl:call-template name="identification-info"/>
    <xsl:text>with </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Handle_Hash;&#10;</xsl:text>

    <xsl:choose>

      <xsl:when test="$max &lt;= $max-bounded-container">
        <!-- Wnen the size isn't too large, use the Bounded version -->
        <xsl:text>with BC.Containers.Sets.Bounded;&#10;</xsl:text>
        <xsl:text>with </xsl:text>
        <xsl:value-of select="$class"/>
        <xsl:text>.Abstract_Sets;&#10;</xsl:text>
        <xsl:text>package </xsl:text>
        <xsl:value-of select="$class"/>
        <xsl:text>.Sets&#10;</xsl:text>
        <xsl:text>is new Abstract_Sets.Bounded&#10;</xsl:text>
        <xsl:value-of select="$C"/>
        <xsl:text>(Hash =&gt; </xsl:text>
        <xsl:value-of select="$class"/>
        <xsl:text>.Handle_Hash,&#10;</xsl:text>
        <xsl:value-of select="$C"/>
        <xsl:text> Buckets =&gt; </xsl:text>
        <xsl:call-template name="hash-buckets"/>
        <xsl:text>,&#10;</xsl:text>
        <xsl:value-of select="$C"/>
        <xsl:text> Maximum_Size =&gt; </xsl:text>
        <xsl:value-of select="$max"/>
        <xsl:text>);&#10;</xsl:text>
      </xsl:when>

      <xsl:otherwise>
        <!-- Use the Unbounded version -->
        <xsl:text>with BC.Containers.Sets.Unbounded;&#10;</xsl:text>
        <xsl:text>with ColdFrame.Project.Storage_Pools;&#10;</xsl:text>
        <xsl:text>with </xsl:text>
        <xsl:value-of select="$class"/>
        <xsl:text>.Abstract_Sets;&#10;</xsl:text>
        <xsl:text>package </xsl:text>
        <xsl:value-of select="$class"/>
        <xsl:text>.Sets&#10;</xsl:text>
        <xsl:text>is new Abstract_Sets.Unbounded&#10;</xsl:text>
        <xsl:value-of select="$C"/>
        <xsl:text> (Hash =&gt; </xsl:text>
        <xsl:value-of select="$class"/>
        <xsl:text>.Handle_Hash,&#10;</xsl:text>
        <xsl:value-of select="$C"/>
        <xsl:text> Buckets =&gt; </xsl:text>
        <xsl:call-template name="hash-buckets"/>
        <xsl:text>,&#10;</xsl:text>
        <xsl:value-of select="$C"/>
        <xsl:text> Storage =&gt; ColdFrame.Project.Storage_Pools.Pool);&#10;</xsl:text>
      </xsl:otherwise>

    </xsl:choose>

    <!-- Function to return a Collection of all the Instances -->
    <!--
         with {domain}.{class}.Collections;
         function {domain}.{class}.All_Instances
         return {domain}.{class}.Collections.Collection;
         pragma Elaborate_Body
            ({domain}.{class}.All_Instances);
         -->
    <xsl:call-template name="do-not-edit"/>
    <xsl:call-template name="identification-info"/>
    <xsl:text>with </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Collections;&#10;</xsl:text>
    <xsl:text>function </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.All_Instances&#10;</xsl:text>
    <xsl:value-of select="$C"/>
    <xsl:text>return </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Collections.Collection;&#10;</xsl:text>
    <xsl:text>pragma Elaborate_Body&#10;</xsl:text>
    <xsl:value-of select="$C"/>
    <xsl:text>(</xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.All_Instances);&#10;</xsl:text>

    <!-- Generic filter function to return a Collection of selected
         Instances -->
    <!--
         with {domain}.{class}.Collections;
         generic
            with function Pass (This : Handle) return Boolean is <>;
         function {domain}.{class}.Selection_Function
         return {domain}.{class}.Collections.Collection;
         pragma Elaborate_Body
            ({domain}.{class}.Selection_Function);
         -->
    <xsl:call-template name="do-not-edit"/>
    <xsl:call-template name="identification-info"/>
    <xsl:text>with </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Collections;&#10;</xsl:text>
    <xsl:text>generic&#10;</xsl:text>
    <xsl:value-of select="$I"/>
    <xsl:text>with function Pass (This : Handle) return Boolean is &lt;&gt;;&#10;</xsl:text>
    <xsl:text>function </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Selection_Function&#10;</xsl:text>
    <xsl:value-of select="$C"/>
    <xsl:text>return </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Collections.Collection;&#10;</xsl:text>
    <xsl:text>pragma Elaborate_Body&#10;</xsl:text>
    <xsl:value-of select="$C"/>
    <xsl:text>(</xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Selection_Function);&#10;</xsl:text>

    <!-- Generic filter function for collections of Instances -->
    <!--
         with {domain}.{class}.Collections;
         generic
            with function Pass (This : Handle) return Boolean is <>;
         function {domain}.{class}.Filter_Function
            (The_Collection : {domain}.{class}.Collections.Collection)
         return {domain}.{class}.Collections.Collection;
         pragma Elaborate_Body
            ({domain}.{class}.Filter_Function);
         -->
    <xsl:call-template name="do-not-edit"/>
    <xsl:call-template name="identification-info"/>
    <xsl:text>with </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Collections;&#10;</xsl:text>
    <xsl:text>generic&#10;</xsl:text>
    <xsl:value-of select="$I"/>
    <xsl:text>with function Pass (This : Handle) return Boolean is &lt;&gt;;&#10;</xsl:text>
    <xsl:text>function </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Filter_Function&#10;</xsl:text>
    <xsl:value-of select="$C"/>
    <xsl:text>(The_Collection : </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Collections.Collection)&#10;</xsl:text>
    <xsl:value-of select="$C"/>
    <xsl:text>return </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Collections.Collection;&#10;</xsl:text>
    <xsl:text>pragma Elaborate_Body&#10;</xsl:text>
    <xsl:value-of select="$C"/>
    <xsl:text>(</xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Filter_Function);&#10;</xsl:text>

    <!-- Iteration support -->
    <!--
         with {domain}.{class}.Collections;
         generic
           with procedure Process (H : Handle);
         procedure {domain}.{class}.Iterate
            (Over : {domain}.{class}.Collections.Collection);
         pragma Elaborate_Body
            ({domain}.{class}.Iterate);
         -->
    <xsl:call-template name="do-not-edit"/>
    <xsl:call-template name="identification-info"/>
    <xsl:text>with </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Collections;&#10;</xsl:text>
    <xsl:text>generic&#10;</xsl:text>
    <xsl:value-of select="$I"/>
    <xsl:text>with procedure Process (H : Handle);&#10;</xsl:text>
    <xsl:text>procedure </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Iterate&#10;</xsl:text>
    <xsl:value-of select="$C"/>
    <xsl:text>(Over : </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Collections.Collection);&#10;</xsl:text>
    <xsl:text>pragma Elaborate_Body&#10;</xsl:text>
    <xsl:value-of select="$C"/>
    <xsl:text>(</xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Iterate);&#10;</xsl:text>

  </xsl:template>

  <xsl:template mode="collection-support-spec" match="*"/>


  <!-- Called to generate Collection support package bodies. -->
  <xsl:template match="class" mode="collection-support-body">

    <!-- Make the name of the parent class (Domain.Class) -->
    <xsl:variable name="class">
      <xsl:value-of select="../name"/>
      <xsl:text>.</xsl:text>
      <xsl:value-of select="name"/>
    </xsl:variable>

    <!-- Calculate the maximum number of instances. -->
    <xsl:variable name="max">
      <xsl:call-template name="number-of-instances"/>
    </xsl:variable>

    <!-- Function to return a Collection of all the Instances -->
    <!-- full version ..
         function {dom}.{class}.All_Instances
           return {dom}.{class}.Collections.Collection is
            use ColdFrame.Instances.Abstract_Containers;
            It : Iterator'Class := Maps.New_Iterator (The_Container);
            Result : Collections.Collection;
         begin
            while not Is_Done (It) loop
               Collections.Append (Result, Handle (Current_Item (It)));
               Next (It);
            end loop;
            return Result;
         end {dom}.{class}.All_Instances;
         -->
    <xsl:call-template name="do-not-edit"/>
    <xsl:call-template name="identification-info"/>

    <xsl:text>function </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.All_Instances&#10;</xsl:text>
    <xsl:value-of select="$C"/>
    <xsl:text>return </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Collections.Collection is&#10;</xsl:text>

    <xsl:if test="$max &gt; 1">
      <xsl:value-of select="$I"/>
      <xsl:text>use ColdFrame.Instances.Abstract_Containers;&#10;</xsl:text>
      <xsl:value-of select="$I"/>
      <xsl:text>It : Iterator'Class := Maps.New_Iterator (The_Container);&#10;</xsl:text>
    </xsl:if>

    <xsl:value-of select="$I"/>
    <xsl:text>Result : Collections.Collection;&#10;</xsl:text>
    <xsl:text>begin&#10;</xsl:text>

    <xsl:choose>
      
      <xsl:when test="$max &gt; 1">

        <xsl:value-of select="$I"/>
        <xsl:text>while not Is_Done (It) loop&#10;</xsl:text>
        <xsl:value-of select="$II"/>
        <xsl:text>Collections.Append (Result, Handle (Current_Item (It)));&#10;</xsl:text>
        <xsl:value-of select="$II"/>
        <xsl:text>Next (It);&#10;</xsl:text>
        <xsl:value-of select="$I"/>
        <xsl:text>end loop;&#10;</xsl:text>

      </xsl:when>

      <xsl:otherwise>

        <xsl:value-of select="$I"/>
        <xsl:text>if This /= null then&#10;</xsl:text>
        <xsl:value-of select="$II"/>
        <xsl:text>Collections.Append (Result, This);&#10;</xsl:text>
        <xsl:value-of select="$I"/>
        <xsl:text>end if;&#10;</xsl:text>

      </xsl:otherwise>

    </xsl:choose>

    <xsl:value-of select="$I"/>
    <xsl:text>return Result;&#10;</xsl:text>
    <xsl:text>end </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.All_Instances;&#10;</xsl:text>

    <!-- Generic filter function to return a Collection of selected
         Instances -->
    <!-- full version ..
         function {dom}.{class}.Selection_Function
           return {dom}.{class}.Collections.Collection is
            use ColdFrame.Instances.Abstract_Containers;
            It : Iterator'Class := Maps.New_Iterator (The_Container);
            Result : Collections.Collection;
         begin
            while not Is_Done (It) loop
               if Pass (Handle (Current_Item (It))) then
                  Collections.Append (Result, Handle (Current_Item (It)));
               end if;
               Next (It);
            end loop;
            return Result;
         end {dom}.{class}.Selection_Function;
         -->
    <xsl:call-template name="do-not-edit"/>
    <xsl:call-template name="identification-info"/>

    <xsl:text>function </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Selection_Function&#10;</xsl:text>
    <xsl:value-of select="$C"/>
    <xsl:text>return </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Collections.Collection is&#10;</xsl:text>

    <xsl:if  test="$max &gt; 1">
      <xsl:value-of select="$I"/>
      <xsl:text>use ColdFrame.Instances.Abstract_Containers;&#10;</xsl:text>
      <xsl:value-of select="$I"/>
      <xsl:text>It : Iterator'Class := Maps.New_Iterator (The_Container);&#10;</xsl:text>
    </xsl:if>

    <xsl:value-of select="$I"/>
    <xsl:text>Result : Collections.Collection;&#10;</xsl:text>
    <xsl:text>begin&#10;</xsl:text>

    <xsl:choose>
      
      <xsl:when test="$max &gt; 1">
        
        <xsl:value-of select="$I"/>
        <xsl:text>while not Is_Done (It) loop&#10;</xsl:text>
        <xsl:value-of select="$II"/>
        <xsl:text>if Pass (Handle (Current_Item (It))) then&#10;</xsl:text>
        <xsl:value-of select="$III"/>
        <xsl:text>Collections.Append (Result, Handle (Current_Item (It)));&#10;</xsl:text>
        <xsl:value-of select="$II"/>
        <xsl:text>end if;&#10;</xsl:text>
        <xsl:value-of select="$II"/>
        <xsl:text>Next (It);&#10;</xsl:text>
        <xsl:value-of select="$I"/>
        <xsl:text>end loop;&#10;</xsl:text>
        
      </xsl:when>
      
      <xsl:otherwise>
        
        <xsl:value-of select="$I"/>
        <xsl:text>if This /= null and then Pass (This) then&#10;</xsl:text>
        <xsl:value-of select="$II"/>
        <xsl:text>Collections.Append (Result, This);&#10;</xsl:text>
        <xsl:value-of select="$I"/>
        <xsl:text>end if;&#10;</xsl:text>
        
      </xsl:otherwise>
      
    </xsl:choose>

    <xsl:value-of select="$I"/>
    <xsl:text>return Result;&#10;</xsl:text>
    <xsl:text>end </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Selection_Function;&#10;</xsl:text>

    <!-- Generic filter function for collections of Instances -->
    <!-- 
         with {dom}.{class}.Abstract_Containers;
         function {dom}.{class}.Filter_Function
           (The_Collection : {dom}.{class}.Collections.Collection)
           return {dom}.{class}.Collections.Collection is
            use Abstract_Containers;
            It : Iterator'Class := Collections.New_Iterator (The_Collection);
            Result : Collections.Collection;
         begin
            while not Is_Done (It) loop
               if Pass (Current_Item (It)) then
                  Collections.Append (Result, Current_Item (It));
               end if;
               Next (It);
            end loop;
            return Result;
         end {dom}.{class}.Filter_Function;
         -->
    <xsl:call-template name="do-not-edit"/>
    <xsl:call-template name="identification-info"/>

    <xsl:text>with </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Abstract_Containers;&#10;</xsl:text>

    <xsl:text>function </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Filter_Function&#10;</xsl:text>
    <xsl:value-of select="$C"/>
    <xsl:text>(The_Collection : </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Collections.Collection)&#10;</xsl:text>
    <xsl:value-of select="$C"/>
    <xsl:text>return </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Collections.Collection is&#10;</xsl:text>

    <xsl:value-of select="$I"/>
    <xsl:text>use Abstract_Containers;&#10;</xsl:text>
    <xsl:value-of select="$I"/>
    <xsl:text>It : Iterator'Class := Collections.New_Iterator (The_Collection);&#10;</xsl:text>
    
    <xsl:value-of select="$I"/>
    <xsl:text>Result : Collections.Collection;&#10;</xsl:text>
    <xsl:text>begin&#10;</xsl:text>
    
    <xsl:value-of select="$I"/>
    <xsl:text>while not Is_Done (It) loop&#10;</xsl:text>
    <xsl:value-of select="$II"/>
    <xsl:text>if Pass (Current_Item (It)) then&#10;</xsl:text>
    <xsl:value-of select="$III"/>
    <xsl:text>Collections.Append (Result, Current_Item (It));&#10;</xsl:text>
    <xsl:value-of select="$II"/>
    <xsl:text>end if;&#10;</xsl:text>
    <xsl:value-of select="$II"/>
    <xsl:text>Next (It);&#10;</xsl:text>
    <xsl:value-of select="$I"/>
    <xsl:text>end loop;&#10;</xsl:text>
    
    <xsl:value-of select="$I"/>
    <xsl:text>return Result;&#10;</xsl:text>
    <xsl:text>end </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Filter_Function;&#10;</xsl:text>

    <!-- Iteration support -->
    <!--
         with {domain}.{class}.Abstract_Containers;
         procedure {domain}.{class}.Iterate
           (Over : {domain}.{class}.Collections.Collection) is
            It : Abstract_Containers.Iterator'Class
              := Collections.New_Iterator (Over);
         begin
            while not Abstract_Containers.Is_Done (It) loop
               Process (Abstract_Containers.Current_Item (It));
               Abstract_Containers.Next (It);
            end loop;
         end {domain}.{class}.Iterate;
         -->
    <xsl:call-template name="do-not-edit"/>
    <xsl:call-template name="identification-info"/>
    <xsl:text>with </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Abstract_Containers;&#10;</xsl:text>
    <xsl:text>procedure </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Iterate&#10;</xsl:text>
    <xsl:value-of select="$C"/>
    <xsl:text>(Over : </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Collections.Collection) is&#10;</xsl:text>
    <xsl:value-of select="$I"/>
    <xsl:text>It : Abstract_Containers.Iterator'Class&#10;</xsl:text>
    <xsl:value-of select="$IC"/>
    <xsl:text>:= Collections.New_Iterator (Over);&#10;</xsl:text>
    <xsl:text>begin&#10;</xsl:text>
    <xsl:value-of select="$I"/>
    <xsl:text>while not Abstract_Containers.Is_Done (It) loop&#10;</xsl:text>
    <xsl:value-of select="$II"/>
    <xsl:text>Process (Abstract_Containers.Current_Item (It));&#10;</xsl:text>
    <xsl:value-of select="$II"/>
    <xsl:text>Abstract_Containers.Next (It);&#10;</xsl:text>
    <xsl:value-of select="$I"/>
    <xsl:text>end loop;&#10;</xsl:text>
    <xsl:text>end </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>.Iterate;&#10;</xsl:text>


  </xsl:template>

  <xsl:template mode="collection-support-body" match="*"/>


</xsl:stylesheet>
