<!-- $Id: ada-state.xsl,v a708c87c1e88 2002/02/20 20:25:04 simon $ -->
<!-- XSL stylesheet to generate Ada state machine code. -->
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

     As a special exception, when portions of this file are copied by a
     stylesheet processor into an output file, you may use that output
     file without restriction.
     -->

<xsl:stylesheet
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  version="1.0">


  <!-- Called at domain/class to generate event types. -->
  <xsl:template name="event-type-specs">

    <!--
         type {name} (For_The_Instance : access Instance)
         is new ColdFrame.Events.Event_Base (For_The_Instance) with record
           {argument-name} : {argument-type};
         end record;
         -->

    <xsl:for-each
      select="statemachine/event">
      <xsl:sort select="name"/>

      <xsl:value-of select="$I"/>
      <xsl:text>type </xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text> (For_The_Instance : access Instance)&#10;</xsl:text>
      <xsl:value-of select="$I"/>
      <xsl:text>is new ColdFrame.Events.Event_Base (For_The_Instance) with </xsl:text>
      
      <xsl:choose>
        
        <xsl:when test="argument">
          <xsl:text>record&#10;</xsl:text>
          <xsl:for-each select="argument">
            <xsl:value-of select="$II"/>
            <xsl:value-of select="name"/>
            <xsl:text> : </xsl:text>
            <xsl:value-of select="type"/>
            <xsl:text>;&#10;</xsl:text>
          </xsl:for-each>
          <xsl:value-of select="$I"/>
          <xsl:text>end record;&#10;</xsl:text>
        </xsl:when>
        
        <xsl:otherwise>
          <xsl:text>null record;&#10;</xsl:text>
        </xsl:otherwise>
        
      </xsl:choose>
      
      <xsl:value-of select="$blank-line"/>

    </xsl:for-each>

  </xsl:template>


  <!-- Called at domain/class to generate the enumerated type for the state
       machine. -->
  <xsl:template name= "state-machine-states">
    
    <xsl:value-of select="$I"/>
    <xsl:text>type State_Machine_State_T is&#10;</xsl:text>
    <xsl:value-of select="$IC"/>
    <xsl:text>(</xsl:text>
    <xsl:for-each select="statemachine/state/name">
      <!-- initial state first -->
      <!-- XXX final state last? -->
      <xsl:sort select="concat(not (../@initial),.)"/>
      <xsl:value-of select="."/>
      <xsl:if test="position() &lt; last()">
        <xsl:text>,&#10; </xsl:text>
        <xsl:value-of select="$IC"/>
      </xsl:if>
    </xsl:for-each>
    <xsl:text>);&#10;</xsl:text>
    <xsl:value-of select="$blank-line"/>

  </xsl:template>


  <!-- Called at domain/class to generate the State_Image operation spec. -->
  <xsl:template name="state-image-spec">
    
    <xsl:value-of select="$I"/>
    <xsl:text>function State_Image (This : Instance) return String;&#10;</xsl:text>
    <xsl:value-of select="$blank-line"/>

  </xsl:template>


  <!-- Called at domain/class to generate the State_Image operation body. -->
  <xsl:template name="state-image-body">
    
    <xsl:value-of select="$I"/>
    <xsl:text>function State_Image (This : Instance) return String is&#10;</xsl:text>
    <xsl:value-of select="$I"/>
    <xsl:text>begin&#10;</xsl:text>
    <xsl:value-of select="$II"/>
    <xsl:text>return This.State_Machine_State'Img;&#10;</xsl:text>
    <xsl:value-of select="$I"/>
    <xsl:text>end State_Image;&#10;</xsl:text>
    <xsl:value-of select="$blank-line"/>

  </xsl:template>


  <!-- Generate event handler specs. -->
  <xsl:template match="statemachine/event" mode="event-handler-specs">

    <!--
         procedure Process (This : {event});
         -->
    <xsl:value-of select="$I"/>
    <xsl:text>procedure Handler (This : </xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>);&#10;</xsl:text>
    <xsl:value-of select="$blank-line"/>

  </xsl:template>

  <xsl:template match="*" mode="event-handler-specs"/>


  <!-- Generate event handler bodies. -->
  <xsl:template match="statemachine/event" mode="event-handler-bodies">

    <!-- non-singleton
         procedure Handler (This : {event}) is
            That : Handle := This.For_The_Instance.all'Unchecked_Access;
         begin
            case That.State_Machine_State is
               when {source-state} =>
                  Enter_{target-state} (That, This);
               when {source-state} =>
                  raise ColdFrame.Events.Cant_Happen;
            end case;
         end Handler;
         -->

    <!-- singleton
         procedure Handler (This : {event}) is
            That : Handle := This.For_The_Instance.all'Unchecked_Access;
         begin
            case That.State_Machine_State is
               when {source-state} =>
                  Enter_{target-state} (This);
               when {source-state} =>
                  raise ColdFrame.Events.Cant_Happen;
            end case;
         end Handler;
         -->

    <xsl:variable name="e" select="name"/>

    <xsl:value-of select="$I"/>
    <xsl:text>procedure Handler (This : </xsl:text>
    <xsl:value-of select="$e"/>
    <xsl:text>) is&#10;</xsl:text>

    <xsl:value-of select="$II"/>
    <xsl:text>That : Handle := This.For_The_Instance.all'Unchecked_Access;&#10;</xsl:text>
    <xsl:value-of select="$I"/>
    <xsl:text>begin&#10;</xsl:text>

    <xsl:value-of select="$II"/>
    <xsl:text>case That.State_Machine_State is&#10;</xsl:text>

    <xsl:for-each select="../state">
      <xsl:sort select="name"/>

      <xsl:variable name="s" select="name"/>

      <xsl:value-of select="$III"/>
      <xsl:text>when </xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text> =&gt;&#10;</xsl:text>

      <xsl:choose>
        
        <xsl:when test="../transition[event=$e and source=$s]">
          <xsl:value-of select="$IIII"/>
          <xsl:text>Enter_</xsl:text>
          <xsl:value-of select="../transition[event=$e and source=$s]/target"/>
          <xsl:choose>
            <xsl:when test="../../@singleton">
              <xsl:text> (This);&#10;</xsl:text>              
            </xsl:when>
            <xsl:otherwise>
              <xsl:text> (That, This);&#10;</xsl:text>              
            </xsl:otherwise>
          </xsl:choose>
        </xsl:when>
        
        <xsl:otherwise>
          <xsl:value-of select="$IIII"/>
          <xsl:text>raise ColdFrame.Events.Cant_Happen;&#10;</xsl:text>
        </xsl:otherwise>
        
      </xsl:choose>
          
    </xsl:for-each>

    <xsl:value-of select="$II"/>
    <xsl:text>end case;&#10;</xsl:text>
    <xsl:value-of select="$I"/>
    <xsl:text>end Handler;&#10;</xsl:text>

    <xsl:value-of select="$blank-line"/>

  </xsl:template>

  <xsl:template match="*" mode="event-handler-specs"/>


  <!-- Called at domain/class to generate any class body "with"s. -->
  <xsl:template name="state-body-context">

    <!-- The initial state automatically enters the next state if there's an
         unguarded transtion.
         No support here for singletons. -->
    
    <xsl:variable name="init" select="statemachine/state[@initial]/name"/>

    <xsl:if test="not(@singleton)
      and statemachine/transition[source=$init and not (event)]">
      <xsl:text>with ColdFrame.Events.Creation;&#10;</xsl:text>
    </xsl:if>
    
  </xsl:template>


  <!-- Generate state entry procedure specs. -->
  <xsl:template match="statemachine/state" mode="state-entry-specs">

    <!-- non-singleton
         procedure Enter_{state}
           (This : Handle; What : ColdFrame.Events.Event_Base'Class);
         -->

    <!-- singleton
         procedure Enter_{state}
           (What : ColdFrame.Events.Event_Base'Class);
         -->

     <xsl:variable name="s" select="name"/>
     <!-- <xsl:variable name="e" select="../transition[target=$s]/event"/> -->
     <xsl:variable name="singleton" select="../../@singleton"/>

     <xsl:value-of select="$I"/>
     <xsl:text>procedure Enter_</xsl:text>
     <xsl:value-of select="$s"/>
     <xsl:text>&#10;</xsl:text>
     <xsl:value-of select="$IC"/>
     <xsl:choose>
       <xsl:when test="$singleton">
         <xsl:text>(What : ColdFrame.Events.Event_Base'Class)</xsl:text>
       </xsl:when>
       <xsl:otherwise>
         <xsl:text>(This : Handle; What : ColdFrame.Events.Event_Base'Class)</xsl:text>
       </xsl:otherwise>
     </xsl:choose>
     <xsl:text>;&#10;</xsl:text>

     <xsl:value-of select="$blank-line"/>

  </xsl:template>

  <xsl:template match="*" mode="state-entry-specs"/>


  <!-- Generate state entry procedure bodies. -->
  <xsl:template match="statemachine/state" mode="state-entry-bodies">

    <!-- non-singleton
         procedure Enter_{state}
           (This : Handle; What : ColdFrame.Events.Event_Base'Class) is
         begin
            {entry-action} (This, {event} (What));  -  if has parameter
            {entry-action} (This);                  -  if has no parameter
            This.State_Machine_State := {state};
            Enter_{next-state} (This, What);       -  if unguarded exit
         end Enter_{state};
         -->

    <!-- singleton
         procedure Enter_{state}
           (What : ColdFrame.Events.Event_Base'Class) is
         begin
            {entry-action} ({event} (What));        -  if has parameter
            {entry-action};                         -  if has no parameter
            This.State_Machine_State := {state};
            Enter_{next-state} (Wnat);             -  if unguarded exit
         end Enter_{state};
         -->

     <!-- XXX which event? are they all the same? (should be, of course) -->

     <xsl:variable name="s" select="name"/>
     <xsl:variable name="e" select="../transition[target=$s]/event"/>
     <xsl:variable name="singleton" select="../../@singleton"/>

     <xsl:value-of select="$I"/>
     <xsl:text>procedure Enter_</xsl:text>
     <xsl:value-of select="$s"/>
     <xsl:text>&#10;</xsl:text>
     <xsl:value-of select="$IC"/>
     <xsl:choose>
       <xsl:when test="$singleton">
         <xsl:text>(What : ColdFrame.Events.Event_Base'Class)</xsl:text>
       </xsl:when>
       <xsl:otherwise>
         <xsl:text>(This : Handle; What : ColdFrame.Events.Event_Base'Class)</xsl:text>
       </xsl:otherwise>
     </xsl:choose>
     <xsl:text> is&#10;</xsl:text>

     <xsl:value-of select="$I"/>
     <xsl:text>begin&#10;</xsl:text>

     <xsl:for-each select="action">

       <xsl:variable name="n" select="."/>
       <xsl:variable name="params" select="../../operation[name=$n]/parameter"/>

       <xsl:value-of select="$II"/>
       <xsl:value-of select="$n"/>

       <xsl:choose>

         <xsl:when test="$singleton">

           <xsl:choose>
             
             <xsl:when test="$params">

               <xsl:text> (</xsl:text>
               <xsl:value-of select="$e"/>
               <xsl:text> (What))</xsl:text>
               
             </xsl:when>

           </xsl:choose>
           
         </xsl:when>

         <xsl:otherwise>
           
           <xsl:choose>
             
             <xsl:when test="$params">
               <xsl:text> (This, </xsl:text>
               <xsl:value-of select="$e"/>
               <xsl:text> (What))</xsl:text>
             </xsl:when>

             <xsl:otherwise>
               <xsl:text> (This)</xsl:text>
             </xsl:otherwise>

           </xsl:choose>

         </xsl:otherwise>

       </xsl:choose>

       <xsl:text>;&#10;</xsl:text>

     </xsl:for-each>

     <xsl:value-of select="$II"/>
     <xsl:text>This.State_Machine_State := </xsl:text>
     <xsl:value-of select="$s"/>
     <xsl:text>;&#10;</xsl:text>

     <xsl:if test="../transition[source=$s and not(event)]">
       <xsl:value-of select="$II"/>
       <xsl:text>Enter_</xsl:text>
       <xsl:value-of select="../transition[source=$s and not(event)]/target"/>
       <xsl:choose>
         <xsl:when test="$singleton">
           <xsl:text> (What);&#10;</xsl:text>
         </xsl:when>
         <xsl:otherwise>
           <xsl:text> (This, What);&#10;</xsl:text>
         </xsl:otherwise>
       </xsl:choose>
     </xsl:if>

     <xsl:value-of select="$I"/>
     <xsl:text>end Enter_</xsl:text>
     <xsl:value-of select="$s"/>
     <xsl:text>;&#10;</xsl:text>

     <xsl:value-of select="$blank-line"/>
    
  </xsl:template>

  <xsl:template match="*" mode="state-entry-bodies"/>


  <!-- Called from domain/class, within the Create function, to
       apply an unguarded transition (if any) from the initial state. -->
  <xsl:template name="initialize-state-machine">
    
    <!-- standard: if there is an unguarded transition from the initial state
         declare
            Creation : aliased ColdFrame.Events.Creation.Event (Result);
         begin
            Enter_{next-state} (Result, Creation);
         end;
         -->

    <!-- XXX I know the next bit is stupid! still, the logic may be needed
         later. -->
    <!-- singleton: if there is an unguarded transition from the initial state
         declare
            Creation : aliased ColdFrame.Events.Creation.Event (Result);
         begin
            Enter_{next-state} (Creation);
         end;
         -->

    <xsl:variable name="init" select="statemachine/state[@initial]/name"/>
    <xsl:variable
      name="next"
      select="statemachine/transition[source=$init and not (event)]/target"/>

    <xsl:if test="$next">

      <xsl:value-of select="$II"/>
      <xsl:text>declare&#10;</xsl:text>
      <xsl:value-of select="$III"/>
      <xsl:text>Creation : aliased ColdFrame.Events.Creation.Event (Result);&#10;</xsl:text>
      <xsl:value-of select="$II"/>
      <xsl:text>begin&#10;</xsl:text>

      <xsl:value-of select="$III"/>
      <xsl:text>Enter_</xsl:text>

      <xsl:value-of select="$next"/>

      <xsl:choose>
        <xsl:when test="@singleton">
          <xsl:text> (Creation);&#10;</xsl:text>
        </xsl:when>
        <xsl:otherwise>
          <xsl:text> (Result, Creation);&#10;</xsl:text>
        </xsl:otherwise>
      </xsl:choose>

      <xsl:value-of select="$II"/>
      <xsl:text>end;&#10;</xsl:text>

    </xsl:if>

  </xsl:template>


</xsl:stylesheet>
