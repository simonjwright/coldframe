<!-- $Id: ada-state.xsl,v 729dd8ca193e 2003/08/30 09:57:13 simon $ -->
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

     As a special exception, when portions of this file are copied by
     a stylesheet processor into an output file, this file does not by
     itself cause the resulting file to be covered by the GNU General
     Public License.  This exception does not however invalidate any
     other reasons why the output file might be covered by the GNU
     Public License.
     -->

<xsl:stylesheet
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  version="1.0">


  <!-- Called at domain/class to generate event types. -->
  <xsl:template name="event-type-specs">

    <!-- class:
         type {name} is new ColdFrame.Project.Events.Event_Base with record
           Payload : {type};
         end record;
         -->

    <!-- non-class:
         type {name} (For_The_Instance : access Instance)
         is new ColdFrame.Project.Events.Instance_Event_Base (For_The_Instance)
         with record
           Payload : {type};
         end record;
         -->

    <xsl:for-each
      select="event">
      <xsl:sort select="name"/>

      <xsl:choose>
        
        <xsl:when test="@class">
          
          <xsl:value-of select="$I"/>
          <xsl:text>type </xsl:text>
          <xsl:value-of select="name"/>
          <xsl:text>&#10;</xsl:text>
          <xsl:value-of select="$I"/>
          <xsl:text>is new ColdFrame.Project.Events.Event_Base with </xsl:text>

        </xsl:when>

        <xsl:otherwise>
          
          <xsl:value-of select="$I"/>
          <xsl:text>type </xsl:text>
          <xsl:value-of select="name"/>
          <xsl:text> (For_The_Instance : access Instance)&#10;</xsl:text>
          <xsl:value-of select="$I"/>
          <xsl:text>is new ColdFrame.Project.Events.Instance_Event_Base (For_The_Instance)&#10;</xsl:text>
          <xsl:value-of select="$I"/>
          <xsl:text>with </xsl:text>

        </xsl:otherwise>

      </xsl:choose>
      
      <xsl:choose>
        
        <xsl:when test="type">
          <xsl:text>record&#10;</xsl:text>
          <xsl:value-of select="$II"/>
          <xsl:text>Payload : </xsl:text>
          <xsl:value-of select="type"/>
          <xsl:text>;&#10;</xsl:text>
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
  <xsl:template match="event" mode="event-handler-specs">

    <!--
         procedure Handler (Ev : {event});
         -->
    <xsl:value-of select="$I"/>
    <xsl:text>procedure Handler (Ev : </xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>);&#10;</xsl:text>
    <xsl:value-of select="$blank-line"/>

  </xsl:template>

  <xsl:template match="*" mode="event-handler-specs"/>


  <!-- Generate event handler bodies for <<event>>s. -->
  <xsl:template match="statemachine/event" mode="event-handler-bodies">

    <!-- non-singleton
         procedure Handler (Ev : {event}) is
            This : constant Handle := Ev.For_The_Instance.all'Unchecked_Access;
         begin
            case That.State_Machine_State is
               when {source-state} =>
                  Enter_{target-state} (This, Ev);
               when {source-state} =>
                  null;
               when {source-state} =>
                  raise ColdFrame.Exceptions.Cant_Happen;
            end case;
         end Handler;
         -->

    <!-- singleton
         procedure Handler (Ev : {event}) is
            This : constant Handle := Ev.For_The_Instance.all'Unchecked_Access;
         begin
            case This.State_Machine_State is
               when {source-state} =>
                  Enter_{target-state} (Ev);
               when {source-state} =>
                  null;
               when {source-state} =>
                  raise ColdFrame.Exceptions.Cant_Happen;
            end case;
         end Handler;
         -->

    <xsl:variable name="e" select="name"/>

    <xsl:value-of select="$I"/>
    <xsl:text>procedure Handler (Ev : </xsl:text>
    <xsl:value-of select="$e"/>
    <xsl:text>) is&#10;</xsl:text>

    <xsl:value-of select="$II"/>
    <xsl:text>This : constant Handle := Ev.For_The_Instance.all'Unchecked_Access;&#10;</xsl:text>
    <xsl:value-of select="$I"/>
    <xsl:text>begin&#10;</xsl:text>

    <xsl:value-of select="$II"/>
    <xsl:text>case This.State_Machine_State is&#10;</xsl:text>

    <xsl:for-each select="../state">
      <xsl:sort select="name"/>

      <xsl:variable name="s" select="name"/>

      <xsl:value-of select="$III"/>
      <xsl:text>when </xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text> =&gt;&#10;</xsl:text>

      <xsl:choose>

        <xsl:when test="../transition[event=$e and source=$s]/@ignore">
          <xsl:value-of select="$IIII"/>
          <xsl:text>null;&#10;</xsl:text>          
        </xsl:when>
        
        <xsl:when test="../transition[event=$e and source=$s]">

          <xsl:if test="../transition[event=$e and source=$s]/action">
            <xsl:value-of select="$IIII"/>
            <xsl:text>--  TRANSITION ACTION HERE!&#10;</xsl:text>
          </xsl:if>

          <xsl:value-of select="$IIII"/>
          <xsl:text>Enter_</xsl:text>
          <xsl:value-of select="../transition[event=$e and source=$s]/target"/>
          <xsl:choose>
            <xsl:when test="../../@singleton">
              <xsl:text> (Ev);&#10;</xsl:text>              
            </xsl:when>
            <xsl:otherwise>
              <xsl:text> (This, Ev);&#10;</xsl:text>              
            </xsl:otherwise>
          </xsl:choose>

        </xsl:when>
        
        <xsl:otherwise>
          <xsl:value-of select="$IIII"/>
          <xsl:text>raise ColdFrame.Exceptions.Cant_Happen;&#10;</xsl:text>
        </xsl:otherwise>
        
      </xsl:choose>
          
    </xsl:for-each>

    <xsl:value-of select="$II"/>
    <xsl:text>end case;&#10;</xsl:text>
    <xsl:value-of select="$I"/>
    <xsl:text>end Handler;&#10;</xsl:text>

    <xsl:value-of select="$blank-line"/>

  </xsl:template>


  <!-- Generate event handler bodies for <<class events>>s. -->
  <xsl:template match="event[@class]" mode="event-handler-bodies">

    <!-- 
         procedure Handler (Ev : {event}) is
         begin
            {receiver} (Ev);
         end Handler;
         -->

    <xsl:variable name="e" select="name"/>
    <xsl:variable
      name="op"
      select="../operation[@handler
              and count(parameter)=1
              and parameter/type=$e]"/>

    <xsl:if test="not($op)">
      <xsl:message terminate="yes">
        <xsl:text>Error: no handler for </xsl:text>
        <xsl:value-of select="../name"/>.<xsl:value-of select="$e"/>
      </xsl:message>
    </xsl:if>
    <xsl:if test="count($op) &gt; 1">
      <xsl:message terminate="yes">
        <xsl:text>Error: more than one handler for </xsl:text>
        <xsl:value-of select="../name"/>.<xsl:value-of select="$e"/>
      </xsl:message>
    </xsl:if>

    <xsl:value-of select="$I"/>
    <xsl:text>procedure Handler (Ev : </xsl:text>
    <xsl:value-of select="$e"/>
    <xsl:text>) is&#10;</xsl:text>
    <xsl:value-of select="$I"/>
    <xsl:text>begin&#10;</xsl:text>

    <xsl:value-of select="$II"/>
    <xsl:value-of select="$op/name"/>
    <xsl:text> (Ev);&#10;</xsl:text>              

    <xsl:value-of select="$I"/>
    <xsl:text>end Handler;&#10;</xsl:text>

    <xsl:value-of select="$blank-line"/>

  </xsl:template>

  <xsl:template match="*" mode="event-handler-bodies"/>


  <!-- Called at domain/class to generate any class body "with"s. -->
  <xsl:template name="state-body-context">

    <!-- The initial state automatically enters the next state if there's an
         unguarded transtion. -->
    
    <xsl:variable name="init" select="statemachine/state[@initial]/name"/>

    <xsl:if test="statemachine/transition[source=$init and not (event)]">
      <xsl:text>with ColdFrame.Project.Events.Creation;&#10;</xsl:text>
    </xsl:if>
    
  </xsl:template>


  <!-- Generate state entry procedure specs. -->
  <xsl:template match="statemachine/state" mode="state-entry-specs">

    <!-- non-singleton
         procedure Enter_{state}
           (This : Handle;
            Ev : ColdFrame.Project.Events.Instance_Event_Base'Class);
         pragma Warnings (Off, Enter_{initial-state});
         -->

    <!-- singleton
         procedure Enter_{state}
           (Ev : ColdFrame.Project.Events.Instance_Event_Base'Class);
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
         <xsl:text>(Ev : ColdFrame.Project.Events.Instance_Event_Base'Class)</xsl:text>
       </xsl:when>
       <xsl:otherwise>
         <xsl:text>(This : Handle;&#10;</xsl:text>
         <xsl:value-of select="$IC"/>
         <xsl:text> Ev : ColdFrame.Project.Events.Instance_Event_Base'Class)</xsl:text>
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
           (This : Handle;
            Ev : ColdFrame.Project.Events.Instance_Event_Base'Class) is
           pragma Warnings (Off, Ev);
         begin
            This.State_Machine_State := {state};
            {entry-action} (This, {event} (Ev).Payload);   -  if has parameter
            {entry-action} (This);                         -  if no parameter
            This.Old_State_Machine_State := {state};
            Enter_{next-state} (This, Ev);                 -  if unguarded exit
         end Enter_{state};
         -->

    <!-- singleton
         procedure Enter_{state}
           (Ev : ColdFrame.Project.Events.Instance_Event_Base'Class) is
           pragma Warnings (Off, Ev);
         begin
            This.State_Machine_State := {state};
            {entry-action} ({event} (Ev).Payload);         -  if has parameter
            {entry-action};                                -  if no parameter
            This.Old_State_Machine_State := {state};
            Enter_{next-state} (Ev);                       -  if unguarded exit
         end Enter_{state};
         -->

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
         <xsl:text>(Ev : ColdFrame.Project.Events.Instance_Event_Base'Class)</xsl:text>
       </xsl:when>
       <xsl:otherwise>
         <xsl:text>(This : Handle;&#10;</xsl:text>
         <xsl:value-of select="$IC"/>
         <xsl:text> Ev : ColdFrame.Project.Events.Instance_Event_Base'Class)</xsl:text>
       </xsl:otherwise>
     </xsl:choose>
     <xsl:text> is&#10;</xsl:text>

     <xsl:value-of select="$II"/>
     <xsl:text>pragma Warnings (Off, Ev);&#10;</xsl:text>

     <xsl:value-of select="$I"/>
     <xsl:text>begin&#10;</xsl:text>

     <xsl:value-of select="$II"/>
     <xsl:text>This.State_Machine_State := </xsl:text>
     <xsl:value-of select="$s"/>
     <xsl:text>;&#10;</xsl:text>       

     <xsl:for-each select="action">

       <xsl:variable name="n" select="."/>
       <xsl:variable
         name="params"
         select="../../../operation[name=$n]/parameter"/>

       <xsl:choose>

         <xsl:when test="../../../operation[name=$n]/@return">
           <xsl:message terminate="yes">
             <xsl:text>Error: </xsl:text>
             <xsl:value-of select="../../../name"/>
             <xsl:text>.</xsl:text>
             <xsl:value-of select="$n"/>
             <xsl:text> is a function, can't be an entry action.</xsl:text>
           </xsl:message>           
         </xsl:when>

         <xsl:when test="$n='Delete' and $singleton">
           <xsl:message terminate="yes">
             <xsl:text>Error: </xsl:text>
             <xsl:value-of select="../../../name"/>
             <xsl:text>.Delete not allowed as a singleton entry action.</xsl:text>
           </xsl:message>           
         </xsl:when>
         
         <xsl:when test="count($params)&gt;1">
           <xsl:message terminate="yes">
             <xsl:text>Error: </xsl:text>
             <xsl:value-of select="../../../name"/>
             <xsl:text>.</xsl:text>
             <xsl:value-of select="$n"/>
             <xsl:text> has too many parameters to be an entry action.</xsl:text>
           </xsl:message>
         </xsl:when>

         <xsl:when test="count($params)=1">
           <!-- The full spec of the event is in the class, not the
                state machine. -->
           <xsl:if test="not(../../../event[name=$e]/type=$params/type)">
             <xsl:message terminate="yes">
               <xsl:value-of select="../../../name"/>
               <xsl:text>.</xsl:text>
               <xsl:value-of select="$n"/>
               <xsl:text>'s parameter is of the wrong type.</xsl:text>
             </xsl:message>
           </xsl:if>
         </xsl:when>

         <!-- XXX what if there is an operation with a parameter for a state
              with more than one event type leading to it?
              Find the number of events leading to this state with type /=
              the operations's parameter type? -->

       </xsl:choose>

       <xsl:choose>

         <xsl:when test="$singleton">

           <xsl:choose>
             
             <xsl:when test="$params">

               <xsl:value-of select="$II"/>
               <xsl:value-of select="$n"/>
               <xsl:text> (</xsl:text>
               <xsl:value-of select="$e"/>
               <xsl:text> (Ev).Payload);&#10;</xsl:text>
               
             </xsl:when>

             <xsl:otherwise>
               <xsl:value-of select="$II"/>
               <xsl:value-of select="$n"/>
               <xsl:text>;&#10;</xsl:text>               
             </xsl:otherwise>

           </xsl:choose>
           
         </xsl:when>

         <xsl:otherwise>
           
           <xsl:choose>

             <xsl:when test="$n='Delete' and not($params)">

               <!--
                    declare
                       H : Handle := This;
                    begin
                       Delete (H);
                       ColdFrame.Project.Events.Instance_Is_Deleted
                         (Ev'Unrestricted_Access);
                    end;
                    -->
               <xsl:value-of select="$II"/>
               <xsl:text>declare&#10;</xsl:text>
               <xsl:value-of select="$III"/>
               <xsl:text>H : Handle := This;&#10;</xsl:text>
               <xsl:value-of select="$II"/>
               <xsl:text>begin&#10;</xsl:text>
               <xsl:value-of select="$III"/>
               <xsl:text>Delete (H);&#10;</xsl:text>
               <xsl:value-of select="$III"/>
               <xsl:text>ColdFrame.Project.Events.Instance_Is_Deleted&#10;</xsl:text>
               <xsl:value-of select="$IIIC"/>
               <xsl:text>(Ev'Unrestricted_Access);&#10;</xsl:text>
               <xsl:value-of select="$II"/>
               <xsl:text>end;&#10;</xsl:text>

             </xsl:when>
             
             <xsl:when test="$params">
               <xsl:value-of select="$II"/>
               <xsl:value-of select="$n"/>
               <xsl:text> (This, </xsl:text>
               <xsl:value-of select="$e"/>
               <xsl:text> (Ev).Payload);&#10;</xsl:text>
             </xsl:when>

             <xsl:otherwise>
               <xsl:value-of select="$II"/>
               <xsl:value-of select="$n"/>
               <xsl:text> (This);&#10;</xsl:text>
             </xsl:otherwise>

           </xsl:choose>

         </xsl:otherwise>

       </xsl:choose>

     </xsl:for-each>

     <xsl:if test="not(action/name='Delete')">
       <!-- Set the old state variable. -->
       <xsl:value-of select="$II"/>
       <xsl:text>This.Old_State_Machine_State := </xsl:text>
       <xsl:value-of select="$s"/>
       <xsl:text>;&#10;</xsl:text>       
     </xsl:if>
     
     <xsl:if test="../transition[source=$s and not(event)]">
       <xsl:value-of select="$II"/>
       <xsl:text>Enter_</xsl:text>
       <xsl:value-of select="../transition[source=$s and not(event)]/target"/>
       <xsl:choose>
         <xsl:when test="$singleton">
           <xsl:text> (Ev);&#10;</xsl:text>
         </xsl:when>
         <xsl:otherwise>
           <xsl:text> (This, Ev);&#10;</xsl:text>
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
            Creation : ColdFrame.Project.Events.Creation.Event (Result);
         begin
            Enter_{next-state} (Result, Creation);
         end;
         -->

    <!-- singleton: if there is an unguarded transition from the initial state
         declare
            Creation : ColdFrame.Project.Events.Creation.Event (Result);
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
      <xsl:text>Creation : ColdFrame.Project.Events.Creation.Event (Result);&#10;</xsl:text>
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


  <!-- Called from domain to generate the domain's event manager spec. -->
  <xsl:template name="event-manager-spec">
    
    <!--
         with ColdFrame.Project.Events;
         package {domain}.Events is
           Dispatcher : ColdFrame.Project.Events.Event_Queue_P;
           procedure Initialize;
         end {domain}.Events;
         -->

    <xsl:call-template name="do-not-edit"/>

    <xsl:text>with ColdFrame.Project.Events;&#10;</xsl:text>
    <xsl:text>package </xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>.Events is&#10;</xsl:text>
    <xsl:value-of select="$I"/>
    <xsl:text>Dispatcher : ColdFrame.Project.Events.Event_Queue_P;&#10;</xsl:text>
    <xsl:value-of select="$I"/>
    <xsl:text>procedure Initialize;&#10;</xsl:text>
    <xsl:text>end </xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>.Events;&#10;</xsl:text>

  </xsl:template>


  <!-- Called from domain to generate the domain's event manager body. -->
  <xsl:template name="event-manager-body">
    
    <!--
         package body {domain}.Events is
           procedure Initialize is separate;
         end {domain}.Events;
         separate ({domain}.Events)
         procedure Initialize is
         begin
           null;
         end Initialize;
         -->

    <xsl:call-template name="do-not-edit"/>

    <xsl:text>package body </xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>.Events is&#10;</xsl:text>
    <xsl:value-of select="$I"/>
    <xsl:text>procedure Initialize is separate;&#10;</xsl:text>
    <xsl:text>end </xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>.Events;&#10;</xsl:text>

    <xsl:call-template name="should-edit"/>

    <xsl:text>separate (</xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>.Events)&#10;</xsl:text>
    <xsl:text>procedure Initialize is&#10;</xsl:text>
    <xsl:text>begin&#10;</xsl:text>
    <xsl:value-of select="$I"/>
    <xsl:text>null;&#10;</xsl:text>
    <xsl:text>end Initialize;&#10;</xsl:text>

  </xsl:template>


</xsl:stylesheet>
