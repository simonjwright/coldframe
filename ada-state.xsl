<!-- $Id: ada-state.xsl,v 7c9483a18bda 2004/04/23 14:24:54 simon $ -->
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
  version="1.1">


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

      <!-- This seems like as good a place as any to check the state machine's
           validity. -->
      <xsl:variable name="leaving" select="../../transition[source=current()]"/>

      <xsl:if test="count($leaving[not(event)]) &gt; 1">
        <xsl:call-template name="log-error"/>
        <xsl:message>
          <xsl:text>Error: more than one drop-through transition from state </xsl:text>
          <xsl:value-of select="../../../name"/>
          <xsl:text>.</xsl:text>
          <xsl:value-of select="."/>
        </xsl:message>
      </xsl:if>

      <xsl:if test="$leaving[event] and $leaving[not(event)]">
        <xsl:call-template name="log-error"/>
        <xsl:message>
          <xsl:text>Error: drop-through and triggered transitions from state </xsl:text>
          <xsl:value-of select="../../../name"/>
          <xsl:text>.</xsl:text>
          <xsl:value-of select="."/>
        </xsl:message>
      </xsl:if>

      <!--
      <xsl:for-each select="$leaving">
        <xsl:sort select="event"/>
        <xsl:if test="event=preceding-sibling::node()/event">
          <xsl:call-template name="log-error"/>
          <xsl:message>
            <xsl:text>Error: more than one transition triggered by </xsl:text>
            <xsl:value-of select="event"/>
            <xsl:text> from state </xsl:text>
            <xsl:value-of select="../../name"/>
            <xsl:text>.</xsl:text>
            <xsl:value-of select="source"/>
          </xsl:message>
        </xsl:if>
      </xsl:for-each>
      -->

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

    <!--
         procedure Handler (Ev : {event}) is
            This : constant Handle := Ev.For_The_Instance.all'Unchecked_Access;
         begin
            case That.State_Machine_State is
               when {source-state (normal transition)} =>
                  {perform-transition}
               when {source-state (ignored transition)} =>
                  null;
               when {source-state} =>
                  Ada.Exceptions.Raise_Exception
                    (ColdFrame.Exceptions.Cant_Happen'Identity,
                     "{domain}.{class}.{event} in {source-state}");
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

          <xsl:call-template name="perform-transition">
            <xsl:with-param
              name="tr"
              select="../transition[event=$e and source=$s]"/>
            <xsl:with-param name="indent" select="$IIII"/>
          </xsl:call-template>

        </xsl:when>

        <xsl:otherwise>
          <xsl:value-of select="$IIII"/>
          <xsl:text>Ada.Exceptions.Raise_Exception&#10;</xsl:text>
          <xsl:value-of select="$IIIIC"/>
          <xsl:text>(ColdFrame.Exceptions.Cant_Happen'Identity,&#10;</xsl:text>
          <xsl:value-of select="$IIIIC"/>
          <xsl:text> "</xsl:text>
          <xsl:value-of select="../../../name"/>
          <xsl:text>.</xsl:text>
          <xsl:value-of select="../../name"/>
          <xsl:text>.</xsl:text>
          <xsl:value-of select="$e"/>
          <xsl:text> in </xsl:text>
          <xsl:value-of select="$s"/>
          <xsl:text>");&#10;</xsl:text>
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
      <xsl:call-template name="log-error"/>
      <xsl:message>
        <xsl:text>Error: no handler for </xsl:text>
        <xsl:value-of select="../name"/>.<xsl:value-of select="$e"/>
      </xsl:message>
    </xsl:if>
    <xsl:if test="count($op) &gt; 1">
      <xsl:call-template name="log-error"/>
      <xsl:message>
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

    <!-- Ada.Exceptions is only needed if there are Cant_Happen exceptions;
         but that would be rather complex to detect. -->
    <xsl:text>with Ada.Exceptions;&#10;</xsl:text>
    <xsl:text>pragma Warnings (Off, Ada.Exceptions);&#10;</xsl:text>

    <!-- The initial state automatically enters the next state if there's an
         untriggered transtion. If so, and if there are actions with parameters
         in that state, we need a Creation event. -->

    <!-- the initial state .. -->
    <xsl:variable name="init" select="statemachine/state[@initial]/name"/>
    <!-- .. the target state .. -->
    <xsl:variable
      name="next"
      select="statemachine/transition[source=$init and not(event)]/target"/>
    <!-- .. the actions of that state (none on the transition, I hope) .. -->
    <xsl:variable
      name="action"
      select="statemachine/state[name=$next]/action"/>

    <xsl:if test="operation[name=$action]/parameter">
      <xsl:text>with ColdFrame.Project.Events.Creation;&#10;</xsl:text>
    </xsl:if>

  </xsl:template>


  <!-- Called at statemachine/state to generate code to perform
       transitions. -->
  <xsl:template name="perform-transition">

    <!-- The (not ignored) transition we have to generate code for. -->
    <xsl:param name="tr"/>

    <!-- Indentation. -->
    <xsl:param name="indent"/>

    <!-- The target state. -->
    <xsl:variable name="target" select="../state[name=$tr/target]"/>

    <xsl:value-of select="$indent"/>
    <xsl:text>This.State_Machine_State := </xsl:text>
    <xsl:value-of select="$tr/target"/>
    <xsl:text>;&#10;</xsl:text>

    <xsl:if test="$tr/action">
      <!-- Check for actions after instance deletion. -->
      <xsl:if test="($tr/action='Delete'
                     or ../../operation[name=$tr/action]/@final)
                    and $target/action">
        <xsl:call-template name="log-error"/>
        <xsl:message>
          <xsl:text>Error: entry action(s) after final action on transition to </xsl:text>
          <xsl:value-of select="../../name"/>
          <xsl:text>.</xsl:text>
          <xsl:value-of select="$tr/target"/>
        </xsl:message>
      </xsl:if>
      <xsl:call-template name="call-action">
        <xsl:with-param name="class" select="../.."/>
        <xsl:with-param name="event" select="$tr/event"/>
        <xsl:with-param name="action" select="$tr/action"/>
        <xsl:with-param name="indent" select="$indent"/>
      </xsl:call-template>
    </xsl:if>

    <xsl:for-each select="$target/action">
      <!-- Check for actions after instance deletion. -->
      <xsl:if test="(.='Delete' or ../../../operation[name=.]/@final)
                    and not(position()=last())">
        <xsl:call-template name="log-error"/>
        <xsl:message>
          <xsl:text>Error: entry action(s) after final action in </xsl:text>
          <xsl:value-of select="../../../name"/>
          <xsl:text>.</xsl:text>
          <xsl:value-of select="../name"/>
        </xsl:message>
      </xsl:if>
      <xsl:call-template name="call-action">
        <xsl:with-param name="class" select="../../.."/>
        <xsl:with-param name="event" select="$tr/event"/>
        <xsl:with-param name="action" select="."/>
        <xsl:with-param name="indent" select="$indent"/>
      </xsl:call-template>
    </xsl:for-each>

    <xsl:variable
      name="deleting"
      select="$tr/action='Delete'
              or $target/@final
              or $target/action='Delete'
              or $target/action/@final"/>

    <xsl:if test="not($deleting)">
      <xsl:value-of select="$indent"/>
      <xsl:text>This.Old_State_Machine_State := </xsl:text>
      <xsl:value-of select="$tr/target"/>
      <xsl:text>;&#10;</xsl:text>
    </xsl:if>

    <!-- Now do drop-throughs (recursively). -->

    <xsl:variable
      name="drop-through"
      select="../transition[source=$tr/target and not(event)]"/>

    <xsl:if test="$drop-through">

      <xsl:if test="$deleting">
        <xsl:call-template name="log-error"/>
        <xsl:message>
          <xsl:text>Error: drop-through transition after final state </xsl:text>
          <xsl:value-of select="../../name"/>
          <xsl:text>.</xsl:text>
          <xsl:value-of select="$tr/target"/>
        </xsl:message>
      </xsl:if>

      <!-- Need to change context to call perform-transition from the
           current target state. -->

      <xsl:for-each select="../state[name=$drop-through/source]">
        <xsl:call-template name="perform-transition">
          <xsl:with-param name="tr" select="$drop-through"/>
          <xsl:with-param name="indent" select="$indent"/>
        </xsl:call-template>
      </xsl:for-each>

    </xsl:if>

  </xsl:template>


  <!-- Called (at statemachine/state or statemachine/state/action) to
       generate an action call. -->
  <xsl:template name="call-action">

    <!-- The class. -->
    <xsl:param name="class"/>

    <!-- The triggering event's name. -->
    <xsl:param name="event"/>

    <!-- The action name. -->
    <xsl:param name="action"/>

    <!-- The indentation. -->
    <xsl:param name="indent"/>

    <xsl:variable name="single" select="$class/@singleton"/>
    <xsl:variable name="impl-class">
      <xsl:call-template name="class-of-operation-for-action">
        <xsl:with-param name="class" select="$class"/>
        <xsl:with-param name="action" select="$action"/>
      </xsl:call-template>
    </xsl:variable>
    <xsl:variable
      name="op"
      select="/domain/class[name=$impl-class]/operation[name=$action]"/>
    <xsl:variable name="params" select="$op/parameter"/>

    <!-- Check for errors. -->
    <xsl:choose>

      <xsl:when test="not($op or $action='Delete')">
        <xsl:call-template name="log-error"/>
        <xsl:message>
          <xsl:text>Error: no operation for </xsl:text>
          <xsl:value-of select="$class/name"/>
          <xsl:text>, state </xsl:text>
          <xsl:value-of select="../name"/>
          <xsl:text>, action </xsl:text>
          <xsl:value-of select="$action"/>
        </xsl:message>
      </xsl:when>

      <xsl:when test="$op/@return">
        <xsl:call-template name="log-error"/>
        <xsl:message>
          <xsl:text>Error: </xsl:text>
          <xsl:value-of select="$class/name"/>
          <xsl:text>.</xsl:text>
          <xsl:value-of select="../name"/>
          <xsl:text>, </xsl:text>
          <xsl:value-of select="$action"/>
          <xsl:text> is a function, can't be an entry action</xsl:text>
        </xsl:message>
      </xsl:when>

      <xsl:when test="($action='Delete' or $op/@final) and $single">
        <xsl:call-template name="log-error"/>
        <xsl:message>
          <xsl:text>Error: </xsl:text>
          <xsl:value-of select="$class/name"/>
          <xsl:text>.</xsl:text>
          <xsl:value-of select="../name"/>
          <xsl:text>, a singleton entry action may not delete the instance</xsl:text>
        </xsl:message>
      </xsl:when>

      <xsl:when test="count($params)&gt;1">
        <xsl:call-template name="log-error"/>
        <xsl:message>
          <xsl:text>Error: </xsl:text>
          <xsl:value-of select="$class/name"/>
          <xsl:text>.</xsl:text>
          <xsl:value-of select="../name"/>
          <xsl:text>, </xsl:text>
          <xsl:value-of select="$action"/>
          <xsl:text> has too many parameters to be an entry action</xsl:text>
        </xsl:message>
      </xsl:when>

      <xsl:when test="count($params)=1">
        <!-- The full spec of the event is in the class, not the
             state machine. -->
        <xsl:if test="not($class/event[name=$event]/type=$params/type)">
          <xsl:call-template name="log-error"/>
          <xsl:message>
            <xsl:text>Error: </xsl:text>
            <xsl:value-of select="$class/name"/>
            <xsl:text>.</xsl:text>
            <xsl:value-of select="$event"/>
            <xsl:text>'s payload is of the wrong type (</xsl:text>
            <xsl:value-of select="$class/event[name=$event]/type"/>
            <xsl:text>: </xsl:text>
            <xsl:value-of select="$action"/>
            <xsl:text> expects </xsl:text>
            <xsl:value-of select="$params/type"/>
            <xsl:text>)</xsl:text>
          </xsl:message>
        </xsl:if>
      </xsl:when>

      <!-- XXX what if there is an operation with a parameter for a state
           with more than one event type leading to it?
           Find the number of events leading to this state with type /=
           the operations's parameter type? -->

    </xsl:choose>

    <!-- Generate code. -->
    <xsl:choose>

      <xsl:when test="$single">

        <xsl:choose>

          <xsl:when test="$params">

            <xsl:value-of select="$indent"/>
            <xsl:value-of select="$action"/>
            <xsl:text> (Ev.Payload);&#10;</xsl:text>

          </xsl:when>

          <xsl:otherwise>
            <xsl:value-of select="$indent"/>
            <xsl:value-of select="$action"/>
            <xsl:text>;&#10;</xsl:text>
          </xsl:otherwise>

        </xsl:choose>

      </xsl:when>

      <xsl:otherwise>

        <xsl:if test="$action='Delete' or $op/@final">
          <!-- Delete() is permitted. -->
          <xsl:value-of select="$indent"/>
          <xsl:text>Mark_Deletable (This);&#10;</xsl:text>
        </xsl:if>

        <xsl:choose>

          <xsl:when test="$action='Delete'">

            <!--
                 declare
                    H : Handle := This;
                 begin
                    Delete (H);
                 end;
                 -->
            <xsl:value-of select="$indent"/>
            <xsl:text>declare&#10;</xsl:text>
            <xsl:value-of select="$indent"/>
            <xsl:value-of select="$I"/>
            <xsl:text>H : Handle := This;&#10;</xsl:text>
            <xsl:value-of select="$indent"/>
            <xsl:text>begin&#10;</xsl:text>
            <xsl:value-of select="$indent"/>
            <xsl:value-of select="$I"/>
            <xsl:text>Delete (H);&#10;</xsl:text>
            <xsl:value-of select="$indent"/>
            <xsl:text>end;&#10;</xsl:text>

          </xsl:when>

          <xsl:when test="$params">
            <xsl:value-of select="$indent"/>
            <xsl:value-of select="$action"/>
            <xsl:text> (This, Ev.Payload);&#10;</xsl:text>
          </xsl:when>

          <xsl:otherwise>
            <xsl:value-of select="$indent"/>
            <xsl:value-of select="$action"/>
            <xsl:text> (This);&#10;</xsl:text>
          </xsl:otherwise>

        </xsl:choose>

        <xsl:if test="$action='Delete' or $op/@final">
          <!--
               ColdFrame.Project.Events.Instance_Is_Deleted
                 (Ev'Unrestricted_Access);
               -->
          <xsl:value-of select="$indent"/>
          <xsl:text>ColdFrame.Project.Events.Instance_Is_Deleted&#10;</xsl:text>
          <xsl:value-of select="$indent"/>
          <xsl:value-of select="$C"/>
          <xsl:text>(Ev'Unrestricted_Access);&#10;</xsl:text>
        </xsl:if>

      </xsl:otherwise>

    </xsl:choose>

  </xsl:template>


  <!-- Called to find the class of the actual operation for an action
       (may be in a parent class). -->
  <xsl:template name="class-of-operation-for-action">

    <!-- The class(es) to be considered. -->
    <xsl:param name="class"/>

    <!-- The name of the action. -->
    <xsl:param name="action"/>

    <xsl:choose>

      <xsl:when test="$class[operation[name=$action]]">
        <!-- XXX should perhaps check count? but
             (a) might not be at the same level,
             (b) multiple parents with the same operation is a bug anyway. -->
        <xsl:value-of select="$class/name"/>
      </xsl:when>

      <xsl:when test="$class">
        <xsl:call-template name="class-of-operation-for-action">
          <xsl:with-param
            name="class"
            select="/domain/class[name=../inheritance[child=$class/name]/parent]"/>
          <xsl:with-param name="action" select="$action"/>
        </xsl:call-template>
      </xsl:when>

    </xsl:choose>

  </xsl:template>


  <!-- Called from domain/class, within the Create function, to
       apply an unguarded transition (if any) from the initial state. -->
  <xsl:template name="initialize-state-machine">

    <!-- if there is an unguarded transition from the initial state
         declare
            This : constant Handle renames Result;
            Ev : ColdFrame.Project.Events.Creation.Event (Result);
         begin
            {perform-transition}
         end;
         -->

    <xsl:variable name="init" select="statemachine/state[@initial]/name"/>
    <xsl:variable
      name="tr"
      select="statemachine/transition[source=$init and not (event)]"/>

    <xsl:if test="$tr">

      <xsl:value-of select="$II"/>
      <xsl:text>declare&#10;</xsl:text>
      <!-- fudge with This to avoid extra parameters! -->
      <xsl:value-of select="$III"/>
      <xsl:text>This : Handle renames Result;&#10;</xsl:text>

      <!-- the entry actions of the target state .. -->
      <xsl:variable
        name="action"
        select="statemachine/state[name=$tr/target]/action"/>

      <xsl:if test="operation[name=$action]/parameter">
        <xsl:value-of select="$III"/>
        <xsl:text>Ev : ColdFrame.Project.Events.Creation.Event (This);&#10;</xsl:text>
      </xsl:if>


      <xsl:value-of select="$II"/>
      <xsl:text>begin&#10;</xsl:text>

      <!-- Need to change context to call perform-transition from the
           target state. -->

      <xsl:for-each select="statemachine/state[name=$tr/source]">
        <xsl:call-template name="perform-transition">
          <xsl:with-param name="tr" select="$tr"/>
          <xsl:with-param name="indent" select="$III"/>
        </xsl:call-template>
      </xsl:for-each>

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
    <xsl:call-template name="identification-info"/>

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
    <xsl:call-template name="identification-info"/>

    <xsl:text>package body </xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>.Events is&#10;</xsl:text>
    <xsl:value-of select="$I"/>
    <xsl:text>procedure Initialize is separate;&#10;</xsl:text>
    <xsl:text>end </xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>.Events;&#10;</xsl:text>

    <xsl:call-template name="could-edit"/>
    <xsl:call-template name="identification-info"/>

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
