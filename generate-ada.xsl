<!-- $Id: generate-ada.xsl,v 04f7b672839f 2006/04/19 22:36:00 simonjwright $ -->
<!-- XSL stylesheet to generate Ada code. -->
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
  xmlns:ac="http://pushface.org/coldframe/association-collection"
  xmlns:as="http://pushface.org/coldframe/association"
  xmlns:at="http://pushface.org/coldframe/attribute"
  xmlns:cb="http://pushface.org/coldframe/callback"
  xmlns:cl="http://pushface.org/coldframe/class"
  xmlns:co="http://pushface.org/coldframe/collection"
  xmlns:in="http://pushface.org/coldframe/inheritance"
  xmlns:op="http://pushface.org/coldframe/operation"
  xmlns:se="http://pushface.org/coldframe/serialization"
  xmlns:st="http://pushface.org/coldframe/state"
  xmlns:td="http://pushface.org/coldframe/teardown"
  xmlns:ty="http://pushface.org/coldframe/type"
  xmlns:un="http://pushface.org/coldframe/unittest"
  xmlns:ut="http://pushface.org/coldframe/utilities"
  version="1.0">

  <xsl:include href="ada-association-collection.xsl"/>
  <xsl:include href="ada-association.xsl"/>
  <xsl:include href="ada-attribute.xsl"/>
  <xsl:include href="ada-callback.xsl"/>
  <xsl:include href="ada-class.xsl"/>
  <xsl:include href="ada-collection.xsl"/>
  <xsl:include href="ada-inheritance.xsl"/>
  <xsl:include href="ada-operation.xsl"/>
  <xsl:include href="ada-serialization.xsl"/>
  <xsl:include href="ada-state.xsl"/>
  <xsl:include href="ada-teardown.xsl"/>
  <xsl:include href="ada-type.xsl"/>
  <xsl:include href="ada-unittest.xsl"/>
  <xsl:include href="ada-utilities.xsl"/>

  <xsl:strip-space elements="*"/>

  <xsl:output method="text"/>


  <!-- +++++ Command line parameters. +++++ -->

  <!-- Controls when attribute accessor functions are generated. -->
  <xsl:param name="generate-accessors" select="'defined'"/>

  <!-- Controls when stub implementations are generated. -->
  <xsl:param name="generate-stubs" select="'no'"/>

  <!-- Controls when unit test support is generated. -->
  <xsl:param name="unit-test-support" select="'no'"/>

  <!-- Control indentation. -->
  <xsl:param name="standard-indent" select="'   '"/>
  <xsl:param name="continuation-indent" select="'  '"/>

  <!-- Control added blank lines: no or yes.. -->
  <xsl:param name="add-blank-lines" select="'yes'"/>

  <!-- Control verbosity: no or yes. -->
  <xsl:param name="verbose" select="'no'"/>

  <!-- Control comment paragraph fill width. -->
  <xsl:param name="fill-column" select="70"/>

  <!-- Control limit on using bounded containers. -->
  <xsl:param name="max-bounded-container" select="49"/>

  <!-- Control limit on number of hash buckets. -->
  <xsl:param name="max-hash-buckets" select="49"/>


  <!-- Global shorthands for indentation. -->
  <xsl:param name="I" select="$standard-indent"/>
  <xsl:param name="II" select="concat($I, $I)"/>
  <xsl:param name="III" select="concat($II, $I)"/>
  <xsl:param name="IIII" select="concat($III, $I)"/>
  <xsl:param name="IIIII" select="concat($IIII, $I)"/>
  <xsl:param name="C" select="$continuation-indent"/>
  <xsl:param name="IC" select="concat($I, $C)"/>
  <xsl:param name="IIC" select="concat($II, $C)"/>
  <xsl:param name="IIIC" select="concat($III, $C)"/>
  <xsl:param name="IIIIC" select="concat($IIII, $C)"/>
  <xsl:param name="IIIIIC" select="concat($IIIII, $C)"/>

  <!-- Added blank lines -->
  <xsl:param name="blank-line">
    <xsl:choose>
      <xsl:when test="$add-blank-lines='yes'">
        <xsl:text>&#10;</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:param>

  <!-- Remember the main document. -->
  <xsl:variable name="main-document" select="/"/>


  <!-- The overridable main template. -->
  <xsl:template match="domain">
    <xsl:apply-templates select="." mode="coldframe"/>
  </xsl:template>


  <!-- Generate the top-level package for the domain, then all the
       others. -->
  <xsl:template match="domain" mode="coldframe">

    <xsl:call-template name="ut:do-not-edit"/>
    <xsl:text>pragma Style_Checks (Off);&#10;</xsl:text>

    <!-- Identification info -->
    <xsl:call-template name="ut:identification-info"/>
    <!-- Marker for result of SLOC calculations. -->
    <xsl:text>--  Lines: LINES-OF-CODE&#10;</xsl:text>

    <!-- Commentary. -->
    <xsl:value-of select="$blank-line"/>
    <xsl:call-template name="ut:commentary">
      <xsl:with-param name="separate-pars" select="$blank-line"/>
    </xsl:call-template>

    <!-- Any context clauses needed for top-level package .. -->
    <xsl:call-template name="ut:progress-message">
      <xsl:with-param name="m" select="'.. domain context ..'"/>
    </xsl:call-template>
    <xsl:call-template name="ty:domain-context"/>

    <!-- .. the top-level package spec .. -->
    <xsl:call-template name="ut:progress-message">
      <xsl:with-param name="m" select="'.. top-level package spec ..'"/>
    </xsl:call-template>
    <xsl:text>package </xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text> is&#10;</xsl:text>
    <xsl:value-of select="$blank-line"/>

    <!-- If there are protected types, or operations of types, or if
         we are generating stubs, we will need a package body.
         The reason for needing a package body if we are stubbing is
         that the full domain might have elements that require a body;
         if the stubbed interface doesn't, the extra and unneeded body
         code would be an embarrassment.
         -->
    <xsl:variable
      name="type-operations" 
      select="type/operation
              [not(@access)
              and not(@suppressed)
              and not(@imported)
              and not(@renames)]"/>
    <xsl:variable
      name="plain-type-operations"
      select="type[not(@protected)]/operation[not(@access)
              and not(@suppressed)
              and not(@imported)
              and not(@renames)]"/>

    <xsl:if test="$generate-stubs='yes' 
                  and not($plain-type-operations)
                  and not(type/@protected)">
      <xsl:value-of select="$I"/>
      <xsl:text>pragma Elaborate_Body;&#10;</xsl:text>
      <xsl:value-of select="$blank-line"/>
    </xsl:if>

    <!-- .. any exceptions .. -->
    <xsl:call-template name="ut:progress-message">
      <xsl:with-param name="m" select="'.. any exceptions ..'"/>
    </xsl:call-template>
    <xsl:for-each select="exception">
      <xsl:sort select="name"/>

      <xsl:value-of select="$I"/>
      <xsl:value-of select="name"/>
      <xsl:text> : exception</xsl:text>

      <xsl:choose>
        <xsl:when test="@imported">
          <xsl:text>&#10;</xsl:text>
          <xsl:value-of select="$IC"/>
          <xsl:text>renames </xsl:text>
          <xsl:value-of select="@imported"/>.<xsl:value-of select="."/>
        </xsl:when>
        <xsl:when test="@renames">
          <xsl:text>&#10;</xsl:text>
          <xsl:value-of select="$IC"/>
          <xsl:text>renames </xsl:text>
          <xsl:value-of select="@renames"/>
        </xsl:when>
      </xsl:choose>

      <xsl:text>;&#10;</xsl:text>
      <xsl:call-template name="ut:commentary">
        <xsl:with-param name="indent" select="$I"/>
      </xsl:call-template>
      <xsl:value-of select="$blank-line"/>

    </xsl:for-each>

    <!-- .. any constants .. -->
    <xsl:call-template name="ut:progress-message">
      <xsl:with-param name="m" select="'.. any constants ..'"/>
    </xsl:call-template>
    <xsl:if test="references='pi'">
      <xsl:value-of select="$I"/>
      <xsl:text>Pi : constant := Ada.Numerics.Pi;&#10;</xsl:text>
      <xsl:value-of select="$blank-line"/>
    </xsl:if>

    <!-- .. any specially-declared public types .. -->
    <xsl:call-template name="ut:progress-message">
      <xsl:with-param
        name="m"
        select="'.. any specially-declared public types ..'"/>
    </xsl:call-template>

    <xsl:call-template name="ty:sorted-domain-types">
      <xsl:with-param 
        name="types" 
        select="/domain/type[not(@visibility='private')]"/>
      <xsl:with-param 
        name="finished" 
        select="type[@standard]"/>
    </xsl:call-template>

    <!-- .. any type operations .. -->
    <xsl:call-template name="ut:progress-message">
      <xsl:with-param
        name="m"
        select="'.. any operations of public types ..'"/>
    </xsl:call-template>

    <!-- .. access-to-operation .. -->
    <xsl:apply-templates
      select="type[not(@visibility='private')]/operation[@access]"
      mode="op:access-to-operation">
      <xsl:sort select="name"/>
      <xsl:with-param name="is-class" select="'no'"/>
    </xsl:apply-templates>

    <!--  .. others .. -->
    <xsl:apply-templates
      select="type[not(@visibility='private') and not(@protected)]
              /operation[not(@access) and not(@suppressed)]"
      mode="ty:domain-type-operation-spec">
      <xsl:sort select="name"/>
    </xsl:apply-templates>

    <!-- The private part. -->
    <xsl:text>private&#10;</xsl:text>
    <xsl:value-of select="$blank-line"/>

     <!-- .. any specially-declared private types .. -->
    <xsl:call-template name="ut:progress-message">
      <xsl:with-param
        name="m"
        select="'.. any specially-declared private types ..'"/>
    </xsl:call-template>

    <xsl:call-template name="ty:sorted-domain-types">
      <xsl:with-param
        name="types"
        select="/domain/type[@visibility='private']"/>
      <!-- Types already output in the public part are visible here, too. -->
      <xsl:with-param 
        name="finished" 
        select="type[@standard or not(@visibility='private')]"/>
    </xsl:call-template>

    <!-- .. any type operations .. -->
    <xsl:call-template name="ut:progress-message">
      <xsl:with-param
        name="m"
        select="'.. any operations of private types ..'"/>
    </xsl:call-template>

    <!-- .. access-to-operation .. -->
    <xsl:apply-templates
      select="type[@visibility='private']/operation[@access]"
      mode="op:access-to-operation">
      <xsl:sort select="name"/>
      <xsl:with-param name="is-class" select="'no'"/>
    </xsl:apply-templates>

    <!--  .. others .. -->
    <xsl:apply-templates
      select="type[@visibility='private' and not(@protected)]
              /operation[not(@access) and not(@suppressed)]"
      mode="ty:domain-type-operation-spec">
      <xsl:sort select="name"/>
    </xsl:apply-templates>

   <!-- .. initialization detection .. -->
    <xsl:value-of select="$I"/>
    <xsl:text>Domain_Initializing : Boolean := False;&#10;</xsl:text>
    <xsl:value-of select="$I"/>
    <xsl:text>Domain_Initialized : Boolean := False;&#10;</xsl:text>
    <xsl:value-of select="$blank-line"/>

    <!-- .. and close. -->
    <xsl:text>end </xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>;&#10;</xsl:text>

    <!-- .. the domain package body, if needed .. -->
    <xsl:if test="$generate-stubs='yes' or $type-operations">

      <xsl:call-template name="ut:do-not-edit"/>
      <xsl:text>pragma Style_Checks (Off);&#10;</xsl:text>
      <xsl:call-template name="ut:identification-info"/>

      <!-- If we have operations of plain types and we're stubbing, we
           must register them. -->

      <xsl:if test="$generate-stubs='yes' and $plain-type-operations">
        <xsl:text>with ColdFrame.Stubs;&#10;</xsl:text>
      </xsl:if>

      <xsl:text>package body </xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text> is&#10;</xsl:text>
      <xsl:value-of select="$blank-line"/>

      <!-- Protected types need stubs. -->
      <xsl:for-each select="type[@protected]">
        <xsl:sort select="name"/>
        <xsl:value-of select="$I"/>
        <xsl:text>pragma Style_Checks (On);&#10;</xsl:text>
        <xsl:value-of select="$I"/>
        <xsl:text>protected body </xsl:text>
        <xsl:value-of select="name"/>
        <xsl:text> is separate;&#10;</xsl:text>
        <xsl:value-of select="$I"/>
        <xsl:text>pragma Style_Checks (Off);&#10;</xsl:text>
        <xsl:value-of select="$blank-line"/>
      </xsl:for-each>

      <!-- Operations that were analyst-specified need stubs. -->
      <xsl:apply-templates
        select="type[not(@protected)]
                /operation[not(@access) and not(@suppressed)]"
        mode="ty:domain-type-operation-body-stub">
        <xsl:sort select="name"/>
      </xsl:apply-templates>

      <!-- If we have operations of plain types and we're stubbing, we
           must register them. -->
      <xsl:if test="$generate-stubs='yes' and $plain-type-operations">
        <xsl:text>begin&#10;</xsl:text>
        <xsl:value-of select="$blank-line"/>
         <xsl:for-each select="$plain-type-operations">
           <!-- I know that this doesn't exclude all the operations that
                won't be stubbed, but there's no harm in having too
                many. And you shouldn't have accessor operations on
                public classes. -->
           <xsl:sort select="name"/>
           <xsl:call-template name="op:register-operation-stub">
             <xsl:with-param name="subprogram-name">
               <xsl:value-of select="../../name"/>
               <xsl:text>.</xsl:text>
               <xsl:value-of select="name"/>
             </xsl:with-param>
           </xsl:call-template>
        </xsl:for-each>
      </xsl:if>

      <!-- .. and close. -->
      <xsl:text>end </xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text>;&#10;</xsl:text>

    </xsl:if>

    <!-- .. domain type operations .. -->
    <xsl:call-template name="ut:progress-message">
      <xsl:with-param
        name="m"
        select="'.. operations of types ..'"/>
    </xsl:call-template>
    <xsl:apply-templates
      select="type[not(@protected)]
              /operation[not(@access) and not(@suppressed)]"
      mode="ty:domain-type-operation-body">
    </xsl:apply-templates>


    <!-- .. protected type bodies .. -->
    <xsl:call-template name="ut:progress-message">
      <xsl:with-param
        name="m"
        select="'.. protected bodies ..'"/>
    </xsl:call-template>
    <xsl:apply-templates
      select="type[@protected]"
      mode="ty:protected-type-body">
    </xsl:apply-templates>


    <!-- .. the domain event manager .. -->
    <xsl:call-template name="ut:progress-message">
      <xsl:with-param
        name="m"
        select="'.. the domain event manager ..'"/>
    </xsl:call-template>
    <xsl:call-template name="st:event-manager-spec"/>
    <xsl:call-template name="st:event-manager-body"/>

    <!-- .. the domain Initialize procedure .. -->
    <xsl:call-template name="ut:progress-message">
      <xsl:with-param
        name="m"
        select="'.. the domain Initialize procedure ..'"/>
    </xsl:call-template>

    <!--
         with ColdFrame.Project.Events;
         procedure {domain}.Initialize
           (Dispatcher : ColdFrame.Project.Events.Event_Queue_P := null);
         -->

    <xsl:call-template name="ut:do-not-edit"/>
    <xsl:text>pragma Style_Checks (Off);&#10;</xsl:text>
    <xsl:call-template name="ut:identification-info"/>

    <xsl:text>with ColdFrame.Project.Events;&#10;</xsl:text>
    <xsl:text>procedure </xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>.Initialize&#10;</xsl:text>
    <xsl:value-of select="$C"/>
    <xsl:text>(Dispatcher : ColdFrame.Project.Events.Event_Queue_P := null);&#10;</xsl:text>

    <!-- The domain Initialize procedure body. -->
    <xsl:call-template name="ut:progress-message">
      <xsl:with-param
        name="m"
        select="'.. the domain Initialize procedure body ..'"/>
    </xsl:call-template>

    <!--
         with Ada.Exceptions;
         with ColdFrame.Exceptions;
         with ColdFrame.Project.Log_Error;
         with {domain-init-proc-package};
         with {domain}.Events;
         with {domain}.{class};
         procedure {domain}.Initialize
           (Dispatcher : ColdFrame.Project.Events.Event_Queue_P := null) is
            use type ColdFrame.Project.Events.Event_Queue_P;
         begin
            if not Domain_Initialized then
               Domain_Initializing := True;
               if Dispatcher /= null then
                  Events.Dispatcher := Dispatcher;
               else
                  Events.Initialize;
               end if;
               ColdFrame.Project.Events.Add_Reference (Events.Dispatcher);
               {domain-init-proc};
               {class}.CF_Class_Initialize:
               {class}.{init-operation};
               Domain_Initialized := True;
               Domain_Initializing := False;
            end if;
         exception
            when E : Others =>
               ColdFrame.Project.Log_Error
                 (Ada.Exceptions.Exception_Information (E));
               raise ColdFrame.Exceptions.Initialization_Error;
         end {domain}.Initialize;
         -->
    <xsl:call-template name="ut:do-not-edit"/>
    <xsl:text>pragma Style_Checks (Off);&#10;</xsl:text>
    <xsl:call-template name="ut:identification-info"/>

    <xsl:variable
      name="class-initializations"
      select="class[attribute[@class and initial]
              or @singleton
              or (@public and attribute)]"/>

    <xsl:variable
      name="instance-initializations"
      select="class[operation/@initialize]"/>

    <!-- .. withs, starting with exception handling .. -->
    <xsl:text>with Ada.Exceptions;&#10;</xsl:text>
    <xsl:text>with ColdFrame.Exceptions;&#10;</xsl:text>
    <xsl:text>with ColdFrame.Project.Log_Error;&#10;</xsl:text>
    <!-- .. the Events package .. -->
    <xsl:text>with </xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>.Events;&#10;</xsl:text>

    <xsl:for-each select="$class-initializations">
      <xsl:sort select="name"/>
      <xsl:text>with </xsl:text>
      <xsl:value-of select="../name"/>
      <xsl:text>.</xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text>.CF_Class_Initialize;&#10;</xsl:text>
    </xsl:for-each>

    <xsl:if test="initialize">
      <xsl:variable name="context">
        <xsl:call-template name="ty:find-source-package">
          <xsl:with-param name="input" select="initialize"/>
        </xsl:call-template>
      </xsl:variable>
      <xsl:if test="$context != ''">
        <xsl:text>with </xsl:text>
        <xsl:value-of select="$context"/>
        <xsl:text>;&#10;</xsl:text>
      </xsl:if>
    </xsl:if>

    <xsl:for-each select="$instance-initializations">
      <xsl:sort select="name"/>
      <xsl:text>with </xsl:text>
      <xsl:value-of select="../name"/>.<xsl:value-of select="name"/>
      <xsl:text>.CF_Initialize;&#10;</xsl:text>
    </xsl:for-each>

    <xsl:text>procedure </xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>.Initialize&#10;</xsl:text>
    <xsl:value-of select="$C"/>
    <xsl:text>(Dispatcher : ColdFrame.Project.Events.Event_Queue_P := null) is&#10;</xsl:text>
    <xsl:value-of select="$I"/>
    <xsl:text>use type ColdFrame.Project.Events.Event_Queue_P;&#10;</xsl:text>
    <xsl:text>begin&#10;</xsl:text>

    <xsl:value-of select="$I"/>
    <xsl:text>if not Domain_Initialized then&#10;</xsl:text>

    <!-- Mark initializing (required for any Create calls in class
         initialization procedures). -->
    <xsl:value-of select="$II"/>
    <xsl:text>Domain_Initializing := True;&#10;</xsl:text>

    <!-- .. the Events package initialization .. -->
    <xsl:value-of select="$II"/>
    <xsl:text>if Dispatcher /= null then&#10;</xsl:text>
    <xsl:value-of select="$III"/>
    <xsl:text>Events.Dispatcher := Dispatcher;&#10;</xsl:text>
    <xsl:value-of select="$II"/>
    <xsl:text>else&#10;</xsl:text>
    <xsl:value-of select="$III"/>
    <xsl:text>Events.Initialize;&#10;</xsl:text>
    <xsl:value-of select="$II"/>
    <xsl:text>end if;&#10;</xsl:text>
    <xsl:value-of select="$II"/>
    <xsl:text>ColdFrame.Project.Events.Add_Reference (Events.Dispatcher);&#10;</xsl:text>

    <!-- .. any domain initialization .. -->
    <xsl:if test="initialize">
      <xsl:value-of select="$II"/>
      <xsl:value-of select="initialize"/>
      <xsl:text>;&#10;</xsl:text>
    </xsl:if>

    <!-- .. class initializations .. -->
    <xsl:for-each select="$class-initializations">
      <xsl:sort select="name"/>
      <xsl:value-of select="$II"/>
      <xsl:value-of select="name"/>
      <xsl:text>.CF_Class_Initialize;&#10;</xsl:text>
    </xsl:for-each>

    <!-- .. instance operations .. -->
    <xsl:for-each select="$instance-initializations">
      <xsl:sort select="name"/>
      <xsl:value-of select="$II"/>
      <xsl:value-of select="name"/>
      <xsl:text>.CF_Initialize;&#10;</xsl:text>
    </xsl:for-each>

    <!-- .. mark initialization complete (so any waiting tasks can
         continue) .. --> 
    <xsl:value-of select="$II"/>
    <xsl:text>Domain_Initialized := True;&#10;</xsl:text>
    <xsl:value-of select="$II"/> <xsl:text>Domain_Initializing := False;&#10;</xsl:text>

    <xsl:value-of select="$I"/>
    <xsl:text>end if;&#10;</xsl:text>

    <xsl:text>exception&#10;</xsl:text>
    <xsl:value-of select="$I"/>
    <xsl:text>when E : others =&gt;&#10;</xsl:text>
    <xsl:value-of select="$II"/>
    <xsl:text>ColdFrame.Project.Log_Error&#10;</xsl:text>
    <xsl:value-of select="$IIC"/>
    <xsl:text>(Ada.Exceptions.Exception_Information (E));&#10;</xsl:text>
    <xsl:value-of select="$II"/>
    <xsl:text>raise ColdFrame.Exceptions.Initialization_Error;&#10;</xsl:text>

    <xsl:text>end </xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>.Initialize;&#10;</xsl:text>

    <!-- The domain Cascade_Initialize procedure .. -->
    <xsl:call-template name="ut:progress-message">
      <xsl:with-param
        name="m"
        select="'.. the domain Cascade_Initialize procedure ..'"/>
    </xsl:call-template>

    <!--
         with ColdFrame.Project.Events;
         procedure {domain}.Cascade_Initialize
           (Dispatcher : ColdFrame.Project.Events.Event_Queue_P := null);
         -->

    <xsl:call-template name="ut:do-not-edit"/>
    <xsl:text>pragma Style_Checks (Off);&#10;</xsl:text>
    <xsl:call-template name="ut:identification-info"/>

    <xsl:text>with ColdFrame.Project.Events;&#10;</xsl:text>
    <xsl:text>procedure </xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>.Cascade_Initialize&#10;</xsl:text>
    <xsl:value-of select="$C"/>
    <xsl:text>(Dispatcher : ColdFrame.Project.Events.Event_Queue_P := null);&#10;</xsl:text>

    <!-- .. the domain Cascade_Initialize procedure body .. -->
    <xsl:call-template name="ut:progress-message">
      <xsl:with-param
        name="m"
        select="'.. the domain Cascade_Initialize procedure body ..'"/>
    </xsl:call-template>

    <!--
         with {domain}.Initialize;
         procedure {domain}.Cascade_Initialize
           (Dispatcher : ColdFrame.Project.Events.Event_Queue_P := null) is
         begin
            Initialize (Dispatcher);
         end {domain}.Cascade_Initialize;
         -->

    <xsl:call-template name="ut:could-edit"/>
    <xsl:call-template name="ut:identification-info"/>

    <xsl:text>with </xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>.Initialize;&#10;</xsl:text>
    <xsl:text>procedure </xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>.Cascade_Initialize&#10;</xsl:text>
    <xsl:value-of select="$C"/>
    <xsl:text>(Dispatcher : ColdFrame.Project.Events.Event_Queue_P := null) is&#10;</xsl:text>
    <xsl:text>begin&#10;</xsl:text>
    <xsl:value-of select="$I"/>
    <xsl:text>Initialize (Dispatcher);&#10;</xsl:text>
    <xsl:text>end </xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>.Cascade_Initialize;&#10;</xsl:text>

    <!-- The domain Cascade_Tear_Down procedure .. -->
    <xsl:call-template name="ut:progress-message">
      <xsl:with-param
        name="m"
        select="'.. the domain Cascade_Tear_Down procedure ..'"/>
    </xsl:call-template>

    <!--
         procedure {domain}.Cascade_Tear_Down;
         -->

    <xsl:call-template name="ut:do-not-edit"/>
    <xsl:text>pragma Style_Checks (Off);&#10;</xsl:text>
    <xsl:call-template name="ut:identification-info"/>
    <xsl:text>procedure </xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>.Cascade_Tear_Down;&#10;</xsl:text>

    <!-- .. the domain Cascade_Tear_Down procedure body .. -->
    <xsl:call-template name="ut:progress-message">
      <xsl:with-param
        name="m"
        select="'.. the domain Cascade_Tear_Down procedure body ..'"/>
    </xsl:call-template>

    <!--
         with {domain}.Tear_Down;
         procedure {domain}.Cascade_Tear_Down is
         begin
            Tear_Down;
         end {domain}.Cascade_Tear_Down;
         -->

    <xsl:call-template name="ut:could-edit"/>
    <xsl:call-template name="ut:identification-info"/>

    <xsl:text>with </xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>.Tear_Down;&#10;</xsl:text>
    <xsl:text>procedure </xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>.Cascade_Tear_Down is&#10;</xsl:text>
    <xsl:text>begin&#10;</xsl:text>
    <xsl:value-of select="$I"/>
    <xsl:text>Tear_Down;&#10;</xsl:text>
    <xsl:text>end </xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>.Cascade_Tear_Down;&#10;</xsl:text>

    <!-- Any support packages for specially-declared types. -->
    <xsl:call-template name="ut:progress-message">
      <xsl:with-param
        name="m"
        select="'.. any support packages for specially-declared types ..'"/>
    </xsl:call-template>
    <xsl:apply-templates select="type" mode="ty:domain-type-support"/>

    <!-- Serializable support. -->

    <xsl:call-template name="ut:progress-message">
      <xsl:with-param
        name="m"
        select="'.. the support package for serializable types ..'"/>
    </xsl:call-template>
    <xsl:call-template name="se:serializable-type-spec"/>
    <xsl:call-template name="se:serializable-type-body"/>

    <!-- Package specs for individual classes. -->
    <xsl:call-template name="ut:progress-message">
      <xsl:with-param
        name="m"
        select="'.. package specs for individual classes ..'"/>
    </xsl:call-template>
    <xsl:apply-templates select="class" mode="cl:class-spec">
      <xsl:sort select="name"/>
    </xsl:apply-templates>

    <!-- Package bodies for individual classes. -->
    <xsl:call-template name="ut:progress-message">
      <xsl:with-param
        name="m"
        select="'.. package bodies for individual classes ..'"/>
    </xsl:call-template>
    <xsl:apply-templates select="class" mode="cl:class-body">
      <xsl:sort select="name"/>
    </xsl:apply-templates>

    <!-- Class initialization procedures. -->
    <xsl:call-template name="ut:progress-message">
      <xsl:with-param
        name="m"
        select="'.. class initializations ..'"/>
    </xsl:call-template>
    <xsl:apply-templates
      select="class[attribute[@class and initial]
              or @singleton
              or (@public and attribute)]"
      mode="cl:class-initialization">
      <xsl:sort select="name"/>
    </xsl:apply-templates>

    <!-- Domain initialization procedures. -->
    <xsl:call-template name="ut:progress-message">
      <xsl:with-param
        name="m"
        select="'.. &lt;&lt;init&gt;&gt; operations ..'"/>
    </xsl:call-template>
    <xsl:apply-templates
      select="class[operation/@initialize]"
      mode="cl:initialization">
      <xsl:sort select="name"/>
    </xsl:apply-templates>

    <!-- Collection support packages. -->
    <xsl:call-template name="ut:progress-message">
      <xsl:with-param name="m" select="'.. Collection support packages ..'"/>
    </xsl:call-template>
    <xsl:apply-templates select="class" mode="co:collection-support">
      <xsl:sort select="name"/>
    </xsl:apply-templates>

    <!-- Package specs for Associations -->
    <xsl:call-template name="ut:progress-message">
      <xsl:with-param name="m" select="'.. package specs for Associations ..'"/>
    </xsl:call-template>
    <xsl:apply-templates select="association" mode="as:association-spec">
      <xsl:sort select="name"/>
    </xsl:apply-templates>

    <!-- Package bodies for Associations -->
    <xsl:call-template name="ut:progress-message">
      <xsl:with-param
        name="m"
        select="'.. package bodies for Associations ..'"/>
    </xsl:call-template>
    <xsl:apply-templates select="association" mode="as:association-body">
      <xsl:sort select="name"/>
    </xsl:apply-templates>

    <!-- Package specs for Associations (navigation from collections) -->
    <xsl:call-template name="ut:progress-message">
      <xsl:with-param
        name="m"
        select="'.. package specs for Associations (collection navigation) ..'"/>
    </xsl:call-template>
    <xsl:apply-templates
      select="association"
      mode="ac:association-collection-spec">
      <xsl:sort select="name"/>
    </xsl:apply-templates>

    <!-- Package bodies for Associations (navigation from collections) -->
    <xsl:call-template name="ut:progress-message">
      <xsl:with-param
        name="m"
        select="'.. package bodies for Associations (collection navigation) ..'"/>
    </xsl:call-template>
    <xsl:apply-templates
      select="association"
      mode="ac:association-collection-body">
      <xsl:sort select="name"/>
    </xsl:apply-templates>

    <!-- Package specs for Inheritances -->
    <xsl:call-template name="ut:progress-message">
      <xsl:with-param name="m" select="'.. package specs for Inheritances ..'"/>
    </xsl:call-template>
    <xsl:apply-templates
      select="class[name=../inheritance/child or name=../inheritance/parent]"
      mode="in:inheritance-spec">
      <xsl:sort select="name"/>
    </xsl:apply-templates>

    <!-- Package bodies for Inheritances -->
    <xsl:call-template name="ut:progress-message">
      <xsl:with-param name="m" select="'.. package bodies for Inheritances ..'"/>
    </xsl:call-template>
    <xsl:apply-templates
      select="class[name=../inheritance/child or name=../inheritance/parent]"
      mode="in:inheritance-body">
      <xsl:sort select="name"/>
    </xsl:apply-templates>

    <xsl:if test="$unit-test-support='yes'">

      <!-- Package specs for Class unit test -->
      <xsl:call-template name="ut:progress-message">
        <xsl:with-param name="m" select="'.. package specs for Class unit test ..'"/>
      </xsl:call-template>
      <xsl:apply-templates
        select="class[attribute[not(@refers)] or statemachine]"
        mode="un:unit-spec">
        <xsl:sort select="name"/>
      </xsl:apply-templates>
      
      <!-- Package bodies for Class unit test -->
      <xsl:call-template name="ut:progress-message">
        <xsl:with-param name="m" select="'.. package bodies for Class unit test ..'"/>
      </xsl:call-template>
      <xsl:apply-templates
        select="class[attribute[not(@refers)] or statemachine]"
        mode="un:unit-body">
        <xsl:sort select="name"/>
      </xsl:apply-templates>

    </xsl:if>

    <!-- Package specs for Callbacks. -->
    <xsl:call-template name="ut:progress-message">
      <xsl:with-param name="m" select="'.. package specs for Callbacks ..'"/>
    </xsl:call-template>
    <xsl:apply-templates select="type[@callback]" mode="cb:callback-spec">
      <xsl:sort select="name"/>
    </xsl:apply-templates>

    <!-- Teardown -->
    <xsl:call-template name="ut:progress-message">
      <xsl:with-param name="m" select="'.. Teardown procedures ..'"/>
    </xsl:call-template>
    <xsl:call-template name="td:domain-teardown"/>

    <xsl:call-template name="ut:progress-message">
      <xsl:with-param name="m" select="'.. done.'"/>
    </xsl:call-template>

    <xsl:call-template name="ut:check-for-errors"/>

  </xsl:template>


  <!-- Catch unspecified default matches -->
  <xsl:template match="*"/>


</xsl:stylesheet>
