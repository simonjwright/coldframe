<!-- $Id: generate-ada.xsl,v e7c7746af93f 2003/06/14 12:54:54 simon $ -->
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
  version="1.0">

  <xsl:include href="ada-association.xsl"/>
  <xsl:include href="ada-association-collection.xsl"/>
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
  <xsl:include href="ada-utilities.xsl"/>

  <xsl:strip-space elements="*"/>

  <xsl:output method="text"/>


  <!-- +++++ Command line parameters. +++++ -->

  <!-- For identification info. -->
  <xsl:param name="coldframe-version"/>

  <!-- Controls when attribute accessor functions are generated. -->
  <xsl:param name="generate-accessors" select="defined"/>

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

  <!-- Generate the top-level package for the domain, then all the
       others. -->
  <xsl:template match="domain">

    <xsl:call-template name="do-not-edit"/>

    <!-- Identification info -->
    <xsl:call-template name="progress-message">
      <xsl:with-param name="m" select="'Generating identification info ..'"/>
    </xsl:call-template>
    <xsl:call-template name="identification-info"/>

    <!-- Commentary. -->
    <xsl:value-of select="$blank-line"/>
    <xsl:call-template name="commentary">
      <xsl:with-param name="separate-pars" select="$blank-line"/>
    </xsl:call-template>
    
    <!-- Any context clauses needed for top-level package .. -->
    <xsl:call-template name="progress-message">
      <xsl:with-param name="m" select="'.. domain context ..'"/>
    </xsl:call-template>
    <xsl:call-template name="domain-context"/>

    <!-- .. the top-level package spec .. -->
    <xsl:call-template name="progress-message">
      <xsl:with-param name="m" select="'.. top-level package spec ..'"/>
    </xsl:call-template>
    <xsl:text>package </xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text> is&#10;</xsl:text>
    <xsl:value-of select="$blank-line"/>

    <!-- .. any exceptions .. -->
    <xsl:call-template name="progress-message">
      <xsl:with-param name="m" select="'.. any exceptions ..'"/>
    </xsl:call-template>
    <xsl:for-each select="exception">
      <xsl:sort select="name"/>
      
      <xsl:value-of select="$I"/>
      <xsl:value-of select="name"/>
      <xsl:text> : exception;&#10;</xsl:text>
      <xsl:call-template name="commentary">
        <xsl:with-param name="indent" select="$I"/>
      </xsl:call-template>
      <xsl:value-of select="$blank-line"/>
       
    </xsl:for-each>

    <!-- .. any constants .. -->
    <xsl:call-template name="progress-message">
      <xsl:with-param name="m" select="'.. any constants ..'"/>
    </xsl:call-template>
    <xsl:if test="references='pi'">
      <xsl:value-of select="$I"/>
      <xsl:text>Pi : constant := Ada.Numerics.Pi;&#10;</xsl:text>
      <xsl:value-of select="$blank-line"/>
    </xsl:if>

    <!-- .. any specially-declared types .. -->
    <xsl:call-template name="progress-message">
      <xsl:with-param name="m" select="'.. any specially-declared types ..'"/>
    </xsl:call-template>
    <xsl:call-template name="domain-types"/>
    
    <!-- .. any type operations .. -->
    <xsl:call-template name="progress-message">
      <xsl:with-param name="m" select="'.. any operations of types ..'"/>
    </xsl:call-template>
    <xsl:apply-templates
      select="type[not(@protected)]/operation[@access and not(@suppressed)]"
      mode="access-to-operation">
      <xsl:sort select="name"/>
      <xsl:with-param name="is-class" select="'no'"/>
    </xsl:apply-templates>
    <xsl:apply-templates
      select="type[not(@protected)]/operation[not(@access) and not(@suppressed)]"
      mode="domain-type-operation-spec">
      <xsl:sort select="name"/>
    </xsl:apply-templates>
    
    <!-- .. initialization detection .. -->
    <xsl:text>private&#10;</xsl:text>
    <xsl:value-of select="$blank-line"/>
    <xsl:value-of select="$I"/>
    <xsl:text>Domain_Initialized : Boolean := False;&#10;</xsl:text>
    <xsl:value-of select="$blank-line"/>
    
    <!-- .. and close. -->
    <xsl:text>end </xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>;&#10;</xsl:text>
    
    <!-- .. the domain package body, if needed .. -->
    <xsl:if test="type/operation[not(@access) and not(@suppressed)]">
      
      <xsl:call-template name="do-not-edit"/>
      
      <xsl:text>package body </xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text> is&#10;</xsl:text>
      <xsl:value-of select="$blank-line"/>
      
      <!-- Protected types need stubs. -->
      <xsl:for-each select="type[@protected]">
        <xsl:sort select="name"/>
        <xsl:value-of select="$I"/>
        <xsl:text>protected body </xsl:text>
        <xsl:value-of select="name"/>
        <xsl:text> is separate;&#10;</xsl:text>
        <xsl:value-of select="$blank-line"/>
      </xsl:for-each>
      
      <!-- Operations that were analyst-specified need stubs. -->
      <xsl:apply-templates
        select="type[not(@protected)]/operation[not(@access) and not(@suppressed)]"
        mode="domain-type-operation-body-stub">
        <xsl:sort select="name"/>
      </xsl:apply-templates>

      <!-- .. and close. -->
      <xsl:text>end </xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text>;&#10;</xsl:text>
      
    </xsl:if>
    
    <!-- .. domain type operations .. -->
    <xsl:call-template name="progress-message">
      <xsl:with-param
        name="m"
        select="'.. operations of types ..'"/>
    </xsl:call-template>
    <xsl:apply-templates
      select="type[not(@protected)]/operation[not(@access) and not(@suppressed)]"
      mode="domain-type-operation-body">
    </xsl:apply-templates>
    
    
    <!-- .. protected type bodies .. -->
    <xsl:call-template name="progress-message">
      <xsl:with-param
        name="m"
        select="'.. protected bodies ..'"/>
    </xsl:call-template>
    <xsl:apply-templates
      select="type[@protected]"
      mode="protected-type-body">
    </xsl:apply-templates>
    
    
    <!-- .. the domain event manager .. -->
    <xsl:call-template name="progress-message">
      <xsl:with-param
        name="m"
        select="'.. the domain event manager ..'"/>
    </xsl:call-template>
    <xsl:call-template name="event-manager-spec"/>
    <xsl:call-template name="event-manager-body"/>
    
    <!-- .. the domain Initialize procedure .. -->
    <xsl:call-template name="progress-message">
      <xsl:with-param
        name="m"
        select="'.. the domain Initialize procedure ..'"/>
    </xsl:call-template>

    <!--
         with ColdFrame.Project.Events;
         procedure {domain}.Initialize
           (Dispatcher : ColdFrame.Project.Events.Event_Queue_P := null);
         -->

    <xsl:call-template name="do-not-edit"/>
    <xsl:text>with ColdFrame.Project.Events;&#10;</xsl:text>
    <xsl:text>procedure </xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>.Initialize&#10;</xsl:text>
    <xsl:value-of select="$C"/>
    <xsl:text>(Dispatcher : ColdFrame.Project.Events.Event_Queue_P := null);&#10;</xsl:text>

    <!-- The domain Initialize procedure body. -->
    <xsl:call-template name="progress-message">
      <xsl:with-param
        name="m"
        select="'.. the domain Initialize procedure body ..'"/>
    </xsl:call-template>

    <!--
         with Ada.Exceptions;
         with ColdFrame.Exceptions;
         with ColdFrame.Project.Log_Error;
         with {domain}.Events;
         with {domain}.{class};
         procedure {domain}.Initialize
           (Dispatcher : ColdFrame.Project.Events.Event_Queue_P := null) is
            use type ColdFrame.Project.Events.Event_Queue_P;
         begin
            if not Domain_Initialized then
               if Dispatcher /= null then
                  Events.Dispatcher := Dispatcher;
               else
                  Events.Initialize;
               end if;
               {class}.Class_Initialize:
               {class}.{init-operation};
               Domain_Initialized := True;
            end if;
         exception
            when E : Others =>
               ColdFrame.Project.Log_Error
                 (Ada.Exceptions.Exception_Information (E));
               raise ColdFrame.Exceptions.Initialization_Error;
         end {domain}.Initialize;
         -->
    <xsl:call-template name="do-not-edit"/>

    <xsl:variable
      name="class-initializations"
      select="class[attribute[@class and initial] or @singleton]"/>

    <xsl:variable
      name="initialize-procedures"
      select="class/operation[@initialize]"/>

    <xsl:for-each select="$initialize-procedures[parameter or @return]">
      <xsl:sort select="../name"/>
      <xsl:sort select="name"/>
      <xsl:message>
        <xsl:text>CF: bad "initialize" operation </xsl:text>
        <xsl:value-of select="../name"/>.<xsl:value-of select="name"/>
      </xsl:message>
    </xsl:for-each>

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
      <xsl:text>.Class_Initialize;&#10;</xsl:text>
    </xsl:for-each>

    <xsl:for-each select="$initialize-procedures">
      <xsl:sort select="../name"/>
      <xsl:sort select="name"/>
      <xsl:text>with </xsl:text>
      <xsl:value-of select="../../name"/>.<xsl:value-of select="../name"/>
      <xsl:text>;&#10;</xsl:text>
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

    <!-- .. class initializations .. -->
    <xsl:for-each select="$class-initializations">
      <xsl:sort select="name"/>
      <xsl:value-of select="$II"/>
      <xsl:value-of select="name"/>
      <xsl:text>.Class_Initialize;&#10;</xsl:text>
    </xsl:for-each>

    <!-- .. <<init>> operations .. -->
    <xsl:for-each select="$initialize-procedures">
      <xsl:sort select="../name"/>
      <xsl:sort select="name"/>
      <xsl:value-of select="$II"/>
      <xsl:value-of select="../name"/>
      <xsl:text>.</xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text>;&#10;</xsl:text>
    </xsl:for-each>

    <xsl:value-of select="$II"/>
    <xsl:text>Domain_Initialized := True;&#10;</xsl:text>
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
    <xsl:call-template name="progress-message">
      <xsl:with-param
        name="m"
        select="'.. the domain Cascade_Initialize procedure ..'"/>
    </xsl:call-template>

    <!--
         with ColdFrame.Project.Events;
         procedure {domain}.Cascade_Initialize
           (Dispatcher : ColdFrame.Project.Events.Event_Queue_P := null);
         -->

    <xsl:call-template name="do-not-edit"/>
    <xsl:text>with ColdFrame.Project.Events;&#10;</xsl:text>
    <xsl:text>procedure </xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>.Cascade_Initialize&#10;</xsl:text>
    <xsl:value-of select="$C"/>
    <xsl:text>(Dispatcher : ColdFrame.Project.Events.Event_Queue_P := null);&#10;</xsl:text>

    <!-- .. the domain Cascade_Initialize procedure body .. -->
    <xsl:call-template name="progress-message">
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

    <xsl:call-template name="could-edit"/>
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
    <xsl:call-template name="progress-message">
      <xsl:with-param
        name="m"
        select="'.. the domain Cascade_Tear_Down procedure ..'"/>
    </xsl:call-template>

    <!--
         procedure {domain}.Cascade_Tear_Down;
         -->

    <xsl:call-template name="do-not-edit"/>
    <xsl:text>procedure </xsl:text>
    <xsl:value-of select="name"/>
    <xsl:text>.Cascade_Tear_Down;&#10;</xsl:text>

    <!-- .. the domain Cascade_Tear_Down procedure body .. -->
    <xsl:call-template name="progress-message">
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

    <xsl:call-template name="could-edit"/>
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
    <xsl:call-template name="progress-message">
      <xsl:with-param
        name="m"
        select="'.. any support packages for specially-declared types ..'"/>
    </xsl:call-template>
    <xsl:apply-templates select="type" mode="domain-type-support"/>

    <!-- Serializable support. -->

    <xsl:call-template name="progress-message">
      <xsl:with-param
        name="m"
        select="'.. the support package for serializable types ..'"/>
    </xsl:call-template>
    <xsl:call-template name="serializable-type-spec"/>
    <xsl:call-template name="serializable-type-body"/>

    <!-- Package specs for individual classes. -->
    <xsl:call-template name="progress-message">
      <xsl:with-param
        name="m"
        select="'.. package specs for individual classes ..'"/>
    </xsl:call-template>
    <xsl:apply-templates select="class" mode="class-spec">
      <xsl:sort select="name"/>
    </xsl:apply-templates>

    <!-- Package bodies for individual classes. -->
    <xsl:call-template name="progress-message">
      <xsl:with-param
        name="m"
        select="'.. package bodies for individual classes ..'"/>
    </xsl:call-template>
    <xsl:apply-templates select="class" mode="class-body">
      <xsl:sort select="name"/>
    </xsl:apply-templates>

    <!-- Class initialization procedures. -->
    <xsl:call-template name="progress-message">
      <xsl:with-param
        name="m"
        select="'.. class initializations ..'"/>
    </xsl:call-template>
    <xsl:apply-templates
      select="class[attribute[@class and initial] or @singleton]"
      mode="class-initialization">
      <xsl:sort select="name"/>
    </xsl:apply-templates>

    <!-- Collection support packages. -->
    <xsl:call-template name="progress-message">
      <xsl:with-param name="m" select="'.. Collection support packages ..'"/>
    </xsl:call-template>
    <xsl:apply-templates select="class" mode="collection-support">
      <xsl:sort select="name"/>
    </xsl:apply-templates>

    <!-- Package specs for Associations -->
    <xsl:call-template name="progress-message">
      <xsl:with-param name="m" select="'.. package specs for Associations ..'"/>
    </xsl:call-template>
    <xsl:apply-templates select="association" mode="association-spec">
      <xsl:sort select="name"/>
    </xsl:apply-templates>

    <!-- Package bodies for Associations -->
    <xsl:call-template name="progress-message">
      <xsl:with-param
        name="m"
        select="'.. package bodies for Associations ..'"/>
    </xsl:call-template>
    <xsl:apply-templates select="association" mode="association-body">
      <xsl:sort select="name"/>
    </xsl:apply-templates>

    <!-- Package specs for Associations (navigation from collections) -->
    <xsl:call-template name="progress-message">
      <xsl:with-param
        name="m"
        select="'.. package specs for Associations (collection navigation) ..'"/>
    </xsl:call-template>
    <xsl:apply-templates select="association" mode="association-collection-spec">
      <xsl:sort select="name"/>
    </xsl:apply-templates>

    <!-- Package bodies for Associations (navigation from collections) -->
    <xsl:call-template name="progress-message">
      <xsl:with-param
        name="m"
        select="'.. package bodies for Associations (collection navigation) ..'"/>
    </xsl:call-template>
    <xsl:apply-templates select="association" mode="association-collection-body">
      <xsl:sort select="name"/>
    </xsl:apply-templates>

    <!-- Package specs for Inheritances -->
    <xsl:call-template name="progress-message">
      <xsl:with-param name="m" select="'.. package specs for Inheritances ..'"/>
    </xsl:call-template>
    <xsl:apply-templates
      select="class[name=../inheritance/child or name=../inheritance/parent]"
      mode="inheritance-spec">
      <xsl:sort select="name"/>
    </xsl:apply-templates>

    <!-- Package bodies for Inheritances -->
    <xsl:call-template name="progress-message">
      <xsl:with-param name="m" select="'.. package bodies for Inheritances ..'"/>
    </xsl:call-template>
    <xsl:apply-templates
      select="class[name=../inheritance/child or name=../inheritance/parent]"
      mode="inheritance-body">
      <xsl:sort select="name"/>
    </xsl:apply-templates>

    <!-- Package specs for callbacks. -->
    <xsl:call-template name="progress-message">
      <xsl:with-param name="m" select="'.. package specs for Callbacks ..'"/>
    </xsl:call-template>
    <xsl:apply-templates select="type[@callback]" mode="callback-spec">
      <xsl:sort select="name"/>
    </xsl:apply-templates>
    
    <!-- Teardown -->
    <xsl:call-template name="progress-message">
      <xsl:with-param name="m" select="'.. Teardown procedures ..'"/>
    </xsl:call-template>
    <xsl:call-template name="domain-teardown"/>
    <xsl:call-template name="event-teardown-spec"/>
    <xsl:call-template name="event-teardown-body"/>

    <xsl:call-template name="progress-message">
      <xsl:with-param name="m" select="'.. done.'"/>
    </xsl:call-template>

  </xsl:template>


  <!-- Called at domain to generate identification information. -->
  <xsl:template name="identification-info">
    <xsl:text>--  Domain revision: </xsl:text>
    <xsl:choose>
      <xsl:when test="revision">
        <xsl:value-of select="revision"/>        
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>unspecified</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:text>&#10;</xsl:text>
    <xsl:text>--  Extraction date: </xsl:text>
    <xsl:value-of select="date/day"/>
    <xsl:text> </xsl:text>
    <xsl:value-of select="date/month"/>
    <xsl:text> </xsl:text>
    <xsl:value-of select="date/year"/>
    <xsl:text>, </xsl:text>
    <xsl:value-of select="date/time"/>
    <xsl:text>&#10;</xsl:text>
    <xsl:text>--  Extractor: </xsl:text>
    <xsl:value-of select="extractor"/>
    <xsl:text>&#10;</xsl:text>
    <xsl:text>--  Normalizer: </xsl:text>
    <xsl:value-of select="normalizer"/>
    <xsl:text>&#10;</xsl:text>
    <xsl:text>--  Generator: </xsl:text>
    <xsl:value-of select="$coldframe-version"/>
    <xsl:text>&#10;</xsl:text>
    <xsl:text>--  Lines: LINES-OF-CODE&#10;</xsl:text>
  </xsl:template>


  <!-- Catch unspecified default matches -->
  <xsl:template match="*"/>


</xsl:stylesheet>
