<!-- $Id: ada-callback.xsl,v 35e50a57c3b6 2001/05/11 19:18:50 simon $ -->
<!-- XSL stylesheet to generate Ada code for Callbacks. -->
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


<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="1.0">

  <xsl:template match="type[@callback]" mode="callback-spec">
    <!--
with bc.containers.collections.bounded;
with coldframe.callbacks;
package domain.type_callback is
  type callback is access procedure (param : type);
  procedure register (proc : callback);
  procedure deregister (proc : callback);
  procedure call_callbacks (with_param : type);
private
  package abstract_containers is new bc.containers (callback);
  package abstract_collections is new abstract_containers.collections;
  package collections is new abstract_collections.bounded (maximum_size => 10);
  procedure remove (from : in out collections.collection; proc : callback);
  package callbacks is new coldframe.callbacks
    (t => type,
     p => callback,
     callback_containers => abstract_containers,
     container => collections.collection,
     add => collections.append,
     remove => remove);
end domain.type_callback;
         -->

    <xsl:variable name="t">
      <xsl:value-of select="../name"/>.<xsl:value-of select="name"/>
    </xsl:variable>

    <xsl:text>with BC.Containers.Collections.Bounded;&#10;</xsl:text>
    <xsl:text>with ColdFrame.Callbacks;&#10;</xsl:text>
    <xsl:text>package </xsl:text>
    <xsl:value-of select="$t"/>
    <xsl:text>_Callback is&#10;</xsl:text>
    <xsl:text>  type Callback is access procedure (Param : </xsl:text>
    <xsl:value-of select="$t"/>
    <xsl:text>);&#10;</xsl:text>
    <xsl:text>  procedure Register (Proc : Callback);&#10;</xsl:text>
    <xsl:text>  procedure Deregister (Proc : Callback);&#10;</xsl:text>
    <xsl:text>  procedure Call_Callbacks (With_Param : </xsl:text>
    <xsl:value-of select="$t"/>
    <xsl:text>);&#10;</xsl:text>
    <xsl:text>private&#10;</xsl:text>
    <xsl:text>  package Abstract_Containers is new BC.Containers (Callback);&#10;</xsl:text>
    <xsl:text>  package Abstract_Collections is new Abstract_Containers.Collections;&#10;</xsl:text>
    <xsl:text>  package Collections is new Abstract_Collections.Bounded (Maximum_Size => </xsl:text>
    <xsl:value-of select="@callback"/>
    <xsl:text>);&#10;</xsl:text>
    <xsl:text>  procedure Remove (From : in out Collections.Collection; Proc : Callback);&#10;</xsl:text>
    <xsl:text>  package Callbacks is new ColdFrame.Callbacks&#10;</xsl:text>
    <xsl:text>    (T => </xsl:text>
    <xsl:value-of select="$t"/>
    <xsl:text>,&#10;</xsl:text>
    <xsl:text>     P =&gt; Callback,&#10;</xsl:text>
    <xsl:text>     Callback_Containers =&gt; Abstract_Containers,&#10;</xsl:text>
    <xsl:text>     Container =&gt; Collections.Collection,&#10;</xsl:text>
    <xsl:text>     Add =&gt; Collections.Append,&#10;</xsl:text>
    <xsl:text>     Remove =&gt; Remove);&#10;</xsl:text>
    <xsl:text>end </xsl:text>
    <xsl:value-of select="$t"/>
    <xsl:text>_Callback;&#10;</xsl:text>
  </xsl:template>

  <xsl:template match="*" mode="callback-spec"/>


  <xsl:template match="type[@callback]" mode="callback-body">
    <!--
package body domain.type_callback is
  procedure register (proc : callback) is
  begin
    callbacks.register (proc);
  end register;
  procedure deregister (proc : callback) is
  begin
    callbacks.deregister (proc);
  end deregister;
  procedure call_callbacks (with_param : type) is
  begin
    callbacks.call_registered_procedures (with_param);
  end call_callbacks;
  procedure remove (from : in out collections.collection; proc : callback) is
    loc : natural;
  begin
    loc := collections.location (from, proc);
    collections.remove (from, loc);
  end remove;
end domain.type_callback;
         -->
    <xsl:variable name="t">
      <xsl:value-of select="../name"/>.<xsl:value-of select="name"/>
    </xsl:variable>

    <xsl:text>package body </xsl:text>
    <xsl:value-of select="$t"/>
    <xsl:text>_Callback is&#10;</xsl:text>
    <xsl:text>  procedure Register (Proc : Callback) is&#10;</xsl:text>
    <xsl:text>  begin&#10;</xsl:text>
    <xsl:text>    Callbacks.Register (Proc);&#10;</xsl:text>
    <xsl:text>  end Register;&#10;</xsl:text>
    <xsl:text>  procedure Deregister (Proc : Callback) is&#10;</xsl:text>
    <xsl:text>  begin&#10;</xsl:text>
    <xsl:text>    Callbacks.Deregister (Proc);&#10;</xsl:text>
    <xsl:text>  end Deregister;&#10;</xsl:text>
    <xsl:text>  procedure Call_Callbacks (With_Param : </xsl:text>
    <xsl:value-of select="$t"/>
    <xsl:text>) is&#10;</xsl:text>
    <xsl:text>  begin&#10;</xsl:text>
    <xsl:text>    Callbacks.Call_Registered_Procedures (With_Param);&#10;</xsl:text>
    <xsl:text>  end Call_Callbacks;&#10;</xsl:text>
    <xsl:text>  procedure Remove (From : in out Collections.Collection; Proc : Callback) is&#10;</xsl:text>
    <xsl:text>    Loc : Natural;&#10;</xsl:text>
    <xsl:text>  begin&#10;</xsl:text>
    <xsl:text>    Loc := Collections.Location (From, Proc);&#10;</xsl:text>
    <xsl:text>    Collections.Remove (From, Loc);&#10;</xsl:text>
    <xsl:text>  end Remove;&#10;</xsl:text>
    <xsl:text>end </xsl:text>
    <xsl:value-of select="$t"/>
    <xsl:text>_Callback;&#10;</xsl:text>

  </xsl:template>

  <xsl:template match="*" mode="callback-body"/>


</xsl:stylesheet>
