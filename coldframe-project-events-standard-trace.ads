--  Copyright (C) Simon Wright <simon@pushface.org>

--  This unit is free software; you can redistribute it and/or modify
--  it as you wish. This unit is distributed in the hope that it will
--  be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  This package supports tracing of code involving events.
--
--  This is ColdFrame's default implementation.

--  $RCSfile: coldframe-project-events-standard-trace.ads,v $
--  $Revision: b72a3ecb25af $
--  $Date: 2004/06/17 07:12:06 $
--  $Author: simon $

with ColdFrame.Events_G.Trace_G;
with ColdFrame.Project.Events.Standard;

package ColdFrame.Project.Events.Standard.Trace
is new Events.Trace_G (Standard_Queue => Standard.Event_Queue_Base);
