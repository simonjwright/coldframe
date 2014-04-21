--  Copyright (C) Simon Wright <simon@pushface.org>

--  This unit is free software; you can redistribute it and/or modify
--  it as you wish. This unit is distributed in the hope that it will
--  be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  This package supports unit testing of code involving events, where
--  the event code is monitored.
--
--  This is ColdFrame's default implementation.

--  $RCSfile: coldframe-project-events-monitoring-trace.ads,v $
--  $Revision: f6d9ce14c0aa $
--  $Date: 2014/04/21 15:48:31 $
--  $Author: simonjwright $

with ColdFrame.Events_G.Trace_G;
with ColdFrame.Project.Events.Monitoring;

package ColdFrame.Project.Events.Monitoring.Trace
is new Events.Trace_G (Standard_Queue => Monitoring.Event_Queue_Base);
