--  Copyright (C) Simon Wright <simon@pushface.org>

--  This unit is free software; you can redistribute it and/or modify
--  it as you wish. This unit is distributed in the hope that it will
--  be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  This package supports unit testing of code involving events, with
--  tracing of events as they are processed.
--
--  This is ColdFrame's default implementation.

--  $RCSfile: coldframe-project-events-standard-test_trace.ads,v $
--  $Revision: 273216b04d56 $
--  $Date: 2005/10/01 13:54:19 $
--  $Author: simonjwright $

with ColdFrame.Events_G.Trace_G;
with ColdFrame.Project.Events.Standard.Test;

--  The previous revision had the instantiations in the other order,
--  which failed. This appears to be a compiler bug.

package ColdFrame.Project.Events.Standard.Test_Trace
is new Events.Trace_G (Standard_Queue => Test.Event_Queue_Base);
