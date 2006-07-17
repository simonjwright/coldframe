--  Copyright (C) Simon Wright <simon@pushface.org>

--  This unit is free software; you can redistribute it and/or modify
--  it as you wish. This unit is distributed in the hope that it will
--  be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  This package supports unit testing of code involving events.
--
--  This is ColdFrame's default implementation.

--  $RCSfile: coldframe-project-events-instance_priority-test.ads,v $
--  $Revision: fbc368af748a $
--  $Date: 2006/07/17 04:44:54 $
--  $Author: simonjwright $

with ColdFrame.Events_G.Test_G;
with ColdFrame.Project.Events.Instance_Priority;

package ColdFrame.Project.Events.Instance_Priority.Test
is new Events.Test_G (Standard_Queue => Instance_Priority.Event_Queue_Base);
