--  Copyright (C) Simon Wright <simon@pushface.org>

--  This unit is free software; you can redistribute it and/or modify
--  it as you wish. This unit is distributed in the hope that it will
--  be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  This package supports unit testing of code involving events.
--
--  This is ColdFrame's default implementation.

--  $RCSfile: coldframe-project-events-standard-test_debug.ads,v $
--  $Revision: d4ba7bb4c119 $
--  $Date: 2003/03/16 12:11:56 $
--  $Author: simon $

with ColdFrame.Events_G.Test_G;
with ColdFrame.Project.Events.Standard.Debug;

package ColdFrame.Project.Events.Standard.Test_Debug
is new Events.Test_G (Standard_Queue => Debug.Event_Queue_Base);
