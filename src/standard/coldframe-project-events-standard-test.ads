--  Copyright (C) Simon Wright <simon@pushface.org>

--  This unit is free software; you can redistribute it and/or modify
--  it as you wish. This unit is distributed in the hope that it will
--  be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  This package supports unit testing of code involving events.
--
--  This is ColdFrame's default implementation.

with ColdFrame.Events_G.Test_G;

package ColdFrame.Project.Events.Standard.Test
is new Events.Test_G (Standard_Queue => Standard.Event_Queue_Base);
