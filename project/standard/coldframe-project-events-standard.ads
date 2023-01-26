--  Copyright (C) Simon Wright <simon@pushface.org>

--  This unit is free software; you can redistribute it and/or modify
--  it as you wish. This unit is distributed in the hope that it will
--  be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  This package supports standard project events.
--
--  This is ColdFrame's default implementation.

with ColdFrame.Events_G.Standard_G;
with ColdFrame.Project.Held_Events.Signature;

package ColdFrame.Project.Events.Standard
is new Project.Events.Standard_G (Held_Events => Held_Events.Signature);
