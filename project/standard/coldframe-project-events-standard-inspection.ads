--  Copyright (C) Simon Wright <simon@pushface.org>

--  This unit is free software; you can redistribute it and/or modify
--  it as you wish. This unit is distributed in the hope that it will
--  be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  This package supports testing of code involving events.
--
--  This is ColdFrame's default implementation.

with ColdFrame.Events_G.Standard_G.Inspection_G;
with ColdFrame.Project.Events.Standard;
with ColdFrame.Project.Held_Events.Inspection_Signature;

package ColdFrame.Project.Events.Standard.Inspection
is new Events.Standard.Inspection_G
  (Held_Events_Inspection => Project.Held_Events.Inspection_Signature);
