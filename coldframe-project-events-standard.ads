--  Copyright (C) Simon Wright <simon@pushface.org>

--  This unit is free software; you can redistribute it and/or modify
--  it as you wish. This unit is distributed in the hope that it will
--  be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  This package supports standard project events.
--
--  This is ColdFrame's default implementation.

--  $RCSfile: coldframe-project-events-standard.ads,v $
--  $Revision: ad7bfbd362d8 $
--  $Date: 2003/03/09 16:02:01 $
--  $Author: simon $

with ColdFrame.Events_G.Standard_G;
with ColdFrame.Project.Events;
with ColdFrame.Project.Held_Event_Support;

package ColdFrame.Project.Events.Standard
is new Project.Events.Standard_G (Held_Events => Held_Event_Support.Signature);
