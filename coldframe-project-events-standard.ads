--  This package is public-domain software; you can redistribute it
--  and/or modify it as you wish. This package is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE.

--  This package supports standard project events.
--
--  This is ColdFrame's default implementation.

--  $RCSfile: coldframe-project-events-standard.ads,v $
--  $Revision: 281d11e491da $
--  $Date: 2002/07/27 13:05:23 $
--  $Author: simon $

with ColdFrame.Events_G.Standard_G;
with ColdFrame.Project.Events;

package ColdFrame.Project.Events.Standard
is new Project.Events.Standard_G;
