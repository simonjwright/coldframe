--  This package is public-domain software; you can redistribute it
--  and/or modify it as you wish. This package is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE.

--  This package supports debug project events.
--
--  This is ColdFrame's default implementation.

--  $RCSfile: coldframe-project-events-standard-debug.ads,v $
--  $Revision: 281d11e491da $
--  $Date: 2002/07/27 13:05:23 $
--  $Author: simon $

with ColdFrame.Events_G.Debug_G;
with ColdFrame.Project.Events.Standard;

package ColdFrame.Project.Events.Standard.Debug
is new Events.Debug_G (Standard_Queue => Standard.Event_Queue);
