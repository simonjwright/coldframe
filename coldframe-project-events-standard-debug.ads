--  This package is public-domain software; you can redistribute it
--  and/or modify it as you wish. This package is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE.

--  This package supports debugging of code involving events.
--
--  This is ColdFrame's default implementation.

--  $RCSfile: coldframe-project-events-standard-debug.ads,v $
--  $Revision: de928f3543dc $
--  $Date: 2002/08/17 15:19:41 $
--  $Author: simon $

with ColdFrame.Events_G.Debug_G;
with ColdFrame.Project.Events.Standard;

package ColdFrame.Project.Events.Standard.Debug
is new Events.Debug_G (Standard_Queue => Standard.Event_Queue);
