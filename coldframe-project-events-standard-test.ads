--  This package is public-domain software; you can redistribute it
--  and/or modify it as you wish. This package is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE.

--  This package supports unit testing of code involving events.
--
--  This is ColdFrame's default implementation.

--  $RCSfile: coldframe-project-events-standard-test.ads,v $
--  $Revision: b0e71732bd8c $
--  $Date: 2002/09/13 19:49:49 $
--  $Author: simon $

with ColdFrame.Events_G.Test_G;
with ColdFrame.Project.Events.Standard;

package ColdFrame.Project.Events.Standard.Test
is new Events.Test_G (Standard_Queue => Standard.Event_Queue_Base);
