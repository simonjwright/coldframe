--  This package is public-domain software; you can redistribute it
--  and/or modify it as you wish. This package is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE.

--  ColdFrame expects this package to exist to provide support for
--  initial state creation events.
--
--  This is ColdFrame's default implementation.

--  $RCSfile: coldframe-project-events-creation.ads,v $
--  $Revision: 281d11e491da $
--  $Date: 2002/07/27 13:05:23 $
--  $Author: simon $

with ColdFrame.Events_G.Creation_G;
with ColdFrame.Project.Events;
package ColdFrame.Project.Events.Creation
is new ColdFrame.Project.Events.Creation_G;
