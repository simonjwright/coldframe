--  This package is public-domain software; you can redistribute it
--  and/or modify it as you wish. This package is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE.

--  ColdFrame expects this package to exist to support event
--  management.
--
--  This is ColdFrame's default implementation.

--  $RCSfile: coldframe-project-events.ads,v $
--  $Revision: 281d11e491da $
--  $Date: 2002/07/27 13:05:23 $
--  $Author: simon $

with ColdFrame.Events_G;
with ColdFrame.Project.Global_Storage_Pool;
with ColdFrame.Project.Event_Support;

package ColdFrame.Project.Events is new ColdFrame.Events_G
  (Time => ColdFrame.Project.Event_Support.Signature,
   Event_Storage => ColdFrame.Project.Global_Storage_Pool.Pool);
