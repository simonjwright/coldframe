--  Copyright (C) Simon Wright <simon@pushface.org>

--  This unit is free software; you can redistribute it and/or modify
--  it as you wish. This unit is distributed in the hope that it will
--  be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  ColdFrame expects this package to exist to support event
--  management.
--
--  This is ColdFrame's default implementation.

--  $RCSfile: coldframe-project-events.ads,v $
--  $Revision: 557c6c64e6a6 $
--  $Date: 2003/07/24 21:04:15 $
--  $Author: simon $

with ColdFrame.Events_G;
with ColdFrame.Project.Storage_Pools;
with ColdFrame.Project.Event_Support;
with ColdFrame.Project.Logging_Support;

package ColdFrame.Project.Events is new ColdFrame.Events_G
  (Logging => Logging_Support.Implementation,
   Time => ColdFrame.Project.Event_Support.Signature,
   Event_Storage => ColdFrame.Project.Storage_Pools.Pool);
