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
--  $Revision: 0342c7e00b63 $
--  $Date: 2007/10/27 12:40:37 $
--  $Author: simonjwright $

with ColdFrame.Event_Basis;
with ColdFrame.Events_G;
with ColdFrame.Project.Storage_Pools;
with ColdFrame.Project.Event_Support;

package ColdFrame.Project.Events is new ColdFrame.Events_G
  (Base_Event => Event_Basis.Event_Base,
   Time => ColdFrame.Project.Event_Support.Signature,
   Event_Storage => ColdFrame.Project.Storage_Pools.Pool);
