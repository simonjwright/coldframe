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
--  $Revision: b720f64f3037 $
--  $Date: 2002/09/28 17:13:37 $
--  $Author: simon $

with ColdFrame.Events_G;
with ColdFrame.Project.Global_Storage_Pool;
with ColdFrame.Project.Event_Support;

package ColdFrame.Project.Events is new ColdFrame.Events_G
  (Time => ColdFrame.Project.Event_Support.Signature,
   Event_Storage => ColdFrame.Project.Global_Storage_Pool.Pool);
