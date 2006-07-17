--  Copyright (C) Simon Wright <simon@pushface.org>

--  This unit is free software; you can redistribute it and/or modify
--  it as you wish. This unit is distributed in the hope that it will
--  be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  This package supports project events where instance events have
--  higher priority than class events..
--
--  This is ColdFrame's default implementation.

--  $RCSfile: coldframe-project-events-instance_priority.ads,v $
--  $Revision: fbc368af748a $
--  $Date: 2006/07/17 04:44:54 $
--  $Author: simonjwright $

with ColdFrame.Events_G.Instance_Priority_G;
with ColdFrame.Project.Events;
with ColdFrame.Project.Held_Events.Signature;

package ColdFrame.Project.Events.Instance_Priority
is new Project.Events.Instance_Priority_G
  (Held_Events => Held_Events.Signature);
