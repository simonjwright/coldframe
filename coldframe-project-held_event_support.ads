--  Copyright (C) Simon Wright <simon@pushface.org>

--  This unit is free software; you can redistribute it and/or modify
--  it as you wish. This unit is distributed in the hope that it will
--  be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  This package provides support operations for project events.
--
--  This is ColdFrame's default implementation.

--  $RCSfile: coldframe-project-held_event_support.ads,v $
--  $Revision: c1e2d532a451 $
--  $Date: 2003/03/09 16:09:24 $
--  $Author: simon $

with ColdFrame.Events_G.Held_Event_Queue_Signature;
with ColdFrame.Project.Events;
with ColdFrame.Project.Held_Events;

package ColdFrame.Project.Held_Event_Support is

   package Signature is new ColdFrame.Project.Events.Held_Event_Queue_Signature
     (Queue => Held_Events.Queue,
      Is_Empty => Held_Events.Is_Empty,
      Next_Event_Time => Held_Events.Next_Event_Time,
      Pop => Held_Events.Pop,
      Add_At_Event => Held_Events.Add_At_Event,
      Add_After_Event => Held_Events.Add_After_Event,
      Start_Processing_After_Events
        => Held_Events.Start_Processing_After_Events,
      Invalidate_Events => Held_Events.Invalidate_Events,
      Tear_Down => Held_Events.Tear_Down);

end ColdFrame.Project.Held_Event_Support;
