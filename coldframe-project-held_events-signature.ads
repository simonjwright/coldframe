--  Copyright (C) Simon Wright <simon@pushface.org>

--  This unit is free software; you can redistribute it and/or modify
--  it as you wish. This unit is distributed in the hope that it will
--  be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  This package provides support operations for project events.
--
--  This is ColdFrame's default implementation.

--  $RCSfile: coldframe-project-held_events-signature.ads,v $
--  $Revision: 7778b3e41931 $
--  $Date: 2004/01/16 06:54:58 $
--  $Author: simon $

with ColdFrame.Events_G.Held_Event_Queue_Signature;
with ColdFrame.Project.Events;

--  We can't instantiate Held_Event_Queue_Signature inside
--  Held_Events, because it would be a premature use of the private
--  type Queue.
package ColdFrame.Project.Held_Events.Signature
is new ColdFrame.Project.Events.Held_Event_Queue_Signature
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
