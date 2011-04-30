--  Copyright (C) Simon Wright <simon@pushface.org>

--  This unit is free software; you can redistribute it and/or modify
--  it as you wish. This unit is distributed in the hope that it will
--  be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  This package provides support operations for inspecting project
--  events.
--
--  This is ColdFrame's default implementation.

--  $RCSfile$
--  $Revision$
--  $Date$
--  $Author$

package ColdFrame.Project.Held_Events.Inspection is

   --  Types and Operations to support
   --  Held_Event_Queue_Signature.Inspection_Signature.

   function Number_Of_At_Events (On : Queue) return Natural;

   function At_Event (On : Queue;
                      At_Index : Positive) return Events.Event_P;

   function When_At (On : Queue;
                     At_Index : Positive) return Times.Time;


   function Number_Of_After_Events (On : Queue) return Natural;

   function After_Event (On : Queue;
                         At_Index : Positive) return Events.Event_P;

   function How_Long_After (On : Queue;
                            At_Index : Positive) return Duration;

   --  We can't actually instantiate the signature here, which would
   --  save a unit, because that would be a premature use of Queue.

end ColdFrame.Project.Held_Events.Inspection;
