--  Copyright (C) Simon Wright <simon@pushface.org>

--  This unit is free software; you can redistribute it and/or modify
--  it as you wish. This unit is distributed in the hope that it will
--  be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  This package provides support operations for inspecting project
--  events.
--
--  This is ColdFrame's default implementation.

package body ColdFrame.Project.Held_Events.Inspection is


   function Number_Of_At_Events (On : Queue) return Natural is
   begin
      return Natural (On.Initial_Queue.Length);
   end Number_Of_At_Events;


   function At_Event (On : Queue;
                      At_Index : Positive) return Events.Event_P is
   begin
      return On.Initial_Queue.Element (At_Index).Event;
   end At_Event;


   function When_At (On : Queue;
                     At_Index : Positive) return Times.Time is
   begin
      return On.Initial_Queue.Element (At_Index).Time_To_Fire;
   end When_At;


   function Number_Of_After_Events (On : Queue) return Natural is
   begin
      return Natural (On.Duration_Queue.Length);
   end Number_Of_After_Events;


   function After_Event (On : Queue;
                         At_Index : Positive) return Events.Event_P is
   begin
      return On.Duration_Queue.Element (At_Index).Event;
   end After_Event;


   function How_Long_After (On : Queue;
                            At_Index : Positive) return Duration is
   begin
      return On.Duration_Queue.Element (At_Index).Delay_To_Fire;
   end How_Long_After;


end ColdFrame.Project.Held_Events.Inspection;
