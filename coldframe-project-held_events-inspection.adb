--  Copyright (C) Simon Wright <simon@pushface.org>

--  This unit is free software; you can redistribute it and/or modify
--  it as you wish. This unit is distributed in the hope that it will
--  be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  This package provides support operations for inspecting project
--  events.
--
--  This is ColdFrame's default implementation.

--  $RCSfile: coldframe-project-held_events-inspection.adb,v $
--  $Revision: 63f8a818a534 $
--  $Date: 2006/03/03 22:08:25 $
--  $Author: simonjwright $

package body ColdFrame.Project.Held_Events.Inspection is


   function Number_Of_At_Events (On : Queue) return Natural is
   begin
      return Initial_Time_Collections.Length (On.Initial_Queue);
   end Number_Of_At_Events;


   function At_Event (On : Queue;
                      At_Index : Positive) return Events.Event_P is
      TC : Time_Cell renames
        Initial_Time_Collections.Item_At (On.Initial_Queue, At_Index);
   begin
      return TC.Event;
   end At_Event;


   function When_At (On : Queue;
                     At_Index : Positive) return Times.Time is
      TC : Time_Cell renames
        Initial_Time_Collections.Item_At (On.Initial_Queue, At_Index);
   begin
      return TC.Time_To_Fire;
   end When_At;


   function Number_Of_After_Events (On : Queue) return Natural is
   begin
      return Duration_Collections.Length (On.Duration_Queue);
   end Number_Of_After_Events;


   function After_Event (On : Queue;
                         At_Index : Positive) return Events.Event_P is
      DC : Duration_Cell
        renames Duration_Collections.Item_At (On.Duration_Queue, At_Index);
   begin
      return DC.Event;
   end After_Event;


   function How_Long_After (On : Queue;
                            At_Index : Positive) return Duration is
      DC : Duration_Cell
        renames Duration_Collections.Item_At (On.Duration_Queue, At_Index);
   begin
      return DC.Delay_To_Fire;
   end How_Long_After;


end ColdFrame.Project.Held_Events.Inspection;
