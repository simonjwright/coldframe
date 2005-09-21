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
--  $Revision: facf98450e1d $
--  $Date: 2005/09/21 20:51:33 $
--  $Author: simonjwright $

package body ColdFrame.Project.Held_Events.Inspection is

   procedure Merge (C : Time_Collections.Collection;
                    Into : in out Time_Collections.Collection);
   procedure Merge (C : Time_Collections.Collection;
                    Into : in out Time_Collections.Collection) is
      It : Abstract_Time_Containers.Iterator'Class
        := Time_Collections.New_Iterator (C);
      use Abstract_Time_Containers;
   begin
      while not Is_Done (It) loop
         Time_Collections.Append (Into, Current_Item (It));
         Next (It);
      end loop;
   end Merge;


   function Number_Of_At_Events (On : Queue) return Natural is
      Result : Natural := 0;
   begin
      for K in Times.Time_Kind loop
         Result := Result + Time_Collections.Length (On.Queues (K));
      end loop;
      return Result;
   end Number_Of_At_Events;


   function At_Event (On : Queue;
                      At_Index : Positive) return Events.Event_P is
      Merged : Time_Collections.Collection;
   begin
      for K in Times.Time_Kind loop
         Merge (On.Queues (K), Into => Merged);
      end loop;
      --  One would have expected to be able to return
      --  function-call.component, but GNAT (4.0.0) says it's
      --  ambiguous.
      declare
         TC : Time_Cell
           renames Time_Collections.Item_At (Merged, At_Index);
      begin
         return TC.Event;
      end;
   end At_Event;


   function When_At (On : Queue;
                     At_Index : Positive) return Times.Time is
      Merged : Time_Collections.Collection;
   begin
      for K in Times.Time_Kind loop
         Merge (On.Queues (K), Into => Merged);
      end loop;
      declare
         TC : Time_Cell
           renames Time_Collections.Item_At (Merged, At_Index);
      begin
         return TC.Time_To_Fire;
      end;
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
