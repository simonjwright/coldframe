--  Copyright (C) Simon Wright <simon@pushface.org>

--  This package is free software; you can redistribute it and/or
--  modify it under the terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or
--  (at your option) any later version. This package is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE. See the GNU General Public License for more
--  details. You should have received a copy of the GNU General Public
--  License distributed with this package; see file COPYING.  If not,
--  write to the Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.

--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License.  This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.

--  $RCSfile: coldframe-events_g-standard_g-inspection_g.adb,v $
--  $Revision: 1da128db0fde $
--  $Date: 2006/10/31 06:40:40 $
--  $Author: simonjwright $

package body ColdFrame.Events_G.Standard_G.Inspection_G is

   package UPEQ renames Unbounded_Posted_Event_Queues;

   --  We need a way of obtaining the N'th Event_P in an unbounded
   --  posted event queue, but Queues don't have an Item_At operation.

   function Nth_Event
     (Q : Unbounded_Posted_Event_Queues.Queue;
      N : Positive) return Event_P;
   function Nth_Event
     (Q : Unbounded_Posted_Event_Queues.Queue;
      N : Positive) return Event_P is
      package APEC renames Abstract_Posted_Event_Containers;
      It : APEC.Iterator'Class := UPEQ.New_Iterator (Q);
   begin
      for C in 1 .. N - 1 loop
         APEC.Next (It);
      end loop;
      return APEC.Current_Item (It);
   end Nth_Event;


   --------------------------------
   --  Events posted to "self".  --
   --------------------------------

   function Number_Of_Self_Events (On : Event_Queue_P) return Natural is
      pragma Assert (On.all in Event_Queue_Base'Class,
                     "not a standard queue");
      Q : Event_Queue_Base renames Event_Queue_Base (On.all);
   begin
      if On.Started then
         raise Started;
      end if;
      return UPEQ.Length (Q.The_Self_Events);
   end Number_Of_Self_Events;


   function Self_Event (On : Event_Queue_P;
                        At_Index : Positive) return Event_P is
      pragma Assert (On.all in Event_Queue_Base'Class,
                     "not a standard queue");
      Q : Event_Queue_Base renames Event_Queue_Base (On.all);
   begin
      if On.Started then
         raise Started;
      end if;
      if At_Index > UPEQ.Length (Q.The_Self_Events) then
         raise Not_Found;
      end if;
      return Nth_Event (Q.The_Self_Events, At_Index);
   end Self_Event;


   -----------------------------------------
   --  Events posted to run immediately.  --
   -----------------------------------------

   function Number_Of_Now_Events (On : Event_Queue_P) return Natural is
      pragma Assert (On.all in Event_Queue_Base'Class,
                     "not a standard queue");
      Q : Event_Queue_Base renames Event_Queue_Base (On.all);
   begin
      if On.Started then
         raise Started;
      end if;
      return UPEQ.Length (Q.The_Instance_Events)
        + UPEQ.Length (Q.The_Class_Events);
   end Number_Of_Now_Events;


   function Now_Event (On : Event_Queue_P;
                       At_Index : Positive) return Event_P is
      pragma Assert (On.all in Event_Queue_Base'Class,
                     "not a standard queue");
      Q : Event_Queue_Base renames Event_Queue_Base (On.all);
      N : Positive := At_Index;
   begin
      if On.Started then
         raise Started;
      end if;
      if N <= UPEQ.Length (Q.The_Instance_Events) then
         return Nth_Event (Q.The_Instance_Events, N);
      end if;
      N := N - UPEQ.Length (Q.The_Instance_Events);
      if N <= UPEQ.Length (Q.The_Class_Events) then
         return Nth_Event (Q.The_Class_Events, N);
      end if;
      raise Not_Found;
   end Now_Event;


   -------------------------------------------
   --  Events posted to run after a delay.  --
   -------------------------------------------

   function Number_Of_After_Events (On : Event_Queue_P) return Natural is
      pragma Assert (On.all in Event_Queue_Base'Class,
                     "not a standard queue");
   begin
      if On.Started then
         raise Started;
      end if;
      return Held_Events_Inspection.Number_Of_After_Events
        (Standard_G.Event_Queue_Base (On.all).The_Held_Events);
   end Number_Of_After_Events;


   function After_Event (On : Event_Queue_P;
                         At_Index : Positive) return Event_P is
      pragma Assert (On.all in Event_Queue_Base'Class,
                     "not a standard queue");
   begin
      if On.Started then
         raise Started;
      end if;
      if At_Index > Number_Of_After_Events (On) then
         raise Not_Found;
      end if;
      return Held_Event
        (Held_Events_Inspection.After_Event
           (Standard_G.Event_Queue_Base (On.all).The_Held_Events,
            At_Index).all).The_Event;
   end After_Event;


   function How_Long_After (On : Event_Queue_P;
                            At_Index : Positive) return Duration is
      pragma Assert (On.all in Event_Queue_Base'Class,
                     "not a standard queue");
   begin
      if On.Started then
         raise Started;
      end if;
      if At_Index > Number_Of_After_Events (On) then
         raise Not_Found;
      end if;
      return Held_Events_Inspection.How_Long_After
        (Standard_G.Event_Queue_Base (On.all).The_Held_Events,
         At_Index);
   end How_Long_After;


   ---------------------------------------------
   --  Events posted to run at a later time.  --
   ---------------------------------------------

   function Number_Of_Later_Events (On : Event_Queue_P) return Natural is
      pragma Assert (On.all in Event_Queue_Base'Class,
                     "not a standard queue");
   begin
      if On.Started then
         raise Started;
      end if;
      return Held_Events_Inspection.Number_Of_At_Events
        (Standard_G.Event_Queue_Base (On.all).The_Held_Events);
   end Number_Of_Later_Events;


   function Later_Event (On : Event_Queue_P;
                         At_Index : Positive) return Event_P is
      pragma Assert (On.all in Event_Queue_Base'Class,
                     "not a standard queue");
   begin
      if On.Started then
         raise Started;
      end if;
      if At_Index > Number_Of_Later_Events (On) then
         raise Not_Found;
      end if;
      return Held_Event
        (Held_Events_Inspection.At_Event
           (Standard_G.Event_Queue_Base (On.all).The_Held_Events,
            At_Index).all).The_Event;
   end Later_Event;


   function When_Later (On : Event_Queue_P;
                        At_Index : Positive) return Time.Time is
      pragma Assert (On.all in Event_Queue_Base'Class,
                     "not a standard queue");
   begin
      if On.Started then
         raise Started;
      end if;
      if At_Index > Number_Of_Later_Events (On) then
         raise Not_Found;
      end if;
      return Held_Events_Inspection.When_At
        (Standard_G.Event_Queue_Base (On.all).The_Held_Events,
         At_Index);
   end When_Later;


   function Event_Of (The_Timer : Timer) return Event_P is
   begin
      if The_Timer.The_Entry = null then
         return null;
      end if;
      return Held_Event (The_Timer.The_Entry.all).The_Event;
   end Event_Of;


end ColdFrame.Events_G.Standard_G.Inspection_G;
