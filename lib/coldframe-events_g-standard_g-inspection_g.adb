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

package body ColdFrame.Events_G.Standard_G.Inspection_G is

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
      return Natural (Q.The_Self_Events.Length);
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
      if At_Index > Natural (Q.The_Self_Events.Length) then
         raise Not_Found;
      end if;
      return Q.The_Self_Events.Element (At_Index);
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
      return Natural (Q.The_Instance_Events.Length)
        + Natural (Q.The_Class_Events.Length);
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
      if N <= Natural (Q.The_Instance_Events.Length) then
         return Q.The_Instance_Events.Element (N);
      end if;
      N := N - Natural (Q.The_Instance_Events.Length);
      if N <= Natural (Q.The_Class_Events.Length) then
         return Q.The_Class_Events.Element (N);
      end if;
      raise Not_Found;
   end Now_Event;


   function Number_Of_Immediate_Class_Events
     (On : Event_Queue_P) return Natural is
      pragma Assert (On.all in Event_Queue_Base'Class,
                     "not a standard queue");
      Q : Event_Queue_Base renames Event_Queue_Base (On.all);
   begin
      if On.Started then
         raise Started;
      end if;
      return Natural (Q.The_Class_Events.Length);
   end Number_Of_Immediate_Class_Events;


   function Immediate_Class_Event (On : Event_Queue_P;
                                   At_Index : Positive) return Event_P is
      pragma Assert (On.all in Event_Queue_Base'Class,
                     "not a standard queue");
      Q : Event_Queue_Base renames Event_Queue_Base (On.all);
   begin
      if On.Started then
         raise Started;
      end if;
      if At_Index > Natural (Q.The_Class_Events.Length) then
         raise Not_Found;
      end if;
      return Q.The_Class_Events.Element (At_Index);
   end Immediate_Class_Event;


   function Number_Of_Immediate_Instance_Events
     (On : Event_Queue_P) return Natural is
      pragma Assert (On.all in Event_Queue_Base'Class,
                     "not a standard queue");
      Q : Event_Queue_Base renames Event_Queue_Base (On.all);
   begin
      if On.Started then
         raise Started;
      end if;
      return Natural (Q.The_Instance_Events.Length);
   end Number_Of_Immediate_Instance_Events;


   function Immediate_Instance_Event (On : Event_Queue_P;
                                      At_Index : Positive) return Event_P is
      pragma Assert (On.all in Event_Queue_Base'Class,
                     "not a standard queue");
      Q : Event_Queue_Base renames Event_Queue_Base (On.all);
   begin
      if On.Started then
         raise Started;
      end if;
      if At_Index > Natural (Q.The_Instance_Events.Length) then
         raise Not_Found;
      end if;
      return Q.The_Instance_Events.Element (At_Index);
   end Immediate_Instance_Event;


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


   procedure Fire (The_Timer : in out Timer) is
      The_Held_Event : Event_P renames The_Timer.The_Entry;
      pragma Assert (The_Held_Event /= null,
                     "no held event on Timer");
      The_Event : Event_P renames Held_Event (The_Held_Event.all).The_Event;
      pragma Assert (not The_Event.Invalidated,
                     "the held event has been invalidated");
   begin
      --  Unset the Timer.
      The_Timer.The_Entry := null;
      Handler (The_Event.all);
      --  Don't delete the events; they'll get deleted during teardown
      --  (and deleting them here will break teardown without a lot
      --  more work).
   end Fire;


end ColdFrame.Events_G.Standard_G.Inspection_G;
