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

--  $RCSfile: coldframe-events_g-test_g.adb,v $
--  $Revision: fde6fd75a1a0 $
--  $Date: 2014/04/05 13:21:13 $
--  $Author: simonjwright $

with Ada.Exceptions;
with ColdFrame.Exceptions;

package body ColdFrame.Events_G.Test_G is


   --  We use an extension of Event_Queue_Base so as to achieve
   --  dispatching depending on the Event_Queue type, even though we
   --  don't need the Event_Queue itself for either Timer
   --  operation. Remember that Timers are not tagged.


   function Is_Set (The_Timer : Timer;
                    On : not null access Event_Queue_Base) return Boolean is
      pragma Warnings (Off, On);
   begin
      return The_Timer.The_Entry /= null;
   end Is_Set;


   function Expires_At
     (The_Timer : Timer;
      On : not null access Event_Queue_Base) return Time.Time is
      pragma Warnings (Off, On);
   begin
      if The_Timer.The_Entry = null then
         Ada.Exceptions.Raise_Exception
           (Exceptions.Use_Error'Identity,
            "attempt to find expiry time of a timer that wasn't set");
      end if;
      return Held_Event (The_Timer.The_Entry.all).Time_To_Fire;
   end Expires_At;


   procedure Wait_Until_Idle (The_Queue : not null access Event_Queue_Base;
                              Ignoring_Timers : Boolean := False) is
   begin
      --  This conversion shouldn't (IMO) be necessary. GNAT
      --  [EC06-001] raised.
      if not Events_G.Event_Queue_Base (The_Queue.all).Started then
         Ada.Exceptions.Raise_Exception
           (ColdFrame.Exceptions.Use_Error'Identity,
            "Wait_Until_Idle called on event queue that wasn't started");
      end if;
      if Ignoring_Timers then
         The_Queue.The_Event_Count.Wait_Until_No_Posted_Events;
      else
         The_Queue.The_Event_Count.Wait_Until_Idle;
      end if;
   end Wait_Until_Idle;


   procedure Note_Addition_Of_Posted_Event
     (On : not null access Event_Queue_Base) is
   begin
      On.The_Event_Count.Add_Posted_Event;
   end Note_Addition_Of_Posted_Event;


   procedure Note_Removal_Of_Posted_Event
     (On : not null access Event_Queue_Base) is
   begin
      On.The_Event_Count.Remove_Posted_Event;
   end Note_Removal_Of_Posted_Event;


   procedure Note_Addition_Of_Held_Event
     (On : not null access Event_Queue_Base) is
   begin
      On.The_Event_Count.Add_Held_Event;
   end Note_Addition_Of_Held_Event;


   procedure Note_Removal_Of_Held_Event
     (On : not null access Event_Queue_Base) is
   begin
      On.The_Event_Count.Remove_Held_Event;
   end Note_Removal_Of_Held_Event;


   procedure Note_Addition_Of_Timer_Event
     (On : not null access Event_Queue_Base) is
   begin
      On.The_Event_Count.Add_Timer_Event;
   end Note_Addition_Of_Timer_Event;


   procedure Note_Removal_Of_Timer_Event
     (On : not null access Event_Queue_Base) is
   begin
      On.The_Event_Count.Remove_Timer_Event;
   end Note_Removal_Of_Timer_Event;


   protected body Event_Count is

      entry Wait_Until_Idle
      when Posted_Events = 0
        and then Held_Events = 0
        and then Timed_Events = 0 is
      begin
         null;
      end Wait_Until_Idle;

      entry Wait_Until_No_Posted_Events
      when Posted_Events = 0 is
      begin
         null;
      end Wait_Until_No_Posted_Events;

      procedure Add_Posted_Event is
      begin
         Posted_Events := Posted_Events + 1;
      end Add_Posted_Event;

      procedure Remove_Posted_Event is
      begin
         Posted_Events := Posted_Events - 1;
      end Remove_Posted_Event;

      procedure Add_Held_Event is
      begin
         Held_Events := Held_Events + 1;
      end Add_Held_Event;

      procedure Remove_Held_Event is
      begin
         Held_Events := Held_Events - 1;
      end Remove_Held_Event;

      procedure Add_Timer_Event is
      begin
         Timed_Events := Timed_Events + 1;
      end Add_Timer_Event;

      procedure Remove_Timer_Event is
      begin
         Timed_Events := Timed_Events - 1;
      end Remove_Timer_Event;

   end Event_Count;


end ColdFrame.Events_G.Test_G;
