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

--  $RCSfile: coldframe-events_g.adb,v $
--  $Revision: d42456779d35 $
--  $Date: 2003/09/02 18:57:03 $
--  $Author: simon $

with Ada.Exceptions;
with ColdFrame.Exceptions;

package body ColdFrame.Events_G is


   procedure Finalize (The_Lock : in out Lock) is
   begin

      if not The_Lock.Finalized then

         The_Lock.Finalized := True;
         Unlocker (The_Lock.The_Queue);

      end if;

   end Finalize;


   procedure Finalize (The_Instance : access Instance_Base'Class) is
   begin
      if The_Instance.Events_Posted_On /= null then
         --  Some events have been posted on a Queue
         Invalidate_Events
           (On => The_Instance.Events_Posted_On,
            For_The_Instance => The_Instance);
      end if;
   end Finalize;


   procedure Finalize (The_Timer : in out Timer) is
   begin
      if The_Timer.The_Entry /= null then

         --  The Timer is set. Tell the timer event that the timer has
         --  been deleted.
         Timer_Event (The_Timer.The_Entry.all).The_Timer := null;

         --  Invalidate the held event.
         Timer_Event (The_Timer.The_Entry.all).The_Event.Invalidated := True;

      end if;
   end Finalize;


   procedure Handler (This : Timer_Event) is
      The_Event : Event_P := This.The_Event;
   begin

      --  First, provided that the Timer (still) exists, indicate to
      --  it that it's now unset.
      if This.The_Timer /= null then
         This.The_Timer.The_Entry := null;
      end if;

      --  Handle the held event, unless it's been invalidated.
      if not The_Event.Invalidated then
         Log_Pre_Dispatch (The_Event => The_Event, On => This.On);
         Handler (The_Event.all);   --  XXX what about exceptions here?
         Log_Post_Dispatch (The_Event => The_Event, On => This.On);
      end if;

      --  Free the referenced memory (the outer dispatcher will free
      --  This)
      Delete (The_Event);

   end Handler;


   procedure Initialize (The_Lock : in out Lock) is
   begin
      Locker (The_Lock.The_Queue);
   end Initialize;


   procedure Instance_Is_Deleted
     (For_The_Event : access Instance_Event_Base'Class) is
   begin
      For_The_Event.Instance_Deleted := True;
   end Instance_Is_Deleted;


   procedure Invalidate (The_Event : access Event_Base;
                         If_For_Instance : Instance_Base_P) is
      pragma Warnings (Off, The_Event);
      pragma Warnings (Off, If_For_Instance);
   begin
      null;
   end Invalidate;


   procedure Invalidate (The_Event : access Instance_Event_Base;
                         If_For_Instance : Instance_Base_P) is
   begin
      if Instance_Base_P (The_Event.For_The_Instance) = If_For_Instance then
         The_Event.Invalidated := True;
      end if;
   end Invalidate;


   procedure Invalidate (The_Event : access Timer_Event;
                         If_For_Instance : Instance_Base_P) is
   begin
      Invalidate (The_Event.The_Event, If_For_Instance);
   end Invalidate;


   procedure Invalidate_Events
     (On : access Event_Queue_Base;
      For_The_Instance : access Instance_Base'Class) is
      pragma Warnings (Off, On);
      pragma Warnings (Off, For_The_Instance);
   begin
      raise Program_Error;
   end Invalidate_Events;


   procedure Locker (The_Queue : access Event_Queue_Base) is
      pragma Warnings (Off, The_Queue);
   begin
      raise Program_Error;
   end Locker;


   procedure Log_Post_Dispatch (The_Event : Event_P;
                                On : access Event_Queue_Base) is
      pragma Warnings (Off, The_Event);
      pragma Warnings (Off, On);
   begin
      null;
   end Log_Post_Dispatch;


   procedure Log_Pre_Dispatch (The_Event : Event_P;
                               On : access Event_Queue_Base) is
      pragma Warnings (Off, The_Event);
      pragma Warnings (Off, On);
   begin
      null;
   end Log_Pre_Dispatch;


   procedure Log_Retraction (The_Event : Event_P;
                             On : access Event_Queue_Base) is
      pragma Warnings (Off, The_Event);
      pragma Warnings (Off, On);
   begin
      null;
   end Log_Retraction;


   procedure Note_Addition_Of_Held_Event (On : access Event_Queue_Base) is
      pragma Warnings (Off, On);
   begin
      null;
   end Note_Addition_Of_Held_Event;


   procedure Note_Addition_Of_Posted_Event (On : access Event_Queue_Base) is
      pragma Warnings (Off, On);
   begin
      null;
   end Note_Addition_Of_Posted_Event;


   procedure Note_Addition_Of_Timer_Event (On : access Event_Queue_Base) is
      pragma Warnings (Off, On);
   begin
      null;
   end Note_Addition_Of_Timer_Event;


   procedure Note_Removal_Of_Held_Event (On : access Event_Queue_Base) is
      pragma Warnings (Off, On);
   begin
      null;
   end Note_Removal_Of_Held_Event;


   procedure Note_Removal_Of_Posted_Event (On : access Event_Queue_Base) is
      pragma Warnings (Off, On);
   begin
      null;
   end Note_Removal_Of_Posted_Event;


   procedure Note_Removal_Of_Timer_Event (On : access Event_Queue_Base) is
      pragma Warnings (Off, On);
   begin
      null;
   end Note_Removal_Of_Timer_Event;


   procedure Start (The_Queue : access Event_Queue_Base) is
      pragma Warnings (Off, The_Queue);
   begin
      if The_Queue.Started then
         Ada.Exceptions.Raise_Exception (Exceptions.Use_Error'Identity,
                                         "queue already started");
      else
         The_Queue.Started := True;
         Start_Queue (Event_Queue_P (The_Queue)); -- need to dispatch
      end if;
   end Start;


   procedure Start_Queue (The_Queue : access Event_Queue_Base) is
      pragma Warnings (Off, The_Queue);
   begin
      raise Program_Error;
   end Start_Queue;


   procedure Tear_Down (The_Queue : in out Event_Queue_Base) is
      pragma Warnings (Off, The_Queue);
   begin
      Ada.Exceptions.Raise_Exception
        (Exceptions.Use_Error'Identity,
         "Tear_Down only legal with Test event queue");
   end Tear_Down;


   procedure Tear_Down (The_Queue : in out Event_Queue_P) is
      procedure Delete
      is new Ada.Unchecked_Deallocation (Event_Queue_Base'Class,
                                         Event_Queue_P);
   begin
      if The_Queue /= null then
         if not The_Queue.Torn_Down then
            The_Queue.Torn_Down := True;
            Tear_Down (The_Queue.all);  -- dispatches to actual Tear_Down
         end if;
         The_Queue.Access_Count := The_Queue.Access_Count - 1;
         if The_Queue.Access_Count = 0 then
            Delete (The_Queue);
         else
            --  We have to clear this pointer (which is of course the
            --  user's) in case there's a cascade teardown and she
            --  calls us again.
            The_Queue := null;
         end if;
      end if;
   end Tear_Down;


   procedure Unlocker (The_Queue : access Event_Queue_Base) is
      pragma Warnings (Off, The_Queue);
   begin
      raise Program_Error;
   end Unlocker;


   procedure Wait_Until_Idle (The_Queue : access Event_Queue_Base;
                              Ignoring_Timers : Boolean := False) is
      pragma Warnings (Off, The_Queue);
      pragma Warnings (Off, Ignoring_Timers);
   begin
      Ada.Exceptions.Raise_Exception
        (Exceptions.Use_Error'Identity,
         "Wait_Until_Idle only legal with Test event queue");
   end Wait_Until_Idle;


end ColdFrame.Events_G;
