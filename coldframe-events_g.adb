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
--  $Revision: 7f8de3dc1e88 $
--  $Date: 2002/09/15 10:31:13 $
--  $Author: simon $

with Ada.Exceptions;
with ColdFrame.Exceptions;

package body ColdFrame.Events_G is


   procedure Instance_Is_Deleted
     (For_The_Event : access Instance_Event_Base'Class) is
   begin
      if For_The_Event.Instance_Deleted then
         Ada.Exceptions.Raise_Exception (Exceptions.Use_Error'Identity,
                                         "instance already marked as deleted");
      else
         For_The_Event.Instance_Deleted := True;
      end if;
   end Instance_Is_Deleted;


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


   procedure Wait_Until_Idle (The_Queue : access Event_Queue_Base;
                              Ignoring_Timers : Boolean := False) is
      pragma Warnings (Off, The_Queue);
      pragma Warnings (Off, Ignoring_Timers);
   begin
      Ada.Exceptions.Raise_Exception
        (Exceptions.Use_Error'Identity,
         "Wait_Until_Idle only legal with Test event queue");
   end Wait_Until_Idle;


   procedure Tear_Down (The_Queue : in out Event_Queue_P) is
      procedure Delete
      is new Ada.Unchecked_Deallocation (Event_Queue_Base'Class,
                                         Event_Queue_P);
   begin
      if The_Queue /= null then
         Tear_Down (The_Queue.all);
         Delete (The_Queue);
      end if;
   end Tear_Down;


   procedure Finalize (The_Terminator : in out Instance_Terminator) is
   begin
      if The_Terminator.For_The_Instance.Events_Posted_On /= null then
         --  Some events have been posted on a Queue
         Invalidate_Events
           (On => The_Terminator.For_The_Instance.Events_Posted_On,
            For_The_Instance => The_Terminator.For_The_Instance);
      end if;
   end Finalize;


   procedure Invalidate_Events
     (On : access Event_Queue_Base;
      For_The_Instance : access Instance_Base'Class) is
      pragma Warnings (Off, On);
      pragma Warnings (Off, For_The_Instance);
   begin
      raise Program_Error;
   end Invalidate_Events;


   procedure Tear_Down (The_Queue : in out Event_Queue_Base) is
      pragma Warnings (Off, The_Queue);
   begin
      Ada.Exceptions.Raise_Exception
        (Exceptions.Use_Error'Identity,
         "Tear_Down only legal with Test event queue");
   end Tear_Down;


   procedure Start_Queue (The_Queue : access Event_Queue_Base) is
      pragma Warnings (Off, The_Queue);
   begin
      raise Program_Error;
   end Start_Queue;


   procedure Add_Posted_Event (On : access Event_Queue_Base) is
      pragma Warnings (Off, On);
   begin
      null;
   end Add_Posted_Event;


   procedure Remove_Posted_Event (On : access Event_Queue_Base) is
      pragma Warnings (Off, On);
   begin
      null;
   end Remove_Posted_Event;


   procedure Add_Held_Event (On : access Event_Queue_Base) is
      pragma Warnings (Off, On);
   begin
      null;
   end Add_Held_Event;


   procedure Remove_Held_Event (On : access Event_Queue_Base) is
      pragma Warnings (Off, On);
   begin
      null;
   end Remove_Held_Event;


   procedure Add_Timer_Event (On : access Event_Queue_Base) is
      pragma Warnings (Off, On);
   begin
      null;
   end Add_Timer_Event;


   procedure Remove_Timer_Event (On : access Event_Queue_Base) is
      pragma Warnings (Off, On);
   begin
      null;
   end Remove_Timer_Event;


   procedure Log_Retraction (The_Event : Event_P;
                             On : access Event_Queue_Base) is
      pragma Warnings (Off, The_Event);
      pragma Warnings (Off, On);
   begin
      null;
   end Log_Retraction;


   procedure Log_Pre_Dispatch (The_Event : Event_P;
                               On : access Event_Queue_Base) is
      pragma Warnings (Off, The_Event);
      pragma Warnings (Off, On);
   begin
      null;
   end Log_Pre_Dispatch;


   procedure Log_Post_Dispatch (The_Event : Event_P;
                                On : access Event_Queue_Base) is
      pragma Warnings (Off, The_Event);
      pragma Warnings (Off, On);
   begin
      null;
   end Log_Post_Dispatch;


   procedure Locker (The_Queue : access Event_Queue_Base) is
      pragma Warnings (Off, The_Queue);
   begin
      raise Program_Error;
   end Locker;


   procedure Unlocker (The_Queue : access Event_Queue_Base) is
      pragma Warnings (Off, The_Queue);
   begin
      raise Program_Error;
   end Unlocker;


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


   procedure Finalize (The_Terminator : in out Timer_Terminator) is
   begin

      --  XXX is there a race condition here?

      if The_Terminator.For_The_Timer.The_Entry /= null then

         --  The Timer is set. Tell the timer event that the timer has
         --  been deleted.
         The_Terminator.For_The_Timer.The_Entry.The_Timer := null;

         --  Invalidate the held event.
         The_Terminator.For_The_Timer.The_Entry.The_Event.Invalidated := True;

         --  Decrement the count of timer events
         Remove_Timer_Event (The_Terminator.For_The_Timer.The_Entry.On);

      end if;

   end Finalize;


   procedure Initialize (The_Lock : in out Lock) is
   begin
      Locker (The_Lock.The_Queue);
   end Initialize;


   procedure Finalize (The_Lock : in out Lock) is
   begin

      if not The_Lock.Finalized then

         The_Lock.Finalized := True;
         Unlocker (The_Lock.The_Queue);

      end if;

   end Finalize;


end ColdFrame.Events_G;
