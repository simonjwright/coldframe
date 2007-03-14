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
--  $Revision: 010a1b946720 $
--  $Date: 2007/03/14 20:24:08 $
--  $Author: simonjwright $

with Ada.Exceptions;
with Ada.Tags;
with ColdFrame.Exceptions;

package body ColdFrame.Events_G is


   procedure Add_Reference (To : Event_Queue_P) is
   begin
      if To /= null then
         To.Access_Count := To.Access_Count + 1;
      end if;
   end Add_Reference;


   procedure Check_Deletable (The_Instance : access Instance_Base) is
   begin
      if The_Instance.In_Handler then
         Ada.Exceptions.Raise_Exception
           (ColdFrame.Exceptions.Use_Error'Identity,
            "deleting instance from event handler");
      end if;
   end Check_Deletable;


   function Copy
     (The_Queue : Event_Queue_P) return Event_Queue_P is
   begin
      return The_Queue;
   end Copy;


   function Expires_At (The_Timer : Timer;
                        On : access Event_Queue_Base) return Time.Time is
      pragma Unreferenced (The_Timer);
      pragma Unreferenced (On);
      Dummy : Time.Time (Kind => Time.Real_Time);
   begin
      Ada.Exceptions.Raise_Exception
        (Exceptions.Use_Error'Identity,
         "Expires_At only legal with Test event queue");
      return Dummy;
   end Expires_At;


   procedure Finalize (The_Instance : access Instance_Base'Class) is
   begin
      if The_Instance.Events_Posted_On /= null then
         --  Some events have been posted on a Queue
         Invalidate_Events
           (On => The_Instance.Events_Posted_On,
            For_The_Instance => The_Instance);
      end if;
   end Finalize;


   procedure Finalize (The_Lock : in out Lock) is
   begin
      if not The_Lock.Finalized then
         The_Lock.Finalized := True;
         Unlocker (The_Lock.The_Queue);
      end if;
   end Finalize;


   procedure Finalize (The_Timer : in out Timer) is
   begin
      if The_Timer.The_Entry /= null then

         --  The Timer is set. Tell the timer event that the timer has
         --  been deleted.
         Held_Event (The_Timer.The_Entry.all).The_Timer := null;

         --  Invalidate the held event.
         Held_Event (The_Timer.The_Entry.all).The_Event.Invalidated := True;

         The_Timer.The_Entry := null;

      end if;
   end Finalize;


   procedure Finalize (The_Checker : in out Timer_Checker) is
      The_Timer : Timer renames The_Checker.For_The_Timer.all;
   begin
      if The_Timer.The_Entry /= null then

         --  Tell the user there's been an error (maybe the Timer was
         --  declared on the stack?)
         --
         --  Because this occurs during finalization, it's not really
         --  sensible to raise an exception.
         Logging.Log
           (Severity => Logging.Error,
            Message => "A Timer has been destroyed while holding a " &
              Ada.Tags.Expanded_Name
              (Held_Event (The_Timer.The_Entry.all).The_Event'Tag) &
              ", may have been declared on the stack");

         Finalize (The_Timer);

      end if;
   end Finalize;


   procedure Handler (This : Held_Event) is
      The_Event : Event_P := This.The_Event;
   begin

      --  First, provided that the Timer (still) exists, indicate to
      --  it that it's now unset.
      if This.The_Timer /= null then
         This.The_Timer.The_Entry := null;
      end if;

      --  Handle the held event, unless it's been invalidated.
      if not The_Event.Invalidated then
         Log (The_Event, Event_Basis.Posting);
         Log (The_Event, Event_Basis.Dispatching);
         Log_Pre_Dispatch (The_Event => The_Event, On => This.On);
         Start_Handling (The_Event);
         begin
            Handler (The_Event.all);
         exception
            when Ex : Exceptions.Cant_Happen =>
               Logging.Log
                 (Severity => Logging.Error,
                  Message => Ada.Exceptions.Exception_Message (Ex));
            when Ex : others =>
               Logging.Log
                 (Severity => Logging.Error,
                  Message =>
                    Ada.Exceptions.Exception_Information (Ex) &
                    " in Held_Event handler (event " &
                    Ada.Tags.Expanded_Name (The_Event.all'Tag) &
                    ")");
         end;
         Stop_Handling (The_Event);
         Log_Post_Dispatch (The_Event => The_Event, On => This.On);
         Log (The_Event, Event_Basis.Finishing);
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
      pragma Unreferenced (The_Event);
      pragma Unreferenced (If_For_Instance);
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


   procedure Invalidate (The_Event : access Held_Event;
                         If_For_Instance : Instance_Base_P) is
   begin
      Invalidate (The_Event.The_Event, If_For_Instance);
   end Invalidate;


   procedure Invalidate_Events
     (On : access Event_Queue_Base;
      For_The_Instance : access Instance_Base'Class) is
      pragma Unreferenced (On);
      pragma Unreferenced (For_The_Instance);
   begin
      raise Program_Error;
   end Invalidate_Events;


   function Is_Set (The_Timer : Timer;
                    On : access Event_Queue_Base) return Boolean is
      pragma Unreferenced (The_Timer);
      pragma Unreferenced (On);
   begin
      Ada.Exceptions.Raise_Exception
        (Exceptions.Use_Error'Identity,
         "Is_Set only legal with Test event queue");
      return False;
   end Is_Set;


   procedure Locker (The_Queue : access Event_Queue_Base) is
      pragma Unreferenced (The_Queue);
   begin
      raise Program_Error;
   end Locker;


   procedure Log_Post_Dispatch (The_Event : Event_P;
                                On : access Event_Queue_Base) is
      pragma Unreferenced (The_Event);
      pragma Unreferenced (On);
   begin
      null;
   end Log_Post_Dispatch;


   procedure Log_Pre_Dispatch (The_Event : Event_P;
                               On : access Event_Queue_Base) is
      pragma Unreferenced (The_Event);
      pragma Unreferenced (On);
   begin
      null;
   end Log_Pre_Dispatch;


   procedure Log_Retraction (The_Event : Event_P;
                             On : access Event_Queue_Base) is
      pragma Unreferenced (The_Event);
      pragma Unreferenced (On);
   begin
      null;
   end Log_Retraction;


   procedure Mark_Deletable (The_Instance : access Instance_Base) is
   begin
      The_Instance.In_Handler := False;
   end Mark_Deletable;


   procedure Note_Addition_Of_Held_Event (On : access Event_Queue_Base) is
      pragma Unreferenced (On);
   begin
      null;
   end Note_Addition_Of_Held_Event;


   procedure Note_Addition_Of_Posted_Event (On : access Event_Queue_Base) is
      pragma Unreferenced (On);
   begin
      null;
   end Note_Addition_Of_Posted_Event;


   procedure Note_Addition_Of_Timer_Event (On : access Event_Queue_Base) is
      pragma Unreferenced (On);
   begin
      null;
   end Note_Addition_Of_Timer_Event;


   procedure Note_Removal_Of_Held_Event (On : access Event_Queue_Base) is
      pragma Unreferenced (On);
   begin
      null;
   end Note_Removal_Of_Held_Event;


   procedure Note_Removal_Of_Posted_Event (On : access Event_Queue_Base) is
      pragma Unreferenced (On);
   begin
      null;
   end Note_Removal_Of_Posted_Event;


   procedure Note_Removal_Of_Timer_Event (On : access Event_Queue_Base) is
      pragma Unreferenced (On);
   begin
      null;
   end Note_Removal_Of_Timer_Event;


   procedure Start (The_Queue : access Event_Queue_Base) is
   begin
      if The_Queue.Started then
         Ada.Exceptions.Raise_Exception (Exceptions.Use_Error'Identity,
                                         "queue already started");
      else
         The_Queue.Started := True;
         Start_Queue (Event_Queue_P (The_Queue)); -- need to dispatch
      end if;
   end Start;


   procedure Start_Handling (The_Event : access Event_Base) is
      pragma Unreferenced (The_Event);
   begin
      null;
   end Start_Handling;


   procedure Start_Handling (The_Event : access Instance_Event_Base) is
   begin
      The_Event.For_The_Instance.In_Handler := True;
   end Start_Handling;


   procedure Stop_Handling (The_Event : access Event_Base) is
      pragma Unreferenced (The_Event);
   begin
      null;
   end Stop_Handling;


   procedure Stop_Handling (The_Event : access Instance_Event_Base) is
   begin
      if not The_Event.Instance_Deleted then
         The_Event.For_The_Instance.In_Handler := False;
      end if;
   end Stop_Handling;


   procedure Start_Queue (The_Queue : access Event_Queue_Base) is
      pragma Unreferenced (The_Queue);
   begin
      raise Program_Error;
   end Start_Queue;


   procedure Stop (The_Queue : in out Event_Queue_Base) is
      pragma Unreferenced (The_Queue);
   begin
      Ada.Exceptions.Raise_Exception
        (Exceptions.Use_Error'Identity,
         "Stop not implemented in concrete event queue");
   end Stop;


   procedure Stop (The_Queue : in out Event_Queue_P) is
   begin
      if The_Queue /= null then
         if not The_Queue.Stopped then
            The_Queue.Stopped := True;
            Stop (The_Queue.all);  -- dispatches to actual Stop
         end if;
      end if;
   end Stop;


   procedure Tear_Down (The_Event : access Event_Base) is
      pragma Unreferenced (The_Event);
   begin
      --  Default no-op.
      null;
   end Tear_Down;


   procedure Tear_Down (The_Queue : in out Event_Queue_Base) is
      pragma Unreferenced (The_Queue);
   begin
      Ada.Exceptions.Raise_Exception
        (Exceptions.Use_Error'Identity,
         "Tear_Down not implemented in concrete event queue");
   end Tear_Down;


   procedure Tear_Down (The_Queue : in out Event_Queue_P) is
      procedure Delete
      is new Ada.Unchecked_Deallocation (Event_Queue_Base'Class,
                                         Event_Queue_P);
   begin
      if The_Queue /= null then
         --  Decrement the use count (if there have actually been any uses).
         if The_Queue.Access_Count > 0 then
            The_Queue.Access_Count := The_Queue.Access_Count - 1;
         end if;
         if The_Queue.Access_Count = 0 then
            Tear_Down (The_Queue.all);  -- dispatches to actual Tear_Down
            Delete (The_Queue);
         else
            --  We have to clear this pointer (which is of course the
            --  user's) in case there's a cascade teardown and she
            --  calls us again.
            The_Queue := null;
         end if;
      end if;
   end Tear_Down;


   procedure Tear_Down (The_Event : access Held_Event) is
   begin
      if The_Event.The_Timer /= null then
         --  Clear the Timer's pointer to this event, so it doesn't
         --  fail the Timer_Checker test when the Timer is deleted as
         --  part of Instance teardown.
         The_Event.The_Timer.The_Entry := null;
      end if;
   end Tear_Down;


   procedure Unlocker (The_Queue : access Event_Queue_Base) is
      pragma Unreferenced (The_Queue);
   begin
      raise Program_Error;
   end Unlocker;


   procedure Wait_Until_Idle (The_Queue : access Event_Queue_Base;
                              Ignoring_Timers : Boolean := False) is
      pragma Unreferenced (The_Queue);
      pragma Unreferenced (Ignoring_Timers);
   begin
      Ada.Exceptions.Raise_Exception
        (Exceptions.Use_Error'Identity,
         "Wait_Until_Idle only legal with Test event queue");
   end Wait_Until_Idle;


end ColdFrame.Events_G;
